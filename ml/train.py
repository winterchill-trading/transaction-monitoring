"""Train, evaluate and export the transaction-monitoring model.

Reads the feature rows written by `transaction-monitoring features` (the OCaml library
computes every feature, so training and live scoring share one implementation), trains
scikit-learn gradient-boosted trees, calibrates them on a later window, chooses the
review and block thresholds, evaluates the model, the rules and both together on a
held-out test window, and writes:

  ../models/tm-aml-gbdt/model.json    risk-models format 1 (kind "gbdt"), scored in OCaml
  ../models/tm-aml-gbdt/metrics.json  every number quoted in the README
  ../test/fixtures/parity.csv         rows with scikit-learn's margin, probability and
                                      calibrated score, for the OCaml parity test

Time-ordered split, never random, with a gap standing in for the label delay
(investigations and reports take days): the first 14 days warm the profiles and are
dropped, then train [14, 45), calibrate [45, 55), gap [55, 59), test [59, end).

Usage: uv run python train.py [--features data/features.csv]
"""

import argparse
import hashlib
import json
import os

import numpy as np
import pandas as pd
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import average_precision_score, roc_auc_score

META = ["event_id", "customer", "day", "label", "rules", "rule_hits"]
WARMUP, TRAIN_END, CAL_END, GAP_END = 14, 45, 55, 59
ALERTS_PER_DAY = 60  # review budget: about 0.5% of the 11 500 events a day
BLOCK_PRECISION = 0.98
POS_WEIGHT = 50.0
SCORE_EDGES = [0.0, 1e-5, 3e-5, 1e-4, 3e-4, 1e-3, 3e-3, 1e-2, 3e-2, 0.1, 0.3, 1.0]
NAME = "tm-aml-gbdt"


def load(path):
    # Empty cells are missing features and read as NaN; label and rule_hits stay text.
    df = pd.read_csv(path, dtype={"label": "string", "rule_hits": "string"})
    df["label"] = df["label"].fillna("")
    df["rule_hits"] = df["rule_hits"].fillna("")
    features = [c for c in df.columns if c not in META]
    # Day 0 is the start of the simulated period: a few dormant accounts have events
    # months earlier, so the first percentile is a robust start.
    start = np.floor(np.quantile(df["day"], 0.01))
    df["d"] = df["day"] - start
    df["y"] = (df["label"] != "").astype(int)
    return df, features


def digest(names):
    return hashlib.md5("\n".join(names).encode()).hexdigest()


def export_tree(pred, keep):
    nodes = pred.nodes
    n = len(nodes)
    leaf = nodes["is_leaf"].astype(bool)
    # The trees were fitted on the [keep] columns; map back to the full schema.
    feature = np.where(leaf, -1, keep[nodes["feature_idx"]]).astype(int)
    left = np.where(leaf, -1, nodes["left"].astype(np.int64))
    right = np.where(leaf, -1, nodes["right"].astype(np.int64))
    value = nodes["value"].astype(float).copy()
    count = nodes["count"].astype(float)
    # Children have larger indices than their parent: a reverse sweep fills each split's
    # expected value from its children (used for reason codes only).
    for i in range(n - 1, -1, -1):
        if not leaf[i]:
            l, r = left[i], right[i]
            value[i] = (count[l] * value[l] + count[r] * value[r]) / (count[l] + count[r])
    threshold = np.where(leaf, 0.0, nodes["num_threshold"])
    # +inf (missing versus present) is not JSON; the largest finite double routes every
    # finite value the same way.
    threshold = np.where(threshold == np.inf, np.finfo(np.float64).max, threshold)
    assert np.isfinite(threshold).all()
    assert not nodes["is_categorical"].any()
    # count (training rows per node) is the cover risk-models' path-dependent
    # attributions weigh by; the explanation attached to holds needs it.
    return dict(feature=feature.tolist(), threshold=threshold.tolist(),
                missing_left=nodes["missing_go_to_left"].astype(int).tolist(),
                left=left.tolist(), right=right.tolist(), value=value.tolist(),
                count=count.tolist())


def per_day_top_k(df, score, k):
    """Precision and recall when investigators review the k highest scores each day."""
    tmp = pd.DataFrame({"day": np.floor(df["d"]).values, "s": score, "y": df["y"].values})
    top = tmp.sort_values("s", ascending=False).groupby("day").head(k)
    return dict(precision=float(top["y"].mean()), recall=float(top["y"].sum() / max(1, tmp["y"].sum())))


def outcome(df, alerted):
    """Event and episode level results of a set of alerts; an episode is a customer's
    labelled typology, detected when any of its events alerted."""
    y = df["y"].values.astype(bool)
    days = np.ptp(np.floor(df["d"])) + 1
    tp = int((alerted & y).sum())
    n = int(alerted.sum())
    ep = df.loc[y, ["customer", "label"]].assign(a=alerted[y])
    episodes = ep.groupby(["customer", "label"])["a"].any()
    by_typology = episodes.groupby(level="label").agg(["sum", "count"])
    return dict(
        alerts=n, alerts_per_day=n / days, precision=tp / n if n else None,
        event_recall=tp / int(y.sum()), episodes=int(len(episodes)),
        episode_recall=float(episodes.mean()),
        by_typology={k: f"{int(r['sum'])}/{int(r['count'])}" for k, r in by_typology.iterrows()})


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("--features", default="data/features.csv")
    p.add_argument("--models", default="../models")
    p.add_argument("--fixtures", default="../test/fixtures")
    a = p.parse_args()

    df, features = load(a.features)
    d = df["d"]
    tr = (d >= WARMUP) & (d < TRAIN_END)
    ca = (d >= TRAIN_END) & (d < CAL_END)
    te = d >= GAP_END
    X = df[features].to_numpy(dtype=np.float64)
    y = df["y"].to_numpy()
    print(f"rows: train {tr.sum()} ({y[tr].sum()} positive), calibrate {ca.sum()} "
          f"({y[ca].sum()}), test {te.sum()} ({y[te].sum()})")

    # Columns this data never fills (balance, verification of payee, SCA failures: the
    # risk-rules record has them, monitoring events do not) or fills with one value
    # cannot be split on; scikit-learn's binning also fails on them. Fit on the others
    # and map tree features back to the full schema on export.
    keep = np.array([j for j in range(X.shape[1])
                     if pd.Series(X[tr][:, j]).nunique(dropna=True) >= 2])
    print(f"fitting on {len(keep)} of {X.shape[1]} features; unused: "
          + ", ".join(features[j] for j in range(X.shape[1]) if j not in set(keep)))
    # Positives are about 0.1% of rows; weighting them keeps the trees from ignoring
    # them, and calibration on a later window undoes the weighting's bias.
    w = np.where(y[tr] == 1, POS_WEIGHT, 1.0)
    g = HistGradientBoostingClassifier(
        max_iter=150, learning_rate=0.08, max_leaf_nodes=24, min_samples_leaf=40,
        l2_regularization=1.0, random_state=7)
    g.fit(X[tr][:, keep], y[tr], sample_weight=w)
    p_ca = g.predict_proba(X[ca][:, keep])[:, 1]
    # The positive weight multiplies the odds the trees learn by POS_WEIGHT; subtracting
    # log(POS_WEIGHT) from the base score undoes that exactly, so sigmoid(margin) is a
    # probability under the training prior and calibration is the identity. (Isotonic
    # regression on a few hundred positives gives a handful of steps, and a threshold on
    # a step function cannot meet an alert budget.)
    shift = float(np.log(POS_WEIGHT))

    def calibrate(prob):
        odds = prob / np.maximum(1.0 - prob, 1e-300)
        return 1.0 / (1.0 + 1.0 / (odds / POS_WEIGHT))

    s_ca = calibrate(p_ca)
    s_te = calibrate(g.predict_proba(X[te][:, keep])[:, 1])

    # Thresholds on the calibration window.
    cal_days = CAL_END - TRAIN_END
    order = np.sort(np.unique(s_ca))[::-1]
    review = float(order[0])
    for t in order:
        if (s_ca >= t).sum() / cal_days <= ALERTS_PER_DAY:
            review = float(t)
        else:
            break
    block = None
    for t in order[::-1][order[::-1] > review]:
        m = s_ca >= t
        if m.sum() >= 5 and y[ca][m].mean() >= BLOCK_PRECISION:
            block = float(t)
            break

    test = df[te].reset_index(drop=True)
    rules_alert = (test["rules"] != "allow").to_numpy()
    model_alert = s_te >= review
    metrics = dict(
        name=NAME,
        rows=dict(train=int(tr.sum()), calibrate=int(ca.sum()), test=int(te.sum()),
                  test_positive=int(y[te].sum())),
        roc_auc=float(roc_auc_score(y[te], s_te)),
        average_precision=float(average_precision_score(y[te], s_te)),
        positive_rate=float(y[te].mean()),
        top_k_per_day={str(k): per_day_top_k(test, s_te, k) for k in (10, 25, 50)},
        thresholds=dict(review=review, block=block, alerts_per_day=ALERTS_PER_DAY,
                        block_precision=BLOCK_PRECISION),
        rules=outcome(test, rules_alert),
        model=outcome(test, model_alert),
        rules_or_model=outcome(test, rules_alert | model_alert),
    )
    if block is not None:
        metrics["model_block"] = outcome(test, s_te >= block)
    names = list(features)
    model = dict(
        format=1, name=NAME, link="logistic",
        schema=dict(name="transaction-monitoring-features", version=1, features=names,
                    digest=digest(names)),
        kind="gbdt",
        gbdt=dict(base_score=float(g._baseline_prediction.ravel()[0]) - shift,
                  trees=[export_tree(pr, keep) for (pr,) in g._predictors]),
        calibration=dict(kind="identity"),
        reference=dict(edges=SCORE_EDGES, counts=np.histogram(s_ca, SCORE_EDGES)[0].tolist()),
        thresholds=dict(review=review, block=block, alerts_per_day=ALERTS_PER_DAY,
                        block_precision=BLOCK_PRECISION),
        metrics=dict(roc_auc=metrics["roc_auc"], average_precision=metrics["average_precision"]),
    )
    out = os.path.join(a.models, NAME)
    os.makedirs(out, exist_ok=True)
    with open(os.path.join(out, "model.json"), "w") as f:
        json.dump(model, f, allow_nan=False, separators=(",", ":"))
        f.write("\n")
    with open(os.path.join(out, "metrics.json"), "w") as f:
        json.dump(metrics, f, indent=2)
        f.write("\n")

    # Parity rows: positives and negatives from the test window, some extra missing
    # values, and some values exactly on a split threshold.
    rng = np.random.default_rng(11)
    idx = np.flatnonzero(te.to_numpy())
    pos, neg = idx[y[idx] == 1], idx[y[idx] == 0]
    chosen = np.sort(np.concatenate([rng.choice(pos, min(100, len(pos)), replace=False),
                                     rng.choice(neg, 300, replace=False)]))
    Xp = X[chosen].copy()
    rows = rng.choice(len(Xp), 60, replace=False)
    for r in rows[:30]:
        Xp[r, rng.integers(0, Xp.shape[1])] = np.nan
    trees = model["gbdt"]["trees"]
    for r in rows[30:]:
        tree = trees[rng.integers(0, len(trees))]
        splits = [i for i, f in enumerate(tree["feature"]) if f >= 0 and tree["threshold"][i] < 1e300]
        i = splits[rng.integers(0, len(splits))]
        Xp[r, tree["feature"][i]] = tree["threshold"][i]
    margin = g.decision_function(Xp[:, keep]) - shift
    prob = 1.0 / (1.0 + np.exp(-margin))
    score = prob
    os.makedirs(a.fixtures, exist_ok=True)
    pd.DataFrame(Xp, columns=names).assign(margin=margin, probability=prob, score=score).to_csv(
        os.path.join(a.fixtures, "parity.csv"), index=False, float_format="%.17g")

    print(json.dumps({k: metrics[k] for k in ("roc_auc", "average_precision", "thresholds",
                                              "top_k_per_day", "rules", "model",
                                              "rules_or_model")}, indent=1))
    print(f"model.json: {os.path.getsize(os.path.join(out, 'model.json')) // 1024} KB, "
          f"{len(trees)} trees")


if __name__ == "__main__":
    main()
