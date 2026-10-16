;; The model acts, but abstains on scores too close to the hold threshold to trust: a
;; band of 0.5 in log-odds either side of it (pick the width from the accuracy-coverage
;; table `replay` prints). Abstentions are held with reason "model uncertain"; use
;; (route Rules_only) to let the rules decide them instead.
((name active-abstain-2026-10) (model_mode Active)
 (abstain ((min_confidence 0.5) (confidence Logit_margin) (route Hold))))
