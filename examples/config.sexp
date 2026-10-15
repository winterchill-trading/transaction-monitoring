;; The default rules config. Amounts are minor units of the reporting currency.
((profile
  ((reporting_threshold 1000000) (below_threshold_bps 1500)
   (round_unit 10000) (small_card_amount 200)))
 (structuring_min_count 3) (smurfing_min_payers 4) (rapid_min_inbound 100000)
 (rapid_share_bps 8000) (mule_min_payers 5) (mule_max_payees 2)
 (mule_max_account_age 90d) (mule_collector_min_customers 5)
 (dormant_after 180d) (dormant_min_amount 500000)
 (high_risk_countries (KP IR MM)) (corridor_min_amount 100000)
 (sanctions_adjacent_countries (RU BY)) (card_testing_min_small 5)
 (card_testing_min_declines 3) (atm_max_count_24h 6) (atm_max_sum_24h 300000)
 (unusual_multiple 10) (unusual_min_amount 300000) (unusual_min_history 10)
 (disabled ()) (shadow ()))
