;; The default rules config with severity overrides: payments touching a country under
;; FATF countermeasures (by counterparty or IP country) are blocked instead of held, and
;; structuring or smurfing touching RU or BY blocks too.
;; Missing fields take their defaults; replace the lists with your own.
((overrides
  (((countries (KP IR MM)) (severity Block))
   ((typologies (structuring smurfing)) (countries (RU BY)) (severity Block)))))
