(defsystem continued-fractions
  :serial t
  :depends-on (ol-utils math-utils)
  :components ((:file "cf-power-series")
               (:file "cf-power-series-sqrt")
               (:file "cf-power-series-quadratic")
               (:file "cf-alternative-power-series")
               (:file "cf-rationals")
               (:file "cf-valuations")
               (:file "examples")
               ;; (:file "examples-sqrt")
               ))
