(defpackage :cf-valuations
  (:use :cl :ol :iterate
        :continued-fractions-power-series)
  (:export))

(in-package :cf-valuations)

;;; some tools for studying poles and zeroes in the cf expansion

(defclass symbolic-continued-fraction (continued-fraction)
  ())


(defmethod setup-continued-fraction ((symbolic-continued-fraction symbolic-continued-fraction))
  "no setup for this one at all"
  nil)

(defmethod vc:valuate-exp (valuation (continued-fraction continued-fraction))
  (let ((scf (make-instance 'symbolic-continued-fraction))
        (v (vc:val valuation)))
    (with-slots (starting
                 complete-quotients
                 partial-quotients
                 approx-numerators
                 approx-denominators) scf
      (with-cf continued-fraction
        (setf starting            (vc:valuate-exp valuation alpha0)
              complete-quotients  (lazy-array-map v alphan )
              partial-quotients   (lazy-array-map v an)
              approx-numerators   (lazy-array-map v pn)
              approx-denominators (lazy-array-map v qn))))
    scf))

(defclass symbolic-sqrt-continued-fraction (symbolic-continued-fraction
                                            sqrt-continued-fraction)
  ())

(defmethod vc:valuate-exp (valuation (sqrt-continued-fraction sqrt-continued-fraction))
  (let ((scf (make-instance 'symbolic-sqrt-continued-fraction))
        (v (vc:val valuation)))
    (with-slots (starting
                 complete-quotients
                 partial-quotients
                 approx-numerators
                 approx-denominators
                 radicand
                 (rrn rn)
                 (ssn sn)) scf
      (with-cf2 sqrt-continued-fraction
        (setf starting            (vc:valuate-exp valuation alpha0)
              complete-quotients  (lazy-array-map v alphan )
              partial-quotients   (lazy-array-map v an)
              approx-numerators   (lazy-array-map v pn)
              approx-denominators (lazy-array-map v qn)
              radicand (vc:valuate-exp valuation d)
              rrn (lazy-array-map v rn)
              ssn  (lazy-array-map v sn))))
    scf))
