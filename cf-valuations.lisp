(defpackage :cf-valuations
  (:use :cl :ol :iterate
        :continued-fractions-power-series
        :infinite-sequence)
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
    (with-slots (cf-ps::starting
                 cf-ps::complete-quotients
                 cf-ps::partial-quotients
                 cf-ps::approx-numerators
                 cf-ps::approx-denominators) scf
      (with-cf continued-fraction
        (setf cf-ps::starting            (vc:valuate-exp valuation alpha0)
              cf-ps::complete-quotients  (map-sequence v alphan)
              cf-ps::partial-quotients   (map-sequence v an)
              cf-ps::approx-numerators   (map-sequence v pn)
              cf-ps::approx-denominators (map-sequence v qn))))
    scf))

(defclass symbolic-sqrt-continued-fraction (symbolic-continued-fraction
                                            sqrt-continued-fraction)
  ())

(defmethod vc:valuate-exp (valuation (sqrt-continued-fraction sqrt-continued-fraction))
  (let ((scf (make-instance 'symbolic-sqrt-continued-fraction))
        (v (vc:val valuation)))
    (with-slots (cf-ps::starting
                 cf-ps::complete-quotients
                 cf-ps::partial-quotients
                 cf-ps::approx-numerators
                 cf-ps::approx-denominators
                 cf-ps::radicand
                 (rrn cf-ps::rn)
                 (ssn cf-ps::sn)) scf
      (with-cf2 sqrt-continued-fraction
        (setf cf-ps::starting            (vc:valuate-exp valuation alpha0)
              cf-ps::complete-quotients  (map-sequence v alphan )
              cf-ps::partial-quotients   (map-sequence v an)
              cf-ps::approx-numerators   (map-sequence v pn)
              cf-ps::approx-denominators (map-sequence v qn)
              cf-ps::radicand            (vc:valuate-exp valuation d)
              rrn                 (map-sequence v rn)
              ssn                 (map-sequence v sn))))
    scf))

(defclass symbolic-quadratic-continued-fraction (symbolic-continued-fraction
                                                 quadratic-continued-fraction)
  ())

(defmethod vc:valuate-exp (valuation (quadratic-continued-fraction quadratic-continued-fraction))
  (let ((scf (make-instance 'symbolic-quadratic-continued-fraction))
        (v (vc:val valuation)))
    (with-slots (cf-ps::starting
                 cf-ps::complete-quotients
                 cf-ps::partial-quotients
                 cf-ps::approx-numerators
                 cf-ps::approx-denominators
                 cf-ps::radicand
                 (aa cf-ps::a)
                 (bb cf-ps::b)
                 (cc cf-ps::c)
                 (rrn cf-ps::rn)
                 (ssn cf-ps::sn)
                 (ttn cf-ps::tn)) scf
      (with-cf2* quadratic-continued-fraction
        (setf cf-ps::starting            (vc:valuate-exp valuation alpha0)
              cf-ps::complete-quotients  (map-sequence v alphan )
              cf-ps::partial-quotients   (map-sequence v an)
              cf-ps::approx-numerators   (map-sequence v pn)
              cf-ps::approx-denominators (map-sequence v qn)
              cf-ps::radicand            (vc:valuate-exp valuation d)
              aa                         (vc:valuate-exp valuation cf-ps::a)
              bb                         (vc:valuate-exp valuation cf-ps::b)
              cc                         (vc:valuate-exp valuation cf-ps::c)
              rrn                        (map-sequence v rn)
              ssn                        (map-sequence v sn)
              ttn                        (map-sequence v tn))))))
