(in-package :continued-fractions-power-series)

(defclass sqrt-continued-fraction (continued-fraction)
  ((radicand :initarg :radicand
             :reader radicand)
   (rn :reader rn)
   (sn :reader sn))
  (:documentation "We know the complete quotients of (sqrt radicand)
  will all be of the form (/ (+ rn (sqrt radicand) sn). TODO"))

(defmacro with-cf2 (continued-fraction &body body)
  `(with-cf ,continued-fraction
     (unless (typep cf 'sqrt-continued-fraction)
       (error "Expected SQRT-CONTINUED-FRACTION, but probably only got
       a CONTINUED-FRACTION."))
     (with-accessors ((d radicand)
                      (rn rn)
                      (sn sn))
         cf
       ,@body)))

(defmethod setup-continued-fraction ((cf sqrt-continued-fraction))
  (with-slots ((d radicand)
               starting
               complete-quotients
               (an partial-quotients)
               rn
               sn)
      cf
    (setf starting (sqrt d))
    ;; detect when we have a square
    (when (typep starting 'polynomial)
      (error "got a square polynomial, cf expansion is trivial."))
    (let ((a0 (series-truncate starting)))
      ;; the main calculations
      (setf an (inf+seq (vector a0) (n) :cf-sqrt-an
                 (div (+ (sref rn n) a0) (sref sn n)))
            rn (inf+seq (vector 0) (n) :cf-sqrt-rn
                 (let ((n-1 (- n 1)))
                   (- (* (sref sn n-1) (sref an n-1)) (this n-1))))
            sn (inf+seq (vector 1) (n) :cf-sqrt-sn
                 (/ (- d (expt (sref rn n) 2)) (this (- n 1))))))
    ;; additional setup
    (setf complete-quotients (inf+seq (vector) (n) :cf-sqrt-alphan
                               (/ (+ (sref rn n) starting) (sref sn n))))
    (setup-continued-fraction-approx-fractions cf)))


(defmethod find-pure-period-length ((continued-fraction sqrt-continued-fraction)
                                     &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the period can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (one-p (sref sn i))
            (return i))
          (finally (return nil)))))

(defmethod find-pure-quasiperiod-length ((continued-fraction sqrt-continued-fraction)
                                         &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the quasiperiod can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (<= (degree (sref sn i)) 0)
            (return (values i (sref sn i))))
          (finally (return nil)))))

;;; for deg 4, we can just check a point on an elliptic curve
(defmethod check-torsion-divisor ((polynomial polynomial))
  (unless (cl:= (degree polynomial) 4)
    (error "CHECK-TORSION-DIVISOR only works for polynomials of deg 4."))
  (labels ((a (n) (/ (nth-coefficient% polynomial n)
                     (combi:binomial 4 n))))
    (let ((g2 (+ (* 3 (expt (a 2) 2))
                 (* -4 (a 1) (a 3))
                 (* (a 0) (a 4))))
          (g3 (+ (* -1 (expt (a 2) 3))
                 (* 2 (a 1) (a 2) (a 3))
                 (* (a 0) (a 2) (a 4))
                 (* -1 (a 0) (a 3) (a 3))
                 (* -1 (a 1) (a 1) (a 4))))
          (s2 (- (expt (a 1) 2)
                 (* (a 0) (a 2))))
          (s3 (+ (* 2 (expt (a 1) 3))
                 (* -3 (a 0) (a 1) (a 2))
                 (* (a 0) (a 0) (a 3)))))
      (let* ((ecurve (make-instance 'ec-ws:elliptic-curve-weierstrass
                                    :a (/ g2 -4) :b (/ g3 -4)))
             (point (make-instance 'ec-ws:ec-point-ws
                                   :curve ecurve
                                   :x (/ s2 (a 0)) :y (/ s3 (* 2 (a 0) (gm:sqrt (a 0)))))))
        (unless (ec-ws:ec-rational-p point)
          (error "Curve and point are not defined over the rationals, cannot apply test."))
        (when (gm:zero-p (ec-ws:discriminant ecurve))
          (error "This curve is singular (i.e. the given polynomial was not squarefree)."))
        (values (ec-ws:ec-rational-torsion-p point)
                point
                ecurve)))))

(defmethod check-torsion-divisor ((continued-fraction sqrt-continued-fraction))
  (check-torsion-divisor (radicand continued-fraction)))

;;; reduction means reducing the radicand
(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (cf sqrt-continued-fraction) &key (mod 3))
  (make-instance 'sqrt-continued-fraction
                 :radicand (-> 'finite-fields:integer-mod (radicand cf) :mod mod)))
