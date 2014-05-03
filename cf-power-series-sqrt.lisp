(in-package :continued-fractions-power-series)

(defclass sqrt-continued-fraction (continued-fraction)
  ((radicand :initarg :radicand
             :reader radicand)
   (sqrt-radicand :reader sqrt-radicand)
   (t0 :initarg :t0
         :initform 0
         :reader t0)
   (s0 :initarg :s0
         :initform 1
         :reader s0)
   (tn :reader tn)
   (sn :reader sn))
  (:documentation "We know the complete quotients of (sqrt radicand)
  will all be of the form (/ (+ sqrt-radicand tn (sqrt radicand) sn)."))

(defmethod gm:generic-= ((cf1 sqrt-continued-fraction) (cf2 sqrt-continued-fraction))
  (and (gm:= (radicand cf1) (radicand cf2))
       (gm:= (t0 cf1) (t0 cf2))
       (gm:= (s0 cf1) (s0 cf2))))

(defmacro with-cf2 (continued-fraction &body body)
  `(with-cf ,continued-fraction
     (unless (typep cf 'sqrt-continued-fraction)
       (error "Expected SQRT-CONTINUED-FRACTION, but probably only got
       a CONTINUED-FRACTION."))
     (with-accessors ((d radicand)
                      (tn tn)
                      (sn sn)
                      (a sqrt-radicand))
         cf
       ,@body)))

(define-condition square-radicand ()
  ((sqrt :initarg :sqrt
         :initform nil)))

(declaim (inline compute-complete-quotient compute-partial-quotient))

(defun compute-complete-quotient (a tn sn sqrt-d)
  (/ (+ a tn sqrt-d) sn))

(defun compute-partial-quotient (a2 tn sn)
  (div (+ a2 tn) sn))

(defmethod setup-continued-fraction ((cf sqrt-continued-fraction))
  (with-slots ((d radicand)
               (a sqrt-radicand)
               starting
               complete-quotients
               (an partial-quotients)
               t0
               s0
               tn
               sn)
      cf
    ;; todo first step: normalise t0 and s0 s.t. s0 | d - r0^2

    ;; second step: compute (sqrt d) and polynomial part `a'
    (let ((sqrt-d (sqrt d))
          a2
          d-a^2)
      (setf a (series-truncate sqrt-d)
            starting (compute-complete-quotient a t0 s0 sqrt-d)
            a2 (* 2 a)
            d-a^2 (- d (^ a 2)))
      ;; detect when we have a square
      (when (typep sqrt-d 'polynomial)
        (error 'square-radicand :sqrt sqrt-d))
      ;; the main calculations
      (setf an (inf+seq (vector) (n) :cf-sqrt-an
                        (compute-partial-quotient a2 (sref tn n) (sref sn n)))
            tn (inf+seq (vector 0) (n) :cf-sqrt-tn
                        (let* ((n-1 (- n 1))
                               (tp (this n-1))
                               (sp (sref sn n-1)))
                          (- (if (< (degree tp) (degree sp))
                                 (divr a2 sp)
                                 (divr (+ a2 tp) sp)))))
            sn (inf+seq (vector 1) (n) :cf-sqrt-sn
                        (let ((tn (sref tn n)))
                          (div (- d-a^2 (* a2 tn) (^ tn 2)) (this (- n 1))))))
      ;; additional setup
      (setf complete-quotients (inf+seq (vector starting) (n) :cf-sqrt-alphan
                                        (compute-complete-quotient a (sref tn n) (sref sn n) sqrt-d))))
    (setup-continued-fraction-approx-fractions cf)))


#|(defmethod find-pure-period-length ((continued-fraction sqrt-continued-fraction)
                                     &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the period can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (one-p (sref sn i))
            (return i))
(finally (return nil)))))|#

(defmethod find-pure-period-length ((continued-fraction sqrt-continued-fraction)
                                    &key (length-bound 40))
  (mvbind (qp-len sn) (find-pure-quasiperiod-length continued-fraction :length-bound length-bound)
    (when qp-len
      (if (one-p sn) qp-len (cl:* 2 qp-len)))))

(defmethod find-pure-quasiperiod-length ((continued-fraction sqrt-continued-fraction)
                                         &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the quasiperiod can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          #|(progress-event)|#
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

;;; compute the order of the points at infinity for the divisors
;;; corresponding to p_n + y q_n
(defmethod phin-infinite-order ((continued-fraction sqrt-continued-fraction) n)
  (with-cf2 continued-fraction
    (let* ((yqn (gm:* alpha0 (sref qn n)))
           (phin+ (simplify (gm:+ (sref pn n) yqn)))
           (phin- (simplify (gm:- (sref pn n) yqn))))
      (list (- (degree phin+)) (- (degree phin-))))))
