(in-package :continued-fractions-power-series)

(defclass quadratic-continued-fraction (continued-fraction)
  ;; we don't inherit from sqrt-continued-fraction, because we have
  ;; more general behaviour, it would only make sense the other way
  ;; around.
  ((radicand :initarg :radicand 
             :reader radicand)
   (a :initarg :a 
      :reader a)
   (b :initarg :b 
      :reader b)
   (c :initarg :c 
      :reader c)
   (rn :reader rn)
   (sn :reader sn))
  (:documentation "Given (/ (+ a (* b (sqrt d))) c), we can easily
  show that all complete quotients will have the same shape, here
  written as (/ (+ rn (* b (sqrt d))) sn) after multiplying a,b,c with
  an appropriate divisor of c."))

(defmacro with-cf2* (continued-fraction &body body)
  `(with-cf ,continued-fraction
     (unless (typep cf 'quadratic-continued-fraction)
       (error "Expected QUADRATIC-CONTINUED-FRACTION, but probably only got
       a CONTINUED-FRACTION."))
     (with-accessors ((d radicand)
                      (a a) (b b) (c c)
                      (rn rn)
                      (sn sn))
         cf
       ,@body)))

(defmethod setup-continued-fraction ((cf quadratic-continued-fraction))
  (with-slots ((d radicand)
               a b c
               starting
               (alphan complete-quotients)
               (an partial-quotients)
               rn sn)
      cf
    ;; first normalise the coefficients, s.t. c | b^2 d - a^2
    (let ((g (/ c (ggt c (^ (ggt a b) 2)))))
      (setf a (* g a)
            b (* g b)
            c (* g c)))
    (let* ((sqrt-d (sqrt d))
           (b-sqrt-d (* b sqrt-d))
           (b2d (* (^ b 2) d)))
      ;; first setup partial and complete quotients
      (setf an (inf+seq nil (n) :cf-quad-an
                 (series-truncate (sref alphan n)))
            alphan (inf+seq nil (n) :cf-quad-alphan
                     (/ (+ (sref rn n) b-sqrt-d)
                        (sref sn n)))
            ;; then come the main calculations
            rn (inf+seq (vector a) (n) :cf-quad-rn
                 (bind-seq (rn sn an) (- n 1)
                   (- (* sn an) rn)))
            sn (inf+seq (vector c) (n) :cf-quad-sn
                 (bind-seq (rn sn an) (- n 1)
                   (/ (- b2d (^ (- (* an sn) rn) 2)) sn)))
            ;; finally, provide `starting'
            starting (sref alphan 0))))
  (setup-continued-fraction-approx-fractions cf))
;;; TODO check these formulas for mistakes

;;; TODO check whether we need to cover additional canceling issues


(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (cf sqrt-continued-fraction) &key (mod 3))
  (make-instance 'quadratic-continued-fraction
                 :radicand (-> target-type (radicand cf) :mod mod)
                 :a (-> target-type (a cf) :mod mod)
                 :b (-> target-type (b cf) :mod mod)
                 :c (-> target-type (c cf) :mod mod)))

