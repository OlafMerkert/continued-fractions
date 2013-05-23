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
   (sn :reader sn)
   (tn :reader tn))
  (:documentation "Given (/ (+ a (* b (sqrt d))) c), we can easily
  show that all complete quotients will have the same shape, here
  written as (/ (+ rn (* tn (sqrt d))) sn)."))

(defmacro with-cf2* (continued-fraction &body body)
  `(with-cf ,continued-fraction
     (unless (typep cf 'quadratic-continued-fraction)
       (error "Expected QUADRATIC-CONTINUED-FRACTION, but probably only got
       a CONTINUED-FRACTION."))
     (with-accessors ((d radicand)
                      (a a) (b b) (c c)
                      (rn rn)
                      (sn sn)
                      (tn tn))
         cf
       ,@body)))

(defmethod setup-continued-fraction ((cf quadratic-continued-fraction))
  (with-slots ((d radicand)
               a b c
               starting
               (alphan complete-quotients)
               (an partial-quotients)
               rn sn tn)
      cf
    (let ((sqrt-d (sqrt d))
          (not-div-cond-0 (not (zero-p (/ (- (expt a 2) (* d (expt b 2))) c)))))
      ;; first setup partial and complete quotients
      (setf an (make-lazy-array (:index-var n)
                 (series-truncate (lazy-aref alphan n)))
            alphan (make-lazy-array (:index-var n)
                     (/ (+ (lazy-aref rn n)
                           (* (lazy-aref tn n) sqrt-d))
                        (lazy-aref sn n)))
            ;; then come the main calculations
            rn (make-lazy-array (:start (a) :index-var n)
                 (lazy-arefs (rn sn an) (- n 1)
                   (if #1=(and (cl:= 1 n) not-div-cond-0)
                       ;; cannot cancel sn
                       (* sn (- rn (* sn an)))
                       ;; can cancel sn
                       (- rn (* sn an)))))
            tn (make-lazy-array (:start (b) :index-var n)
                 (lazy-arefs (sn tn) (- n 1)
                   (if #1#
                       (* -1 tn sn)
                       (* -1 tn))))
            sn (make-lazy-array (:start (c) :index-var n)
                 (lazy-arefs (rn sn tn an) (- n 1)
                   (if #1#
                       (+ (expt rn 2)
                          (* -1 d (expt b 2))
                          (* -2 sn rn an)
                          (* (expt sn 2) (expt an 2)))
                       (+ (/ (- (expt rn 2) (* d (expt tn 2))) sn)
                          (* -2 rn an)
                          (* sn (expt an 2))))))
            ;; finally, provide `starting'
            starting (lazy-aref alphan 0))))
      (setup-continued-fraction-approx-fractions cf))
;;; TODO check these formulas for mistakes

;;; TODO check whether we need to cover additional canceling issues


(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (cf sqrt-continued-fraction) &key (mod 3))
  (make-instance 'quadratic-continued-fraction
                 :radicand (-> target-type (radicand cf) :mod mod)
                 :a (-> target-type (a cf) :mod mod)
                 :b (-> target-type (b cf) :mod mod)
                 :c (-> target-type (c cf) :mod mod)))

