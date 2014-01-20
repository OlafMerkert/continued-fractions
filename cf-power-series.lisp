(defpackage :continued-fractions-power-series
  (:nicknames :cf-ps)
  #.gm:+gm-shadow-imports+
  (:import-from :fractions :ggt)
  (:use :cl :ol :generic-math
        :polynomials :power-series :iterate
        :infinite-sequence)
  (:export
   #:continued-fraction
   #:partial-quotients
   #:complete-quotients
   #:continued-fraction-map
   #:approx-numerators
   #:approx-denominators
   #:with-cf
   #:setup-continued-fraction
   #:setup-continued-fraction-approx-fractions
   #:find-pure-period-length
   #:find-pure-quasiperiod-length
   #:cf
   #:alpha0
   #:alphan
   #:an
   #:pn
   #:qn
   #:sqrt-continued-fraction
   #:starting
   #:with-cf2
   #:d
   #:rn
   #:sn
   #:check-torsion-divisor
   #:quadratic-continued-fraction
   #:with-cf2*
   #:tn
   #:square-radicand
   #:phin-infinite-order))

(in-package :continued-fractions-power-series)

(defun continued-fraction-map (series)
  "The continued fraction map always drops poly part and then inverts."
  (/ (simplify (series-remainder series))))

(defclass continued-fraction ()
  ((starting           :initarg :starting
                       :reader starting)
   (complete-quotients :reader complete-quotients)
   (partial-quotients  :reader partial-quotients)
   (approx-numerators   :reader approx-numerators)
   (approx-denominators :reader approx-denominators)))

(defmacro with-cf (continued-fraction &body body)
  `(let ((cf ,continued-fraction))
     (with-accessors ((alpha0  starting)
                      (alphan  complete-quotients)
                      (an      partial-quotients)
                      (pn      approx-numerators)
                      (qn      approx-denominators))
         cf
       ,@body)))

;; TODO handle perhaps also finite cf expansions.

(defmethod initialize-instance :after ((continued-fraction continued-fraction) &key)
  (setup-continued-fraction continued-fraction))

(defmethod setup-continued-fraction ((cf continued-fraction))
  (with-slots (starting
               (alphan complete-quotients)
               (an partial-quotients))
      cf
    (setf alphan (inf+seq (vector starting) (n) :cf-alphan
                   (continued-fraction-map (this (- n 1))))
          an (inf+seq nil (n) :cf-an
               (series-truncate (sref alphan n)))))
  (setup-continued-fraction-approx-fractions cf))

(declaim (inline setup-continued-fraction-approx-fractions approx-helper))

(defun approx-helper (coefficients &optional (start 0) name)
  (make-instance 'infinite+-sequence
                 :name name
                 :fill-strategy :sequential
                 :start start
                 :data (vector 0 1)
                 :generating-function
                 (flambda (this n)
                   (+ (* (sref coefficients n) (this (- n 1)))
                      (this (- n 2))))))

(defun setup-continued-fraction-approx-fractions (cf)
  (with-slots (approx-numerators approx-denominators) cf
    (setf approx-numerators (approx-helper (partial-quotients cf) -2 :cf-pn) 
          approx-denominators (approx-helper (partial-quotients cf) -1 :cf-qn))))

(defmethod find-pure-period-length ((continued-fraction continued-fraction)
                                    &key (length-bound 40))
  "Determine the length of a pure period of the given cf expansion, if
there is one."
  (with-cf continued-fraction
    (iter (for i from 1 to length-bound)
          #|(progress-event)|#
          (when (= alpha0 (sref alphan i))
            (return i))
          (finally (return nil)))))

(defmethod find-pure-quasiperiod-length ((continued-fraction continued-fraction)
                                         &key (length-bound 40))
  "Determine the length of a pure quasiperiod of the given cf expansion, if
there is one."
  (with-cf continued-fraction
    (iter (for i from 1 to length-bound)
          (for alphai next (sref alphan i))
          (for gamma next (/ (leading-coefficient alphai)
                             (leading-coefficient alpha0)))
          #|(progress-event)|#
          (when (= (* (make-constant-series gamma) alpha0) alphai)
            (return (values i gamma)))
          (finally (return nil)))))

;;; reduction means reducing the starting series
(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (cf continued-fraction) &key (mod 3))
  (make-instance 'continued-fraction :starting (-> 'finite-fields:integer-mod (starting cf) :mod mod)))
