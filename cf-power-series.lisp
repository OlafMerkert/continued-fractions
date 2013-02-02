(defpackage :continued-fractions-power-series
  (:nicknames :cf-ps)
  (:shadowing-import-from :gm :+ :- :* :/ :expt := :sqrt :summing)
  (:use :cl :ol :generic-math :power-series :iterate)
  (:export
   :continued-fraction
   :partial-quotients
   :complete-quotients
   :cq-period
   :cq-quasi-period
   :time-char0
   :continued-fraction-map
   :approx-numerators
   :approx-denominators
   :with-cf))

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
   (approx-denominators :reader approx-denominators))
  (:documentation "TODO"))

(defmethod initialize-instance :after ((continued-fraction continued-fraction) &key)
  (setup-continued-fraction continued-fraction))

(defmethod setup-continued-fraction ((cf continued-fraction))
  (with-slots (starting
               (alphan complete-quotients)
               (an partial-quotients))
      cf
    (setf alphan (make-lazy-array (:start (starting) :index-var n)
                   (continued-fraction-map (aref this (- n 1))))
          an (make-lazy-array (:index-var n)
               (series-truncate (aref alphan n)))))
  (setup-continued-fraction-approx-fractions cf))

(declaim (inline setup-continued-fraction-approx-fractions approx-helper))

(defun approx-helper (coefficients &optional (type 'polynomial))
  (make-lazy-array (:start ((zero type) (one type))
                           :index-var n)
    (+ (* (lazy-aref coefficients (- n 2))
          (aref this (- n 1)))
       (aref this (- n 2)))))

(defun setup-continued-fraction-approx-fractions (cf)
  (with-slots (approx-numerators approx-denominators) cf
    (setf approx-numerators (lazy-array-drop
                             (approx-helper (partial-quotients cf))
                             2)
          approx-denominators (lazy-array-drop
                               (approx-helper (lazy-array-drop
                                               (partial-quotients cf) 1))
                               1))))

(defmacro with-cf (continued-fraction &body body)
  `(let ((cf ,continued-fraction))
     (with-accessors ((alpha0  starting)
                      (alphan  complete-quotients)
                      (an      partial-quotients)
                      (pn      approx-numerators)
                      (qn      approx-denominators))
         cf
       ,@body)))

;; TODO move this to a more suitable place (like ol-utils)??
(defparameter *progress-stream* *standard-output*)

(declaim (inline progress-event))

(defun progress-event ()
  (princ "." *progress-stream*))


(defmethod find-pure-period-length ((continued-fraction continued-fraction)
                                    &key (length-bound 40))
  "Determine the length of a pure period of the given cf expansion, if
there is one."
  (with-cf continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (= alpha0 (lazy-aref alphan i))
            (return i))
          (finally (return nil)))))

(defmethod find-pure-quasiperiod-length ((continued-fraction continued-fraction)
                                         &key (length-bound 40))
  "Determine the length of a pure quasiperiod of the given cf expansion, if
there is one."
  (with-cf continued-fraction
    (iter (for i from 1 to length-bound)
          (for alphai next (lazy-aref alphan i))
          (for gamma next (/ (leading-coefficient alphai)
                             (leading-coefficient alpha0)))
          (progress-event)
          (when (= (* (make-constant-series gamma) alpha0) alphai)
            (return (values i gamma)))
          (finally (return nil)))))
