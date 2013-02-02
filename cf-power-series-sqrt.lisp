(defpackage :continued-fractions-power-series-sqrt
  (:nicknames :cf-pss)
  (:shadowing-import-from :gm :+ :- :* :/ :expt := :sqrt)
  (:use :cl :ol :generic-math :polynomials :power-series)
  (:export
   :make-square-root-continued-fraction
   :d
   :root-of-d
   :complete-quotients
   :partial-quotients
   :rn
   :sn
   :square-root-continued-fraction-quasi-period-length
   :square-root-continued-fraction-period-length
   :approx-numerator
   :approx-denominator
   :test-pell))

(in-package :continued-fractions-power-series-sqrt)

(defstruct (square-root-continued-fraction (:conc-name)
                 (:constructor make-square-root-continued-fraction%))
  d
  root-of-d
  complete-quotients
  partial-quotients
  rn
  sn)

(defun make-square-root-continued-fraction (d)
  (let ((square-root-continued-fraction (make-square-root-continued-fraction% :d d :root-of-d (sqrt d))))
    (setf (complete-quotients square-root-continued-fraction) (make-lazy-array (:start ((root-of-d square-root-continued-fraction)))
                          (with-lazy-arefs (square-root-continued-fraction rn sn)
                            (/ (+ (rn index) (root-of-d square-root-continued-fraction))
                               (sn index))))
          (partial-quotients square-root-continued-fraction) (make-lazy-array ()
                      (with-lazy-arefs (square-root-continued-fraction complete-quotients)
                        (series-truncate (complete-quotients index))))
          (rn square-root-continued-fraction) (make-lazy-array (:start ((zero 'polynomial)))
                      (with-lazy-arefs (square-root-continued-fraction partial-quotients sn)
                        (- (* (sn (- index 1))
                              (partial-quotients (- index 1)))
                           (aref this (- index 1)))))
          (sn square-root-continued-fraction) (make-lazy-array (:start ((one 'polynomial)))
                      (with-lazy-arefs (square-root-continued-fraction rn)
                        (/ (- d (expt (rn index) 2))
                           (aref this (- index 1))))))
    square-root-continued-fraction))

(defun square-root-continued-fraction-quasi-period-length (continued-fraction &optional (period-length-bound 40))
  "Search for a pure quasi-period-length of the given continued-fraction exppartial-quotients of a
square root."
  (with-lazy-arefs (continued-fraction sn)
    (loop
       for i from 1 to period-length-bound
       do (princ ".")
       when (zerop (degree (sn i)))
       do (return i)
       finally (return nil))))

(defun square-root-continued-fraction-period-length (continued-fraction &optional (period-length-bound 40))
  "Search for a pure period-length of the given continued-fraction exppartial-quotients of a
square root."
  (with-lazy-arefs (continued-fraction sn)
    (loop
       for i from 1 to period-length-bound
       do (princ ".")
       when (one-p (sn i))
       do (return i)
       finally (return nil))))

(defun approx-numerator (partial-quotients)
  "Given a lazy-array of the partial quotients partial-quotients, produce a
lazy-array of the approximation numerators approx-numerator."
  (lazy-array-drop
   (make-lazy-array (:start ((zero 'polynomial) ; p_-2 at index 0
                             (one  'polynomial)) ; p_-1 at index 1
                            :index-var n)
     ;; p_0 is supposed to be a_0 at index 2
     (+ (* (lazy-aref partial-quotients (- n 2))
           (aref this (- n 1)))
        (aref this (- n 2))))
   2))

(defun approx-denominator (partial-quotients)
  "Given a lazy-array of the partial quotients partial-quotients, produce a
lazy-array of the approximation denominators approx-denominator."
  (lazy-array-drop
   (make-lazy-array (:start ((zero 'polynomial) ; q_-1 at index 0
                             (one  'polynomial)) ; q_0 at index 1
                            :index-var n)
     ;; q_1 is supposed to be a_1 at index 2
     (+ (* (lazy-aref partial-quotients (- n 1))
           (aref this (- n 1)))
        (aref this (- n 2))))
   1))

(defun test-pell (p q d)
  (- (expt p 2) (* d (expt q 2))))

;;; partial-quotients alternative approach, which does not collect all the data, but
;;; just moves on in the continued-fraction exppartial-quotients

(defstruct (square-root-continued-fraction)
  index
  d
  root-of-d
  a
  r
  s)

(defun init-square-root-continued-fraction (d)
  (let ((rd (sqrt d)))
    (make-square-root-continued-fraction :index 0
                :d d
                :rd rd
                :a (series-truncate rd)
                :r (zero 'polynomial)
                :s (one 'polynomial))))

(defun step-square-root-continued-fraction (x)
  (let* ((r (- (* (square-root-continued-fraction-s x)
                  (square-root-continued-fraction-a x))
               (square-root-continued-fraction-r x)))
         (s (/ (- (square-root-continued-fraction-d x) (expt r 2))
               (square-root-continued-fraction-s x))))
    (make-square-root-continued-fraction :index (1+ (square-root-continued-fraction-index x))
                :d  (square-root-continued-fraction-d  x)
                :rd (square-root-continued-fraction-rd x)
                :a (series-truncate (/ (+ r (square-root-continued-fraction-rd x))
                                       s))
                :r r
                :s s)))

(defun square-root-continued-fraction-quasi-period-length (d degree-bound)
  (loop
     for x = (step-square-root-continued-fraction (init-square-root-continued-fraction d)) then (step-square-root-continued-fraction x)
     summing (degree (square-root-continued-fraction-a x)) into deg-p
     do (princ ".")
     when (zerop (degree (square-root-continued-fraction-s x)))
     do (return (values t (square-root-continued-fraction-index x) deg-p))
     when (> deg-p degree-bound)
     do (return (values nil (square-root-continued-fraction-index x) deg-p))))
