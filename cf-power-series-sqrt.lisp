(defpackage :continued-fractions-power-series-sqrt
  (:nicknames :cf-pss)
  (:shadowing-import-from :gm :+ :- :* :/ :expt := :sqrt)
  (:use :cl :ol :generic-math :polynomials :power-series)
  (:export
   :make-srcf
   :d
   :rd
   :alphan
   :an
   :rn
   :sn
   :srcf-quasi-period
   :srcf-period
   :pn
   :qn
   :test-pell))

(in-package :continued-fractions-power-series-sqrt)

(defstruct (srcf (:conc-name)
                 (:constructor make-srcf%))
  d
  rd
  alphan
  an
  rn
  sn)

(defun make-srcf (d)
  (let ((srcf (make-srcf% :d d :rd (sqrt d))))
    (setf (alphan srcf) (make-lazy-array (:start ((rd srcf)))
                          (with-lazy-arefs (srcf rn sn)
                            (/ (+ (rn index) (rd srcf))
                               (sn index))))
          (an srcf) (make-lazy-array ()
                      (with-lazy-arefs (srcf alphan)
                        (series-truncate (alphan index))))
          (rn srcf) (make-lazy-array (:start ((zero 'polynomial)))
                      (with-lazy-arefs (srcf an sn)
                        (- (* (sn (- index 1))
                              (an (- index 1)))
                           (aref this (- index 1)))))
          (sn srcf) (make-lazy-array (:start ((one 'polynomial)))
                      (with-lazy-arefs (srcf rn)
                        (/ (- d (expt (rn index) 2))
                           (aref this (- index 1))))))
    srcf))

(defun srcf-quasi-period (cf &optional (period-bound 40))
  "Search for a pure quasi-period of the given cf expansion of a
square root."
  (with-lazy-arefs (cf sn)
    (loop
       for i from 1 to period-bound
       do (princ ".")
       when (zerop (degree (sn i)))
       do (return i)
       finally (return nil))))

(defun srcf-period (cf &optional (period-bound 40))
  "Search for a pure period of the given cf expansion of a
square root."
  (with-lazy-arefs (cf sn)
    (loop
       for i from 1 to period-bound
       do (princ ".")
       when (one-p (sn i))
       do (return i)
       finally (return nil))))

(defun pn (an)
  "Given a lazy-array of the partial quotients an, produce a
lazy-array of the approximation numerators pn."
  (lazy-array-drop
   (make-lazy-array (:start ((zero 'polynomial) ; p_-2 at index 0
                             (one  'polynomial)) ; p_-1 at index 1
                            :index-var n)
     ;; p_0 is supposed to be a_0 at index 2
     (+ (* (lazy-aref an (- n 2))
           (aref this (- n 1)))
        (aref this (- n 2))))
   2))

(defun qn (an)
  "Given a lazy-array of the partial quotients an, produce a
lazy-array of the approximation denominators qn."
  (lazy-array-drop
   (make-lazy-array (:start ((zero 'polynomial) ; q_-1 at index 0
                             (one  'polynomial)) ; q_0 at index 1
                            :index-var n)
     ;; q_1 is supposed to be a_1 at index 2
     (+ (* (lazy-aref an (- n 1))
           (aref this (- n 1)))
        (aref this (- n 2))))
   1))

(defun test-pell (p q d)
  (- (expt p 2) (* d (expt q 2))))

;;; an alternative approach, which does not collect all the data, but
;;; just moves on in the continued-fraction expansion

(defstruct (srcf0)
  index
  d
  rd
  a
  r
  s)

(defun init-srcf0 (d)
  (let ((rd (sqrt d)))
    (make-srcf0 :index 0
                :d d
                :rd rd
                :a (series-truncate rd)
                :r (zero 'polynomial)
                :s (one 'polynomial))))

(defun step-srcf0 (x)
  (let* ((r (- (* (srcf0-s x)
                  (srcf0-a x))
               (srcf0-r x)))
         (s (/ (- (srcf0-d x) (expt r 2))
               (srcf0-s x))))
    (make-srcf0 :index (1+ (srcf0-index x))
                :d  (srcf0-d  x)
                :rd (srcf0-rd x)
                :a (series-truncate (/ (+ r (srcf0-rd x))
                                       s))
                :r r
                :s s)))

(defun srcf0-quasi-period (d degree-bound)
  (loop
     for x = (step-srcf0 (init-srcf0 d)) then (step-srcf0 x)
     summing (degree (srcf0-a x)) into deg-p
     do (princ ".")
     when (zerop (degree (srcf0-s x)))
     do (return (values t (srcf0-index x) deg-p))
     when (> deg-p degree-bound)
     do (return (values nil (srcf0-index x) deg-p))))
