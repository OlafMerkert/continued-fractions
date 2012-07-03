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
   (make-lazy-array (:start ((one 'polynomial)
                             (lazy-aref an 0))
                            :index-var n)
     (+ (* (lazy-aref an (- n 1))
           (aref this (- n 1)))
        (aref this (- n 2))))
   1))

(defun qn (an)
  "Given a lazy-array of the partial quotients an, produce a
lazy-array of the approximation denominators qn."
  (make-lazy-array (:start ((one 'polynomial)
                            (lazy-aref an 1))
                           :index-var n)
    (+ (* (lazy-aref an n)
          (aref this (- n 1)))
       (aref this (- n 2)))))

(defun test-pell (p q d)
  (- (expt p 2) (* d (expt q 2))))
