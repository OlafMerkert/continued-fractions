(defpackage :continued-fractions-power-series-sqrt
  (:nicknames :cf-pss)
  (:shadowing-import-from :gm :+ :- :* :/ :expt := :sqrt)
  (:use :cl :ol :generic-math :polynomials :power-series)
  (:export))

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
    (setf (alphan srcf) (with-lazy-arefs (srcf rn sn)
                          (make-lazy-array (:start ((rd srcf)))
                            (/ (+ (rn index) (rd srcf))
                               (sn index))))
          (an srcf)     (with-lazy-arefs (srcf alphan)
                          (make-lazy-array ()
                            (series-truncate (alphan index))))
          (rn srcf)     (with-lazy-arefs (srcf an sn)
                          (make-lazy-array (:start ((zero 'polynomial)))
                            (- (* (sn (- index 1))
                                  (an (- index 1)))
                               (aref this (- index 1)))))
          (sn srcf)     (with-lazy-arefs (srcf rn)
                          (make-lazy-array (:start ((one 'polynomial)))
                            (/ (- d (expt (rn index) 2))
                               (aref this (- index 1))))))
    srcf))

(defun srcf-quasi-period (cf &optional (period-bound 40))
  (with-lazy-arefs (cf sn)
    (loop
       for i from 1 to period-bound
       do (princ ".")
       when (zerop (degree (sn i)))
       do (return i)
       finally (return nil))))

(defun srcf-period (cf &optional (period-bound 40))
  (with-lazy-arefs (cf sn)
    (loop
       for i from 1 to period-bound
       do (princ ".")
       when (one-p (sn i))
       do (return i)
       finally (return nil))))