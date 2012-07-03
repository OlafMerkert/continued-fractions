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