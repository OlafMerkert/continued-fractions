(defpackage :continued-fractions-power-series
  (:nicknames :cf-ps)
  (:shadowing-import-from :gm :+ :- :* :/ :expt := :sqrt)
  (:use :cl :ol :generic-math :power-series)
  (:export
   :cf
   :an
   :complete-quotients
   :cq-period
   :cq-quasi-period
   :time-char0))

(in-package :continued-fractions-power-series)

(defun cf (series)
  "The continued fraction map always drops poly part and then inverts."
  (/ (simplify (series-remainder series))))

(defun complete-quotients (d)
  "Give the complete quotients of the cf devel of a + d^1/2."
  (let* ((rd (sqrt d))
         (a  (series-truncate rd)))
    (make-lazy-array (:start ((+ rd a)) :index-var n)
      (cf (aref this (- n 1))))))

(defun cq-period (cq &optional (period-bound 40))
  "Determine the period length from the given array of complete
quotients."
  (let ((alpha0 (lazy-aref cq 0)))
    (loop
       for i from 1 to period-bound
       do (princ ".")
       when (= alpha0
               (lazy-aref cq i))
       do (return i)
       finally (return nil))))

(defun cq-quasi-period (cq &optional (period-bound 40))
  "Determine the quasi period length and factor from the given array
of complete quotients."
  (let* ((alpha0 (lazy-aref cq 0))
         (alpha0-lk (nth-coefficient% alpha0 0)))
    (loop
       for i from 1 to period-bound
       for cqi = (lazy-aref cq i)
       for ratio = (/ (nth-coefficient% cqi 0) alpha0-lk)
       do (princ ".")
       when (= (* (make-constant-series ratio) alpha0) cqi)
       do (return (values i ratio))
       finally (return nil))))

(defun an (cf)
  "Compute the partial quotients from the complete quotients."
  (make-lazy-array ()
    (series-truncate (lazy-aref cf index))))
;; Careful, alpha_0 is not (sqrt d) here!!
