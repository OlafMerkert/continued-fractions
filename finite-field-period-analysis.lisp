(defpackage :finite-field-period-analysis
  (:nicknames :cf-ff-pa)
  #.gm:+gm-shadow-imports+
  (:use :cl :ol :iterate
        :generic-math
        :finite-fields
        :polynomials
        :continued-fractions-power-series)
  (:export))

(in-package :finite-field-period-analysis)

(defun elements-of-finite-field (p)
  (iter (for r from 0 below p)
        (collect (int% r p))))

(defun units-of-finite-field (p)
  (iter (for r from 1 below p)
        (collect (int% r p))))

(defun multi-dim-mapc (function seq &optional (dims 1) argument-tail)
  (if (zerop dims)
      (apply function argument-tail)
      (map nil (lambda (el)
                 (multi-dim-mapc function seq (cl:- dims 1) (cons el argument-tail)))
           seq)))

(defun monic-poly-period-length (other-coefficients)
  (let* ((poly (apply #'make-polynomial
                      (-> (first other-coefficients) 1)
                      other-coefficients))
         (cf (handler-case (make-instance 'sqrt-continued-fraction :radicand poly)
           (square-radicand () nil))))
    #|(format t "POLY: ~A~%" poly)|#
    (if cf
        (find-pure-quasiperiod-length cf)
        ;; period-length 0 indicates we have a square
        0)))

(defun period-length-statistics (p degree)
  (assert (evenp degree))
  (let ((statistics (make-hash-table)))
    (multi-dim-mapc
     (lambda (&rest other-coefficients)
       (incf (gethash (monic-poly-period-length other-coefficients)
                      statistics 0)))
     (elements-of-finite-field p)
     degree)
    statistics))

(defun nil< (a b)
  (cond ((null b) t)
        ((null a) nil)
        (t (< a b))))

(defun print-hash-table-sorted (hash-table)
  (let (entries)
    (maphash (clambda (push (cons x!key x!val) entries)) hash-table)
    (setf entries (sort entries #'nil< :key #'car))
    (dolist (e entries)
      (format t "~A: ~A~%" (car e) (cdr e)))))

(defun period-length-statistics/print (p degree)
  (format t "P: ~A  DEG: ~A~%" p degree)
  (print-hash-table-sorted
   (period-length-statistics p degree)))
