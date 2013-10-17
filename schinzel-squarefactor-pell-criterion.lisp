(defpackage :schinzel-squarefactor-pell-criterion
  (:nicknames :schinzel-criterion)
  (:use :cl :ol :iterate :polynomials)
  (:import-from :number-theory/multiplicative-functions #:euler-phi)
  (:export))

(in-package :schinzel-squarefactor-pell-criterion)

;;; todo computing characteristic polynomials

;; (declaim (optimize (debug 3)))

(defun cyclotomic-polynomial (n)
  "Compute cyclotomic polynomial with roots of order `n' in
  characteristic 0."
  (let* ((divisors (butlast (number-theory/factorisation:divisors n)))
         (order-n-equation (make-instance 'polynomial :coefficients
                                          (coerce (append (list 1)
                                                          (n-copies (- n 1) 0)
                                                          (list -1))
                                                  'vector))))
    (iter (for k in divisors)
          (for poly initially order-n-equation
               then (gm:/ poly (cyclotomic-polynomial k)))
          (finally (return poly)))))


(defun power-p (poly1 poly2 &optional (found-factors 0))
  "Test whether `poly1' is a power of `poly2'. A second value
indicates how many times `poly2' appears as a factor of `poly1'."
  (cond ((gm:zero-p poly1)
         (error "0 is only a power of 0."))
        ((gm:= poly1 1)
         (values t found-factors))
        (t (mvbind (q r) (gm:div poly1 poly2)
             (if (gm:zero-p r)
                 (power-p q poly2 (+ 1 found-factors))
                 (values nil found-factors))))))


(defun cyclotomic-power-p (polynomial)
  "Test whether a `polynomial' is a power of a cyclotomic polynomial.
  On success, the second value indicates the order of the cyclotomic
  polynomial (or rather, its roots) and the third value the exponent."
  (let ((d (degree polynomial)))
    (iter (for n from 1)
          (for phi-n next (euler-phi n))
          (while (<= phi-n d))
          (when (nt:divides-p phi-n d)
            (mvbind (power mult) (power-p polynomial (cyclotomic-polynomial n))
              (cond (power (return (values t n mult)))
                    ((< 0 mult) (return nil)))))
          (finally (return nil)))))
