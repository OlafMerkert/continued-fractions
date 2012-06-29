(in-package :cf-ps)

(defun example-9 ()
  (let* ((d (power-series::make-power-series/polynomial
             1 -8 -42 424 -119))
         (cq (complete-quotients d)))
    (cq-period cq)))

(defun example-char0 ()
  (let* ((d (power-series::make-power-series/polynomial
             1 0 0 0 1 0 1))
         (cq (complete-quotients d)))
    (cq-period cq 50)))

(defun example-charp (p)
   (let* ((d (finite-fields:with-modulus (p)
               (power-series::make-power-series/polynomial
              1 0 0 0 1 0 1)))
         (cq (complete-quotients d)))
    (cq-period cq 50)))
