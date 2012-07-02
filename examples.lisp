(in-package :cf-ps)

(defun example-9 ()
  (let* ((d (power-series::make-power-series/polynomial
             1 -8 -42 424 -119))
         (cq (complete-quotients d)))
    (cq-period cq)))

(defun example-9/q ()
  (let* ((d (power-series::make-power-series/polynomial
             1 -8 -42 424 -119))
         (cq (complete-quotients d)))
    (cq-quasi-period cq)))

(defun example-char0 (bound)
  (let* ((d (power-series::make-power-series/polynomial
             1 0 0 0 0 1 1))
         (cq (complete-quotients d)))
    (cq-period cq bound)))

(defun example-charp (p)
   (let* ((d (finite-fields:with-modulus (p)
               (power-series::make-power-series/polynomial
              1 0 0 0 0 1 1)))
         (cq (complete-quotients d)))
    (cq-period cq 500)))

(defun example-charp/q (p)
   (let* ((d (finite-fields:with-modulus (p)
               (power-series::make-power-series/polynomial
              1 0 0 0 0 1 1)))
         (cq (complete-quotients d)))
    (cq-quasi-period cq 500)))

