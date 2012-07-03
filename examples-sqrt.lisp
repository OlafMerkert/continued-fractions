(in-package :cf-pss)

(defun example-9 ()
  (let ((cf (make-srcf (make-polynomial
                        1 -8 -42 424 -119))))
    (srcf-quasi-period cf)))

(defun example-char0 (bound)
  (let ((cf (make-srcf 
             (make-polynomial 1 0 0 0 0 1 1))))
    (srcf-quasi-period cf bound)))

(defun example-charp (p)
  (let ((cf (make-srcf (finite-fields:with-modulus (p)
                         (make-polynomial 1 0 0 0 0 1 1)))))
    (srcf-quasi-period cf 500)))
