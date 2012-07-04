(in-package :cf-ps)

(defun example-9 ()
  (let* ((d (make-power-series nil
             1 -8 -42 424 -119))
         (cq (complete-quotients d)))
    (cq-period cq)))

(defun example-9/q ()
  (let* ((d (make-power-series nil
             1 -8 -42 424 -119))
         (cq (complete-quotients d)))
    (cq-quasi-period cq)))

(defun example-char0 (bound)
  (let* ((d (make-power-series nil
             1 0 0 0 0 1 1))
         (cq (complete-quotients d)))
    (cq-period cq bound)))

(defun example-charp (p)
   (let* ((d (finite-fields:with-modulus (p)
               (make-power-series nil
              1 0 0 0 0 1 1)))
         (cq (complete-quotients d)))
    (cq-period cq 500)))

(defun example-charp/q (p)
   (let* ((d (finite-fields:with-modulus (p)
               (make-power-series nil
              1 0 0 0 0 1 1)))
         (cq (complete-quotients d)))
    (cq-quasi-period cq 500)))

(defun time-char0 (&optional (function #'example-char0) (max-time 80))
  (dolist (i (mrange 10 max-time 10))
    (format t "~A~%~%" i)
    (time (funcall function i))
    (force-output t)))
