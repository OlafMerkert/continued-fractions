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

(defun time-char0 ()
  (dolist (i '(10 20 30 40 50 60 70 80 90))
    (format t "~A~%~%" i)
    (time (example-char0 i))
    (force-output t)))
