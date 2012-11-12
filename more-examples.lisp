(in-package :cf-pss)

(defun example-1+ ()
  (let* ((d (make-polynomial 1 4 2 0 0))
         (cf (make-srcf d)))
    (srcf-quasi-period cf)))

(defun search-zero-zero (ex bound)
  (loop for i from 0 to bound
     when (zero-p (nth-coefficient (display ex pn i) 0))
     do (return i)
     finally (return nil)))


(defun example-1 ()
  (let* ((d (make-polynomial 1 4 2))
         (cf (make-srcf d)))
    (list :period (srcf-quasi-period cf 40)
          :cf cf
          :d d
          :an (an cf)
          :pn (pn (an cf))
          :qn (qn (an cf)))))

(defun example-2 ()
  (let* ((d (* (make-polynomial 1 0)
               (make-polynomial 1 4 2 0)))
         (cf (make-srcf d)))
    (srcf-quasi-period cf 200)))
