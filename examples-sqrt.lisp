(in-package :cf-pss)

(defun example-9 ()
  (let ((cf (make-srcf (make-polynomial
                        1 -8 -42 424 -119))))
    (list
     :period (srcf-quasi-period cf)
     :cf cf
     :d (d cf)
     :an (an cf)
     :pn (pn (an cf))
     :qn (qn (an cf)))))

(defun example-char0 (bound)
  (let ((cf (make-srcf 
             (make-polynomial 1 0 0 0 0 1 1))))
    (list
     :period (srcf-quasi-period cf bound)
     :cf cf
     :d (d cf)
     :pn (pn (an cf))
     :qn (qn (an cf)))))

(defun example-charp (p)
  (let ((cf (make-srcf (finite-fields:with-modulus (p)
                         (make-polynomial 1 0 0 0 0 1 1)))))
    (list
     :period (srcf-quasi-period cf 500)
     :cf cf
     :d (d cf)
     :pn (pn (an cf))
     :qn (qn (an cf)))))

(defun test-pell/index (result &optional index)
  (destructuring-bind (&key an pn qn period d cf) result
    (unless index (setf index (- period 1)))
   (values (test-pell (lazy-aref pn index)
               (lazy-aref qn index)
               d))))

(defmacro display (obj slot &rest nrs)
  `(destructuring-bind (&key an period cf d pn qn) ,obj
     (declare (ignorable an period cf d pn qn))
     (values ,@(mapcar #`(lazy-aref ,slot ,a1) nrs))))
