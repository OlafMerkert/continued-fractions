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

(defun example-char0/period (bound)
  (let ((cf (make-srcf 
             (make-polynomial 1 0 0 0 0 1 1)))) 
     (srcf-quasi-period cf bound)))

(defun example-char0 (bound)
  (let ((cf (make-srcf 
             (make-polynomial 1 0 0 0 0 1 1)))) 
     (list
      :period (srcf-quasi-period cf bound)
      :d (d cf)
      :an (an cf)
      :pn (pn (an cf))
      :qn (qn (an cf)))))

(defun example-char0/iter (bound)
  (srcf0-quasi-period (make-polynomial 1 0 0 0 0 1 1) bound))

(defun example-charp (p)
  (let ((cf (make-srcf (finite-fields:with-modulus (p)
                         (make-polynomial 1 0 0 0 0 1 1)))))
    (list
     :period (srcf-quasi-period cf 2000)
     :cf cf
     :d (d cf)
     :an (an cf)
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

(defun show-coeffs (cf)
  (with-lazy-arefs (cf alphan)
    (dotimes (i 8)
      (format t "a_-1 = ~A  a_-2 = ~A ~%"
              (nth-coefficient (alphan i) -1)
              (nth-coefficient (alphan i) -2)))))

(defun show-period-lengths (primes example)
  (princ
   (with-output-to-string (stream)
     (dolist (p primes)
       (let ((e (funcall example p)))
         (format stream "~A & ~A & ~A \\\\~%"
                 p
                 (second e)
                 (when (second e)
                   (degree (display e pn (- (second e) 1))))))))))
