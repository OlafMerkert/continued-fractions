(in-package :cf-ps)



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

(defun test-for-primes (primes function)
  (with-output-to-string (stream)
    (dolist (p (rest primes))
      (format stream "P=~A  N=~A~%" p (second (funcall function p))))))
