(defpackage :continued-fractions-rationals
  (:nicknames :cf-rat)
  (:use :cl :ol :iterate
        :cf-ps)
  (:export))

(in-package :continued-fractions-rationals)

(defun cf-rat (rational)
  (multiple-value-bind (q r) (floor (numerator rational)
                                    (denominator rational))
    (values q (unless (zerop r)
                (/ (denominator rational) r)))))

(defmacro! awhile2 (test &body body)
  `(do ((,g!terminate nil))
       (,g!terminate)
       (multiple-value-bind (it it?) ,test
         (if it?
             (progn ,@body)
             (setf ,g!terminate t)))))

(defun rational-continued-fraction (rational)
  (let ((rat rational)
        (partial-quotients nil)
        (complete-quotients (list rational)))
    (awhile2 (cf-rat rat)
      (push it partial-quotients)
      (push it? complete-quotients)
      (setf rat it?))
    (values (list->array/reverse (list* (first complete-quotients)
                             partial-quotients))
            (list->array/reverse complete-quotients))))

(defun list->array/reverse (list)
  (let* ((len (length list))
         (array (make-array len)))
    (iter (for l in list)
          (for i from (- len 1) downto 0)
          (setf (aref array i) l))
    array))

(defun eval-cf (cf-array)
  (iter (for an in-vector cf-array downto 0)
        (for res first an
             then (+ an (/ res)))
        (finally (return res))))


(defclass continued-fraction-rational (continued-fraction)
  ()
  (:documentation "TODO"))

(defmethod setup-continued-fraction ((cf continued-fraction-rational))
  (with-slots (starting
               (alphan complete-quotients)
               (an partial-quotients))
      cf
    (setf alphan (make-lazy-array (:start (starting) :index-var n)
                   ))))
