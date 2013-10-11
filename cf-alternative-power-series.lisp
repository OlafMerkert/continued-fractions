(defpackage :continued-fractions-alternative-power-series
  (:nicknames :cf-ps-a)
  (:shadowing-import-from :generic-math :+ :- :* :/ :expt := :sqrt :summing :^ :_)
  (:use :cl :ol :iterate :generic-math
        :polynomials :power-series
        :continued-fractions-power-series)
  (:export
   #:alternative-continued-fraction
   #:series-drop-first-term
   #:series-first-term
   #:alternative-continued-fraction-map))

(in-package :continued-fractions-alternative-power-series)

;;; first define an alternative truncation operation.
(defun series-first-term (series)
  "Take the leading monomial term of the given SERIES if it has
non-negative degree."
  (unless (simplified-p series)
    (error "Can only extract first term from simplified series."))
  (if (<= 0 (degree series))
      (make-monomial
       (degree series)
       (leading-coefficient series))
      (zero series)))

(defun series-drop-first-term (series)
  "Remove the leading monomial term of the given SERIES if it has non-negative degree."
  (if (<= 0 (degree series))
      (make-instance 'power-series
                     :degree (- (degree series) 1)
                     :coefficients (lazy-array-drop (coefficients series) 1))
      series))

;;; then use this to define an alternative cf map
(defun alternative-continued-fraction-map (series)
  (/ (simplify (series-drop-first-term series))))

;;; and customise the cf class
(defclass alternative-continued-fraction (continued-fraction)
  ())

;;; all we need to do is change the setup
(defmethod setup-continued-fraction ((cf alternative-continued-fraction))
  (with-slots (starting
               (alphan complete-quotients)
               (an partial-quotients))
      cf
    (setf alphan (make-instance 'infinite-sequence:infinite+-sequence
                                :fill-strategy :sequential
                                :data+ (vector starting)
                                :generating-function
                                (lambda (this n)
                                  (alternative-continued-fraction-map
                                   (infinite-sequence:sref this (- n 1)))))
          an (make-instance 'infinite-sequence:infinite+-sequence
                            :fill-strategy :sequential
                            :generating-function
                            (lambda (this n)
                              (series-first-term (infinite-sequence:sref alphan n))))))
  (setup-continued-fraction-approx-fractions cf))
