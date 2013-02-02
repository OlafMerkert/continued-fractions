(in-package :continued-fractions-power-series)

(defclass sqrt-continued-fraction (continued-fraction)
  ((radicand :initarg :radicand
             :reader radicand)
   (rn :reader rn)
   (sn :reader sn))
  (:documentation "We know the complete quotients of (sqrt radicand)
  will all be of the form (/ (+ rn (sqrt radicand) sn). TODO"))

(defmacro with-cf2 (continued-fraction &body body)
  `(with-cf ,continued-fraction
     (with-accessors ((d radicand)
                      (rn rn)
                      (sn sn))
         cf
       ,@body)))

(defmethod setup-continued-fraction ((cf sqrt-continued-fraction))
  (with-slots ((d radicand)
               starting
               complete-quotients
               (an partial-quotients)
               rn
               sn)
      cf
    (setf starting (sqrt d))
    (let ((a0 (series-truncate starting)))
      ;; the main calculations
      (setf an (make-lazy-array (:start (a0) :index-var n)
                 (/ (+ (lazy-aref rn n) a0)
                    (lazy-aref sn n)))
            rn (make-lazy-array (:start ((zero 'polynomial)) :index-var n)
                 (let ((n-1 (- n 1)))
                   (- (* (lazy-aref sn n-1)
                         (lazy-aref an n-1))
                      (aref this n-1))))
            sn (make-lazy-array (:start ((one 'polynomial)) :index-var n)
                 (/ (- d (expt (lazy-aref rn n) 2))
                    (aref this (- n 1))))))
    ;; additional setup
    (setf complete-quotients (make-lazy-array (:index-var n)
                               (/ (+ (lazy-aref rn n) starting)
                                  (lazy-aref sn n))))
    (setup-continued-fraction-approx-fractions cf)))


(defmethod  find-pure-period-length ((continued-fraction sqrt-continued-fraction)
                                     &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the period can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (one-p (lazy-aref sn i))
            (return i))
          (finally (return nil)))))

(defmethod  find-pure-quasiperiod-length ((continued-fraction sqrt-continued-fraction)
                                     &key (length-bound 40))
  "In fact, (sqrt radicand) has not a pure cf expansion, but a0
  + (sqrt radicand) does, and the quasiperiod can be detected very easily."
  (with-cf2 continued-fraction
    (iter (for i from 1 to length-bound)
          (progress-event)
          (when (<= (degree (lazy-aref sn i)) 0)
            (return (values i (lazy-aref sn i))))
          (finally (return nil)))))

