(in-package :continued-fractions-power-series)

;;; partial-quotients alternative approach, which does not collect all the data, but
;;; just moves on in the continued-fraction exppartial-quotients

(defstruct (square-root-continued-fraction)
  index
  d
  root-of-d
  a
  r
  s)

(defun init-square-root-continued-fraction (d)
  (let ((rd (sqrt d)))
    (make-square-root-continued-fraction :index 0
                :d d
                :rd rd
                :a (series-truncate rd)
                :r (zero 'polynomial)
                :s (one 'polynomial))))

(defun step-square-root-continued-fraction (x)
  (let* ((r (- (* (square-root-continued-fraction-s x)
                  (square-root-continued-fraction-a x))
               (square-root-continued-fraction-r x)))
         (s (/ (- (square-root-continued-fraction-d x) (expt r 2))
               (square-root-continued-fraction-s x))))
    (make-square-root-continued-fraction :index (1+ (square-root-continued-fraction-index x))
                :d  (square-root-continued-fraction-d  x)
                :rd (square-root-continued-fraction-rd x)
                :a (series-truncate (/ (+ r (square-root-continued-fraction-rd x))
                                       s))
                :r r
                :s s)))

(defun square-root-continued-fraction-quasi-period-length (d degree-bound)
  ;; todo rewrite using iterate or something else
  (loop
     for x = (step-square-root-continued-fraction (init-square-root-continued-fraction d)) then (step-square-root-continued-fraction x)
     summing (degree (square-root-continued-fraction-a x)) into deg-p
     do (princ ".")
     when (zerop (degree (square-root-continued-fraction-s x)))
     do (return (values t (square-root-continued-fraction-index x) deg-p))
     when (> deg-p degree-bound)
     do (return (values nil (square-root-continued-fraction-index x) deg-p))))
