(in-package :cl-utils)

(defun SHOW-measure-duration  ()
  "Example of duration measurement"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((real-base (get-internal-real-time))
        (duration 0))

    ;; do something:
    ;; calculation of pi by Leibniz formula
    (let ((n 100000000)
          (tmp 0.0d0))
      (dotimes (i n)
        (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
          (incf tmp (* sign (/ 1.0d0 (+ (* 2 i) 1))))))
      (setq tmp (* 4 tmp))
      (format t "Leibniz formula with n = ~a ==> pi = ~a~%" n tmp))
    
    (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0))
    (format t "Executed in ~f seconds~%" duration)))

;;; end
