(in-package :cl-utils)

(defun measure-duration-example  ()
  "Example of duration measurement"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((real-base (get-internal-real-time))
        (duration 0))
    ;; do something
    (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0))
    (format t "Executed in ~a seconds~%" duration)))

;; end
