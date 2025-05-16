(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (ftype (function (fixnum fixnum) (cons double-float t)) sub-2a))
(declaim (notinline sub-2a))
(defun sub-2a (start end)
  ""
  (declare (type fixnum start end))
  (let ((i start)
        (tmp 0.0d0))
    (declare (type fixnum i)
             (type double-float tmp))
    (loop while (<= i (- end 2))
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0))))
               (setq tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 i) 3.0d0))))
               (setq i (+ i 2))))
    (list tmp)))

(defun boxing-2a ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((start-time (get-internal-real-time))
        (tmp 0.0d0))
    (declare (type double-float tmp))
    (loop for i of-type fixnum from 0 below 100000000
          do (let* ((start (* i 100))
                   (end (+ start 100))
                   (sub-list (sub-2a start end))
                   (sub-value (the double-float (car sub-list))))
               (declare (type fixnum start end)
                        (type (cons double-float t) sub-list)
                        (type double-float sub-value))
               (setq tmp (+ tmp sub-value))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; end
