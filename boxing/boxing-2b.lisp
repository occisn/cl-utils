(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (ftype (function ((cons double-float t) fixnum fixnum) (values)) sub-2b))
(declaim (notinline sub-2b))
(defun sub-2b (res-list start end)
  ""
  (declare (type fixnum start end)
           (type (cons double-float t) res-list))
  (let ((i start)
        (tmp 0.0d0))
    (declare (type fixnum i)
             (type double-float tmp))
    (loop while (<= i (- end 2))
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0))))
               (setq tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 i) 3.0d0))))
               (setq i (+ i 2))))       ; end of loop
    (setf (car res-list) tmp))
  (values))

(defun boxing-2b ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((start-time (get-internal-real-time))
        (tmp-list (list 0.0d0))
        (tmp 0.0d0))
    (declare (type double-float tmp)
             (type (cons double-float t) tmp-list))
    (loop for i of-type fixnum from 0 below 100000000
          do (let* ((start (* i 100))
                    (end (+ start 100)))
               (declare (type fixnum start end))
               (sub-2b tmp-list start end)
               (setq tmp (+ tmp (the double-float (car tmp-list))))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration)))))

;;; end
