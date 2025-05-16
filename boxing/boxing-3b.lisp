(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (ftype (function ((simple-array double-float (*)) fixnum fixnum) (values)) sub-3b))
(declaim (notinline sub-3b))
(defun sub-3b (res-box start end)
  ""
  (declare (type fixnum start end)
           (type (simple-array double-float (*)) res-box))
  (let ((i start)
        (tmp 0.0d0))
    (declare (type fixnum i)
             (type double-float tmp))
    (loop while (<= i (- end 2))
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0))))
               (setq tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 i) 3.0d0))))
               (setq i (+ i 2))))       ; end of loop
    (setf (aref res-box 0) tmp))
  (values))

(defun boxing-3b ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((start-time (get-internal-real-time))
        (tmp-box (make-array 1 :element-type 'double-float :initial-element 0.0d0))
        (tmp 0.0d0))
    (declare (type double-float tmp)
             (type (simple-array double-float (*)) tmp-box))
    (loop for i of-type fixnum from 0 below 100000000
          do (let* ((start (* i 100))
                    (end (+ start 100)))
               (declare (type fixnum start end))
               (sub-3b tmp-box start end)
               (setq tmp (+ tmp (aref tmp-box 0)))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration)))))

;;; end
