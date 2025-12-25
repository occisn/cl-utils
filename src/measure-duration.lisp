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

(defun SHOW-benchmark-5-times-A ()
  "Execute a function 5 times, print each duration, and report the quickest."

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (flet ((foo ()
           (let ((n 100000000)
                 (tmp 0.0d0))
             (dotimes (i n)
               (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
                 (incf tmp (* sign (/ 1.0d0 (+ (* 2 i) 1))))))
             (setq tmp (* 4 tmp))
             tmp)))
    
    (let ((nb-runs 5)
          (durations '())
          (real-base 0)
          (duration 0))
      (dotimes (i nb-runs)
        (setq real-base (get-internal-real-time))
        (foo) ; <-- the function
        (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0))
        (format t "Run ~D / ~D: ~A seconds~%" (1+ i) nb-runs duration)
        (push duration durations))
      
      (let ((quickest (apply #'min durations))
            (slowest (apply #'max durations)))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
        (format t "Run ~D / ~D: ~A seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~A seconds~%" quickest)
        (format t "=> slowest time:  ~A seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest))))
        nil))))

(defun SHOW-benchmark-5-times-B ()
  "Execute function 5 times, print each duration, and report the quickest.
In this version, the function shall return the execution duration to be benchmarked.
This variant enables to do other things in the version, outside of the measured time, for instance printing result."

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (flet ((foo ()
           (let ((real-base (get-internal-real-time))
                 (duration 0))
             
             (let ((n 100000000)
                   (tmp 0.0d0))
               (dotimes (i n)
                 (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
                   (incf tmp (* sign (/ 1.0d0 (+ (* 2 i) 1))))))
               (setq tmp (* 4 tmp))
               (format t "Leibniz formula with n = ~a ==> pi = ~a~%" n tmp))
             
             (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0))
             (format t "Executed in ~f seconds~%" duration)
             duration)))
    
    (let ((nb-runs 5)
          (durations '()))
      (dotimes (i nb-runs)
        (let ((duration (foo)))         ; <--- the function
          (format t "Run ~D / ~D: ~A seconds~%" (1+ i) nb-runs duration)
          (push duration durations)))
      (let ((quickest (apply #'min durations))
            (slowest (apply #'max durations)))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
        (format t "Run ~D / ~D: ~A seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~A seconds~%" quickest)
        (format t "=> slowest time:  ~A seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest))))
        nil))))

;;; end
