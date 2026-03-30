(in-package :cl-utils)

(defun SHOW-measure-duration  (&optional (n 100000000))
  "Example of duration measurement"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((real-base (get-internal-real-time))
        (duration 0))

    ;; do something:
    ;; calculation of pi by Leibniz formula
    (let ((tmp 0.0d0))
      (dotimes (i n)
        (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
          (incf tmp (* sign (/ 1.0d0 (+ (* 2 i) 1))))))
      (setq tmp (* 4 tmp))
      (format t "Leibniz formula with n = ~a ==> pi = ~a~%" n tmp))
    
    (setq duration (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0))
    (format t "Executed in ~f seconds~%" duration)))

(defun SHOW-benchmark-5-times-A (&optional (n 100000000))
  "Execute a function 5 times, print each duration, and report the quickest."

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (flet ((foo ()
           (let ((tmp 0.0d0))
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
        (if (zerop quickest)
            (format t "=> slowest time:  ~A seconds~%" slowest)
            (format t "=> slowest time:  ~A seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
        nil))))

(defun SHOW-benchmark-5-times-B (&optional (n 100000000))
  "Execute function 5 times, print each duration, and report the quickest.
In this version, the function shall return the execution duration to be benchmarked.
This variant enables to do other things in the version, outside of the measured time, for instance printing result."

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (flet ((foo ()
           (let ((real-base (get-internal-real-time))
                 (duration 0))
             
             (let ((tmp 0.0d0))
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
      (let ((duration (foo)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (if (zerop quickest)
            (format t "=> slowest duration:  ~,4F seconds~%" slowest)
            (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest))))))
      nil))))

(defmacro with-timing ((var) &body body)
  `(let ((start (get-internal-real-time)))
     (prog1
         (progn ,@body)
       (setf ,var
             (/ (- (get-internal-real-time) start)
                internal-time-units-per-second)))))

(defun SHOW-with-timing (&optional (n 100000000))
  "Example of duration measurement"
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((duration)
        (tmp 0.0d0))
    (with-timing (duration)
      ;; do something:
      (dotimes (i n)
        (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
          (incf tmp (* sign (/ 1.0d0 (+ (* 2 i) 1))))))
      (setq tmp (* 4 tmp)))
    (format t "Leibniz formula with n = ~a ==> pi = ~a (in ~f seconds)~%" n tmp duration)))

(defun SHOW-all-measure-duration ()
  ""
  (format t "~%~%======~%=== MEASURE-DURATION~%======~%")
  (format t "~%")
  (SHOW-measure-duration 10000)
  (format t "~%")
  (SHOW-benchmark-5-times-A 10000)
  (format t "~%")
  (SHOW-benchmark-5-times-B 10000)
  (format t "~%")
  (SHOW-with-timing 10000))

;;; end
