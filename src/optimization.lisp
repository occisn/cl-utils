(in-package :cl-utils)

(defun SHOW-all-optimization (m n)
  "Example of methods to comply with 'speed 3' compilation notes."
  (declare (type fixnum m n))

    (format t "~%~%======~%=== OPTIMIZATION~%======~%")

  (let ((tmp 0))
    (declare (type fixnum tmp))

    ;; expt
    (setq tmp (the fixnum (expt m (the (integer 0) (- n 2)))))
    ;; (integer 0) can be replaced by unsigned-byte

    ;; ceiling
    ;; (the fixnum (ceiling...
    ;; reason: The return type of ceiling in CL is always (values integer remainder) where integer is a general integer (not necessarily a fixnum)

    ;; floor of double-float
    ;; --> see floor-of-positive-df in double-floats utils
    
    ;; parachute
    ;; (locally
    ;;     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

    (format t "~%")
    (format t "tmp = ~a~%" tmp)))

;;; end
