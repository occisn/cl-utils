(in-package :cl-utils)

(defun optimization-examples (m n)
  "Example of tricks to comply with 'speed 3' compilation notes."
  (declare (type fixnum m n))
  
  (let ((tmp 0))
    (declare (type fixnum tmp))

    ;; expt
    (setq tmp (the fixnum (expt m (the (integer 0) (- n 2)))))
    ;; (integer 0) can be replaced by unsigned-byte

    ;; parachute
    ;; (locally
    ;;     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

    (format t "tmp = ~a~%" tmp)))

;;; end
