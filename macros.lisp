(in-package :cl-utils)

(defmacro while (condition &body body)
  "While macro based on CONDITION and BODY."
  `(loop while ,condition
         do (progn ,@body)))

(defmacro aprogn (&rest args)
  "Anaphoric progn.
Source : On Lisp"
  (declare (type cons args))
  (funcall (%alambda (args)
                     (declare (type cons args))
	             (case (length args)
	               (0 nil)
	               (1 (car args))
	               (t `(let ((it ,(car args)))
		             ,(self (cdr args))))))
	   args))

;;; end
