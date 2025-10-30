(defpackage cl-utils--macros
  (:use :cl))

(in-package :cl-utils--macros)

(defmacro while (condition &body body)
  "While macro based on CONDITION and BODY.
(v1, available in occisn/cl-utils GitHub repository)"
  `(loop while ,condition
         do (progn ,@body)))

(defmacro aprogn (&rest args)
  "Anaphoric progn.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type cons args))
  (funcall (%alambda (args)
                     (declare (type cons args))
	             (case (length args)
	               (0 nil)
	               (1 (car args))
	               (t `(let ((it ,(car args)))
		             ,(self (cdr args))))))
	   args))
;; See "On Lisp" book

;;; end
