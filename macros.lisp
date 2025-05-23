(in-package :cl-utils)

(defmacro while (condition &body body)
  "While macro based on CONDITION and BODY."
  `(loop while ,condition
         do (progn ,@body)))

;;; end
