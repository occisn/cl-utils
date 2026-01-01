(in-package :cl-utils)

(defmacro boolean-value (x)
  "Return t if and only if X is true.
This is equivalent to a coercion to boolean.
It allows for instance to convert 2 to t.
(v1, available in occisn/cl-utils GitHub repository)
"
  `(not (not ,x)))

(defun SHOW-all-booleans ()
  ""
  (format t "~%~%======~%=== BOOLEANS~%======~%")
  (format t "~%")
  (format t "boolean value of 2 is ~a~%" (boolean-value 2)))

;;; end
