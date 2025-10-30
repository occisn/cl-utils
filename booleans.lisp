(defpackage cl-utils--booleans
  (:use :cl))

(in-package :cl-utils--booleans)

(defmacro boolean-value (x)
  "Return t if and only if X is true.
This is equivalent to a coercion to boolean.
It allows for instance to convert 2 to t.
(v1, available in occisn/cl-utils GitHub repository)
"
  `(not (not ,x)))

;;; end
