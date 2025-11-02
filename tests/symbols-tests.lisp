(in-package :cl-utils-tests)

;;; === function-to-string

(parachute:define-test test-function-to-string
  (parachute:is string= "SIN" (function-to-string #'sin)))

;;; end
