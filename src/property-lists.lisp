;;;; Utilities for property lists.
;;;;
;;;; Useful standard functions for property lists.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-property-lists-general ()
  ""

  (let ((x '(:name "mary" :gender "female" age 21)))

    ;; format:
    ;; (key1 value1 key2 value2)
    
    ;; get element:
    (getf x :age)

    ;; modify or add:
    (setf (getf x :city) "New York")))

(defun SHOW-all-property-lists ()
    ""
    (format t "~%~%======~%=== PROPERTY LISTS~%======~%")
    (format t "~%")
    (SHOW-property-lists-general))

;;; end
