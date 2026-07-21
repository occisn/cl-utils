;;;; Utilities for association lists.
;;;;
;;;; Useful standard functions for association lists.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-association-lists-general ()
  ""

  (let ((x '(("banana" 2) ("apple" 3))))

    ;; format:
    ;; ((key1 value1) (key2 value2) ... )
    ;;   or
    ;; ((key1 . value1) (key2 . value2) ... )


    ;; assoc returns a pair
    ;; caution if keys are strings:
    (assoc "banana" x) ; ==> NIL since default test is performed wth EQL
    (assoc "banana" x :test #'equal) ; ==> ("banana" 2)

    ;; ass a pair:
    (push '("orange" 4) x)

    ;; replace value if the format of the alist is (( . ) ( . )) :
    (setf (cdr (assoc "banana" x :test #'equal)) 22)
    (rplacd (assoc "banana" x :test #'equal) 222)))

(defun SHOW-all-association-lists ()
  ""
  (format t "~%~%======~%=== ASSOCIATION LISTS~%======~%")
  (format t "~%")
  (SHOW-association-lists-general))

;;; end
