(in-package :cl-utils-tests)

;;; === farey-immediately-on-left-of

(parachute:define-test farey-immediately-on-left-of
  (parachute:is equal '(2 5) (farey-immediately-on-left-of 8 3 7))
  (parachute:is equal '(428570 999997) (farey-immediately-on-left-of 1000000 3 7)))

;;; === farey-length

(parachute:define-test farey-length-test
  (parachute:is = 23 (farey-length 8)))

;;; === end
