(in-package :cl-utils-tests)

(parachute:define-test test-unliteral--fixnum-vector
  (let* ((input #(1 2 3 4 5 6 7 8 9 10))
         (result (unliteral--fixnum-vector input)))
    (loop for i of-type fixnum from 0 below (length input)
          do (parachute:is = (aref input i) (aref result i)))))

;;; end
