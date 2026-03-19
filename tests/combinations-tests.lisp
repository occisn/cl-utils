(in-package :cl-utils-tests)

(parachute:define-test with-combinations-of-index-test
  (parachute:is = 10
                (let ((count 0))
                  (declare (type fixnum count))
                  (with-combinations-of-index (comb :of 5 3)
                    comb ; use comb to avoid unused warning
                    (incf count))
                  count)))

;; end
