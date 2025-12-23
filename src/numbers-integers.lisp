(in-package :cl-utils)

(defun reverse-number--fixnum (n)
  "Reverse the N, which is supposed to be a fixnum >= 0.
For instance: 123 --> 321.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum n))
  (labels ((sub (num acc)
             (declare (type fixnum acc))
             (if (= num 0)
	         acc
                 (multiple-value-bind (quotient remainder)
                     (floor num 10)
                   (sub quotient (the fixnum (+ (* 10 acc) remainder)))))))
    (sub n 0)))

;; end
