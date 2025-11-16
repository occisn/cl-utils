(in-package :cl-utils)

(defun largest-prime-factor (n)
  "Return the largest prime factor of N. N is supposed to be an integer > 1.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum n))
  
  (let ((largest 0))
    (declare (type fixnum largest))
    
    ;; Remove factors of 2
    (loop while (= 0 (mod n 2)) do
      (setf largest 2)
      (setf n (/ n 2)))

    ;; Remove factors of 3
    (loop while (= 0 (mod n 3)) do
      (setf largest 3)
      (setf n (/ n 3)))

    ;; Test divisors of the form 6k-1 and 6k+1
    (let ((i 5))
      (loop while (<= i (isqrt n)) do
        (cond
          ((= 0 (mod n i))
           (setf largest i)
           (setf n (/ n i)))
          ((= 0 (mod n (+ i 2)))
           (setf largest (+ i 2))
           (setf n (/ n (+ i 2))))
          (t
           (incf i 6)))))

    ;; If n is still > 1, it is prime
    (if (> n 1)
        (setf largest n))

    largest))

;;; end
