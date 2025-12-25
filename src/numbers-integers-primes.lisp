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

(defun primep (n)
  "Return t if and only if fixnum N is prime.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum n))
  (block outer
    (when (or (= n 2) (= n 3) (= n 5) (= n 7))
      (return-from outer t))
    (when (or (<= n 1) (evenp n) (zerop (mod n 3)))
      (return-from outer nil))
    (loop for factor of-type fixnum from 5 by 6
          with root-n of-type fixnum = (isqrt n) ; root-n^2 <= n < (root-n + 1)^2
          while (<= factor root-n)
          never (or (zerop (mod n factor))
                    (zerop (mod n (+ factor 2)))))))

(defun next-prime (n)
  "Return next prime after fixnum N.
Note: if N is prime, the result is not N.
Require 'primep'.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum n))
  (cond ((<= n 1) 2)
        (t
         (loop for i of-type fixnum from (+ n (if (evenp n) 1 2)) by 2
               when (primep i) return i))))

(defun nth-prime (rank)
  "Return RANK-th prime.
For instance: 1 --> 2 ; 2 --> 3 ; 6 --> 13.
The argument is supposed to be an integer >= 1.
Require 'primep' and 'next-prime'.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum rank))
  (loop for n of-type fixnum = 2 then (next-prime n)
        and count of-type fixnum = 1 then (the fixnum (+ count 1))
        when (= count rank) do (return n)))

;;; end
