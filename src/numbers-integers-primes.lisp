;;;; Utilities for numbers integers primes.

(in-package :cl-utils)

(defun largest-prime-factor (n)
  "Return the largest prime factor of N. N is supposed to be an integer > 1.
For instance: 13195 = 5 x 7 x 13 x 29 --> 29.
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
    (when (> n 1)
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

;;; ===
;;; === PRIMES BELOW ===
;;; ===

(defun primes-below-as-list (n)
  "Return the list of all primes strictly below fixnum N, using a sieve of Eratosthenes."
  (declare (type fixnum n))
  (when (< n 3) (return-from primes-below-as-list (if (= n 2) '(2) nil)))
  (let ((sieve (make-array n :element-type 'bit :initial-element 1)))
    (setf (aref sieve 0) 0
          (aref sieve 1) 0)
    (loop for i of-type fixnum from 2 to (isqrt (the fixnum (- n 1)))
          when (= 1 (aref sieve i))
            do (loop for j of-type fixnum from (the fixnum (* i i)) below n by i
                     do (setf (aref sieve j) 0)))
    (loop for i of-type fixnum from 2 below n
          when (= 1 (aref sieve i))
            collect i)))

(defun primes-below-as-vector (n)
  "Return a fixnum vector of all primes strictly below fixnum N."
  (declare (type fixnum n))
  (coerce (primes-below-as-list n) 'vector))

(defun primep-vector-below (nmax)
  "Return a bit vector of length NMAX where index I is 1 if I is prime, 0 otherwise."
  (declare (type fixnum nmax))
  (let ((sieve (make-array nmax :element-type 'bit :initial-element 1)))
    (when (> nmax 0) (setf (aref sieve 0) 0))
    (when (> nmax 1) (setf (aref sieve 1) 0))
    (loop for i of-type fixnum from 2 to (isqrt (the fixnum (- nmax 1)))
          when (= 1 (aref sieve i))
            do (loop for j of-type fixnum from (the fixnum (* i i)) below nmax by i
                     do (setf (aref sieve j) 0)))
    sieve))

;;; ===
;;; === PRIME DECOMPOSITION ===
;;; ===

(defun prime-decomposition-grouped (n)
  "Return the prime decomposition of fixnum N as a list of (prime . exponent) pairs.
For instance: 100 --> '((2 . 2) (5 . 2)).
N is supposed to be an integer >= 2."
  (declare (type fixnum n))
  (let ((result '()))
    ;; Factor out 2
    (let ((count 0))
      (declare (type fixnum count))
      (loop while (= 0 (mod n 2))
            do (incf count)
               (setq n (the fixnum (floor n 2))))
      (when (> count 0) (push (cons 2 count) result)))
    ;; Factor out odd numbers from 3
    (loop for factor of-type fixnum from 3 by 2
          while (<= (the fixnum (* factor factor)) n)
          do (let ((count 0))
               (declare (type fixnum count))
               (loop while (= 0 (mod n factor))
                     do (incf count)
                        (setq n (the fixnum (floor n factor))))
               (when (> count 0) (push (cons factor count) result))))
    ;; If n > 1 then it is a remaining prime factor
    (when (> n 1) (push (cons n 1) result))
    (nreverse result)))

(defun prime-divisors (n)
  "Return the list of prime divisors of fixnum N.
For instance: 100 --> '(2 5).
N is supposed to be an integer >= 2."
  (declare (type fixnum n))
  (mapcar #'car (prime-decomposition-grouped n)))

;;; ===
;;; === DIVISORS ===
;;; ===

(defun list-of-divisors (n &key (strict nil))
  "Return the list of divisors of fixnum N.
If :STRICT is t, the list does not include 1 and N.
N is supposed to be an integer >= 1."
  (declare (type fixnum n))
  (let* ((isqrt (isqrt n))
         (perfect-square-p (= n (the fixnum (* isqrt isqrt)))))
    (declare (type fixnum isqrt))
    (loop for i of-type fixnum from (if strict 2 1) to isqrt
          with small of-type list = '()
          with big of-type list = '()
          when (= 0 (mod n i)) do
            (push i small)
            (push (the fixnum (/ n i)) big)
          finally (return (append
                           (nreverse small)
                           (if perfect-square-p (cdr big) big))))))

(defun nb-of-divisors (n &key (strict nil))
  "Return the number of divisors of fixnum N.
If :STRICT is t, does not count 1 and N.
N is supposed to be an integer >= 1."
  (declare (type fixnum n))
  (let ((res 1))
    (declare (type fixnum res))
    (loop for pair in (prime-decomposition-grouped n)
          for b of-type fixnum = (the fixnum (cdr pair))
          do (setq res (the fixnum (* res (the fixnum (+ b 1))))))
    (the fixnum (- res (if strict 2 0)))))

(defun list-of-proper-divisors (n)
  "Return the list of proper divisors of fixnum N (all divisors except N itself).
For instance: 6 --> '(1 2 3).
N is supposed to be an integer >= 1."
  (declare (type fixnum n))
  (cons 1 (list-of-divisors n :strict t)))

(defun sum-of-proper-divisors (n)
  "Return the sum of proper divisors of fixnum N.
For instance: 9 --> 1+3 = 4.
N is supposed to be an integer >= 1."
  (declare (type fixnum n))
  (if (= n 1)
      1
      (let* ((isqrt (isqrt n))
             (perfect-square-p (= n (the fixnum (* isqrt isqrt)))))
        (declare (type fixnum isqrt))
        (the fixnum
             (+ (loop for i of-type fixnum from 2 to isqrt
                      when (= 0 (mod n i))
                        sum (the fixnum (+ i (the fixnum (/ n i)))) of-type fixnum)
                1
                (if perfect-square-p (- isqrt) 0))))))

(defun abundantp (n)
  "Return t if and only if fixnum N is abundant (sum of proper divisors > N).
N is supposed to be an integer >= 1."
  (declare (type fixnum n))
  (> (sum-of-proper-divisors n) n))

(defun totient-below (n m)
  "Return the number of integers strictly below M that are coprime with N.
Not optimized."
  (declare (type (integer 1 #.most-positive-fixnum) n)
           (type fixnum m))
  (let ((count 0))
    (declare (type fixnum count))
    (loop for k of-type fixnum from 1 below m
          when (eql 1 (the (integer 1 #.most-positive-fixnum) (gcd n k)))
            do (incf count))
    count))

;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-numbers-integers-primes ()
  ""
  (format t "~%~%======~%=== NUMBER-INTEGERS-PRIMES~%======~%")
  (format t "~%")
  (format t "(largest-prime-factor 13195) --> ~a~%" (largest-prime-factor 13195))
  (format t "~%")
  (format t "9973 is prime: ~a~%" (primep 9973))
  (format t "~%")
  (format t "Next prime after 9971 is ~a~%" (next-prime 9971))
  (format t "~%")
  (format t "100th prime is ~a~%" (nth-prime 100))
  (format t "~%")
  (format t "Primes below 30: ~a~%" (primes-below-as-list 30))
  (format t "~%")
  (format t "(prime-decomposition-grouped 360) --> ~a~%" (prime-decomposition-grouped 360))
  (format t "~%")
  (format t "(list-of-divisors 12) --> ~a~%" (list-of-divisors 12))
  (format t "~%")
  (format t "(nb-of-divisors 12) --> ~a~%" (nb-of-divisors 12))
  (format t "~%")
  (format t "(sum-of-proper-divisors 12) --> ~a~%" (sum-of-proper-divisors 12))
  (format t "~%")
  (format t "(abundantp 12) --> ~a~%" (abundantp 12)))

;;; end
