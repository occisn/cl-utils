(in-package :cl-utils)

;;; ===
;;; === Constants
;;; ===

(declaim (type double-float +square-root-of-5+))
(defparameter +square-root-of-5+ (sqrt 5.0d0) "square root of 5")

(declaim (type double-float +phi+))
(defparameter +phi+ (* (+ 1.0d0 (sqrt 5.0d0)) 0.5d0) "phi = golden ratio")

;; (1-V5)/2 = -1/phi
(declaim (type double-float +phi2+))
(defparameter +phi2+ (* (- 1.0d0 (sqrt 5.0d0)) 0.5d0) "-1/phi where phi is golden ratio")


;;; ===
;;; === POWER ===
;;; ===

(declaim (ftype (function (fixnum fixnum) fixnum) power))
(defun power (a b)
  "Return A^B as a fixnum, where A is a fixnum and B is a fixnum >= 0.
0^0 = 1 as expected."
  (declare (type fixnum a b))
  (if (= 0 b)
      1
      (loop with res of-type fixnum = a
            for _i of-type fixnum from 1 below b
            do (setq res (the fixnum (* res a)))
            finally (return res))))

(declaim (ftype (function (fixnum fixnum) integer) power--bigint))
(defun power--bigint (a b)
  "Return A^B as an integer, where A is a fixnum and B is a fixnum >= 0.
0^0 = 1 as expected."
  (declare (type fixnum a b))
  (if (= 0 b)
      1
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (loop with res of-type integer = a
              for _i of-type fixnum from 1 below b
              do (setq res (the integer (* res a)))
              finally (return res)))))


;;; ===
;;; === FLOOR/CEILING TO POWER OF 10 ===
;;; ===

(declaim (ftype (function (fixnum) fixnum) floor-to-power-of-10))
(defun floor-to-power-of-10 (n)
  "Return the floor of the argument as a power of 10.
For instance: 123 --> 100 ; 4 --> 1.
The argument is supposed to be a fixnum >= 1."
  (declare (type fixnum n))
  (multiple-value-bind (n2 p)
      (loop
        for m of-type fixnum = n then (the fixnum (floor m 1000))
        for p of-type fixnum = 1 then (the fixnum (* p 1000))
        while (> m 999)
        finally (return (values m p)))
    (loop for m of-type fixnum = n2 then (the fixnum (floor m 10))
          for p2 of-type fixnum = p then (the fixnum (* p2 10))
          until (<= m 9)
          finally (return p2))))

(declaim (ftype (function (fixnum) fixnum) ceiling-to-power-of-10))
(defun ceiling-to-power-of-10 (n)
  "Return the ceiling of the argument as a power of 10.
For instance: 123 --> 1000 ; 4 --> 10.
The argument is supposed to be a fixnum >= 1."
  (declare (type fixnum n))
  (the fixnum (* 10 (floor-to-power-of-10 n))))

(defun concatenate-numbers (n m)
  "Return the concatenation of N and M.
For instance: 41 5 --> 415
              0 123 --> 123"
  (declare (type fixnum n m))
  (let ((tmp (ceiling-to-power-of-10 m)))
    (declare (type fixnum tmp))
    (the fixnum (+ m (the fixnum (* n tmp))))))


;;; ===
;;; =================
;;; === FIBONACCI ===
;;; =================
;;; ===

(declaim (ftype (function (fixnum) fixnum) fibonacci-through-iteration))
(defun fibonacci-through-iteration (n)
  "Return fib(N) through quick iteration."
  (declare (type fixnum n))
  (if (= n 0)
      0
      (loop
        for i of-type fixnum from 0
        for f1 of-type fixnum = 0 then f2
        and f2 of-type fixnum = 1 then (the fixnum (+ f1 f2))
        while (<= i (the fixnum (- n 1)))
        finally (return f1))))

(declaim (ftype (function (fixnum) fixnum) fibonacci-through-explicit-formula))
(defun fibonacci-through-explicit-formula (n)
  "Return fib(N) through explicit formula with phi."
  (declare (type fixnum n))
  (let* ((res (round-of-df (/ (- (expt +phi+ n) (expt +phi2+ n)) +square-root-of-5+))))
    (declare (type fixnum res))
    res))


;;; ===
;;; ==========================
;;; === INTEGERS SEQUENCES ===
;;; ==========================
;;; ===

(defun non-decreasing-p (lst)
  "Return t if and only if LST is a non-decreasing list."
  (declare (type list lst))
  (loop for L on lst
        always (or (null (cdr L)) (>= (the fixnum (cadr L)) (the fixnum (car L))))))

(defun group-factors (lst)
  "Group factors within _ordered_ fixnum list LST and return the result.
For instance: '(2 2 2 3)) --> ((2 . 3) (3 . 1))

Source: cl-mod-prime"
  (cdr (nreverse
        (reduce (lambda (prev next)
                  (declare (type fixnum next))
                  (destructuring-bind ((prime . count) . rest) prev
                    (declare (type fixnum prime count))
                    (if (= prime next)
                        (cons (cons prime (the fixnum (1+ count))) rest)
                        (cons (cons next 1) prev))))
                lst :initial-value (list (cons 0 0))))))

(defun iota (n &key (from 1))
  "Return the list (FROM ... N) where FROM is 1 by default."
  (declare (type fixnum n from))
  (loop for i of-type fixnum from from to n collect i))

(declaim (ftype (function (fixnum fixnum) list) make-list-within-range))
(defun make-list-within-range (first last)
  "Return list of consecutive fixnums between FIRST and LAST, both included.
For instance: 1 3 --> '(1 2 3)."
  (declare (type fixnum first last))
  (loop for n of-type fixnum from first to last collect n))

(defun new-sequence-fixnum-vector (length)
  "Return a vector of length LENGTH containing sequence 0...(N-1) as fixnum."
  (declare (type fixnum length))
  (let ((res (make-array length :element-type 'fixnum :initial-element 0)))
    (loop for i of-type fixnum from 0 below length do
      (setf (aref res i) i))
    res))

(defun new-random-fixnum-vector (length &key (mini 0) (maxi 100))
  "Return a vector of length LENGTH containing fixnum random numbers between MINI included (default: 0) and MAXI excluded (default: 100)."
  (declare (type fixnum length mini maxi))
  (unless (<= mini maxi) (error "Should not happen ; mini = ~s ; maxi = ~s" mini maxi))
  (let ((res (make-array length :element-type 'fixnum)))
    (loop for i of-type fixnum from 0 below length do
      (setf (aref res i) (+ mini (random (the fixnum (- maxi mini))))))
    res))


;;; ===
;;; =====================
;;; === COMBINATORICS ===
;;; =====================
;;; ===

(declaim (ftype (function (fixnum) fixnum) fact))
(defun fact (n)
  "Return factorial of fixnum N, as a fixnum.
0! = 1 as expected."
  (declare (type fixnum n))
  (the fixnum
       (if (or (= n 0) (= n 1))
           1
           (loop with res of-type fixnum = 1
                 for i of-type fixnum from 2 to n
                 do (setq res (the fixnum (* res i)))
                 finally (return res)))))

(declaim (ftype (function (fixnum) integer) fact--bigint))
(defun fact--bigint (n)
  "Return factorial of fixnum N, as an integer."
  (declare (type fixnum n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if (or (= n 0) (= n 1))
        1
        (loop with res of-type integer = 1
              for i of-type fixnum from 2 to n
              do (setq res (the integer (* res i)))
              finally (return res)))))

(defparameter %*fact-0-9-array* (make-array 10 :element-type 'fixnum :initial-contents (list 1 1 2 6 24 120 720 5040 40320 362880)))
(declaim (type (simple-array fixnum) %*fact-0-9-array*))

(defun fact-0-9 (d)
  "Return factorial of D which is supposed to be a 'digit' (0...9)."
  (declare (type fixnum d))
  (aref %*fact-0-9-array* d))

(defun combin (n p)
  "Return C(n,p) = n! / (p! * (n-p)!)."
  (declare (type fixnum n p))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((result 1))
      (declare (type integer result))
      (let ((k (min p (the fixnum (- n p)))))
        (declare (type fixnum k))
        (loop for i of-type fixnum from 0 below k
              do (setq result (the integer (/ (the integer (* result (the fixnum (- n i)))) (the fixnum (+ i 1))))))
        result))))


;;; ===
;;; ==========================
;;; === MODULAR ARITHMETIC ===
;;; ==========================
;;; ===

(defmacro mod-incf (place n base)
  "Same as (incf PLACE N), but modulo BASE.

For instance:
(let ((a 5))
  (mod-incf a 6 10)
  (print a))
;; --> 1"
  `(setf ,place (mod (+ ,place ,n) ,base)))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) mod-expt--fixnum))
(defun mod-expt--fixnum (base power divisor)
  "Same as (mod (expt BASE POWER) DIVISOR), but without intermediate bignums.

Source: cl-mod-prime, with adaptations."
  (declare (type fixnum base power divisor))
  (let ((a (the fixnum (mod base divisor)))
        (b 1))
    (declare (type fixnum a b))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (loop for i of-type fixnum downfrom (the fixnum (1- (integer-length power))) to 0
            do (if (logbitp i power)
                   (setf b (the fixnum
                                (mod (the fixnum
                                          (* (the integer
                                                  (mod
                                                   (the integer (* b b))
                                                   divisor))
                                             a))
                                     divisor)))
                   (setf b (the integer
                                (mod (the integer (* b b))
                                     divisor))))))
    b))


;;; ===
;;; ==============
;;; === DIGITS ===
;;; ==============
;;; ===

(declaim (inline char->digit))
(defun char->digit (c)
  "Convert char C representing a digit to an integer.
For instance: #\\1 --> 1."
  (declare (type character c))
  (the fixnum (- (char-code c) 48)))

(declaim (inline last-digit))
(defun last-digit (n)
  "Return the last digit of fixnum N.
For instance 10 --> 0."
  (declare (type fixnum n))
  (nth-value 1 (the fixnum (floor n 10))))

(defun first-digit (n)
  "Return first digit of fixnum N.
For instance: 29 --> 2."
  (declare (type fixnum n))
  (loop while (> n 999)
        do (setq n (the fixnum (floor n 1000))))
  (loop while (> n 9)
        do (setq n (the fixnum (floor n 10))))
  n)

(defmacro for-successive-digits-in-reverse-order ((d-symbol _in n) &body body)
  "Execute BODY for each successive digit of fixnum N.
Within BODY, the current digit is accessible by D-SYMBOL. Digits appear in reverse order.

Example: (for-successive-digits-in-reverse-order (d :in 123456)
   (print d))"
  (declare (ignorable _in))
  (with-gensyms (f r m)
    `(let ((,m ,n))
       (declare (type fixnum ,m))
       (while1 (> ,m 0)
               (multiple-value-bind (,f ,r) (floor ,m 10)
                 (let ((,d-symbol ,r))
                   ,@body)
                 (setq ,m ,f))))))

(declaim (ftype (function (fixnum) (cons fixnum)) number->digits))
(defun number->digits (n)
  "Convert a fixnum N to a list of digits.

For instance 123 --> '(1 2 3)."
  (declare (type fixnum n))
  (if (= n 0)
      '(0)
      (let ((acc '()))
        (for-successive-digits-in-reverse-order (d :in n)
                                                (push d acc))
        acc)))

(defun number->digits--bigint (n)
  "Convert an integer N to a list of digits.

For instance 123 --> '(1 2 3)."
  (declare (type integer n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if (= n 0)
        '(0)
        (let ((acc '()))
          (while1 (> n 0)
                  (multiple-value-bind (f r) (floor n 10)
                    (push r acc)
                    (setq n f)))
          acc))))

(defun nth-digit (n rank)
  "Return the digit at position RANK of fixnum N, counting starting at 1.
For instance, 56789 3 --> 7."
  (declare (type fixnum n rank))
  (elt (number->digits n) (the fixnum (- rank 1))))

(defun number->vector-of-digits (n)
  "Convert a fixnum N into a vector of digits.

For instance: 123 --> #(1 2 3)."
  (declare (type fixnum n))
  (let ((lst (number->digits n)))
    (declare (type list lst))
    (make-array (length lst) :element-type 'fixnum :initial-contents lst)))

(defun number->existing-vector-of-digits (n work-array)
  "Populate WORK-ARRAY with the digits of fixnum N, and return the number of digits.

Cells of WORK-ARRAY of index >= nb-digits are left unchanged.

For instance: 1234, #(-1 .... -1) --> 4
and the vector is modified into #(1 2 3 4 -1 ... -1)."
  (declare (type fixnum n)
           (type (simple-array fixnum) work-array))
  (if (= n 0)
      (progn
        (setf (aref work-array 0) 0)
        1)
      (let ((nb-digits 0))
        (declare (type fixnum nb-digits))
        (for-successive-digits-in-reverse-order (d :in n)
                                                (setf (aref work-array nb-digits) d)
                                                (incf nb-digits))
        ;; put the right order:
        (loop for i of-type fixnum from 0 to (the fixnum (ash (the fixnum (- nb-digits 1)) -1))
              do (rotatef (aref work-array i) (aref work-array (the fixnum (- nb-digits 1 i)))))
        nb-digits)))

(defun number->digits-set (n)
  "Convert a fixnum N to a set of digits (with no repetition).

For instance 1223 --> '(1 2 3)."
  (declare (type fixnum n))
  (if (= n 0)
      '(0)
      (let ((acc '()))
        (declare (type list acc))
        (for-successive-digits-in-reverse-order (d :in n)
                                                (pushnew d acc))
        acc)))

(defun number->digits-set--bigint (n)
  "Convert an integer N to a set of digits (with no repetition).

For instance 1223 --> '(1 2 3)."
  (declare (type integer n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (labels ((aux (nn &optional (acc '()))
               (if (= nn 0)
                   acc
                   (multiple-value-bind (f r) (floor nn 10)
                     (aux f (pushnew r acc))))))
      (if (= n 0)
          '(0)
          (aux n)))))

(defun list-of-digits->number (lst)
  "Convert a list LST of digits to a fixnum.

For instance: '(1 2 3) --> 123."
  (declare (type (cons fixnum) lst))
  (loop with res of-type fixnum = 0
        for x of-type fixnum in lst
        do (setq res (the fixnum (+ (the fixnum (* 10 res)) x)))
        finally (return res)))

(defun list-of-digits->number--bigint (lst)
  "Convert a list LST of digits to an integer.

For instance: '(1 2 3) --> 123."
  (declare (type (cons fixnum) lst))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (loop with res of-type integer = 0
          for x of-type fixnum in lst
          do (setq res (+ (* 10 res) x))
          finally (return res))))

(declaim (ftype (function ((simple-array fixnum)) fixnum) vector-of-digits->number))
(defun vector-of-digits->number (vector-of-digits)
  "Convert a vector of digits VECTOR-OF-DIGITS to a fixnum.

For instance: #(1 2 3) --> 123."
  (declare (type (simple-array fixnum (*)) vector-of-digits))
  (loop for x of-type fixnum across vector-of-digits
        for b of-type fixnum = x then (the fixnum (+ (the fixnum (* 10 b)) x))
        finally (return b)))

(declaim (ftype (function (fixnum) list) number->sorted-digits))
(defun number->sorted-digits (n)
  "Convert a fixnum N into a sorted list of digits.

For instance 5643 --> '(3 4 5 6)."
  (declare (type fixnum n))
  (sort (number->digits n) #'<))

(declaim (ftype (function (fixnum fixnum) fixnum) first-digits))
(defun first-digits (n m)
  "Return the number consisting of M first digits of fixnum N."
  (declare (type fixnum n m))
  (let ((tmp (power 10 m)))
    (declare (type fixnum tmp))
    (loop while (>= n tmp)
          do (setq n (the fixnum (floor n 10)))))
  n)

(declaim (ftype (function (integer fixnum) integer) first-digits--bigint))
(defun first-digits--bigint (n m)
  "Return the number consisting of M first digits of integer N."
  (declare (type integer n)
           (type fixnum m))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if (= n 0)
        0
        (let* ((log1 (* (log n) (/ 1.0d0 (log 10.0d0))))
               (nb-chiffres
                 (the fixnum (+ 1 (the fixnum (floor log1))))))
          (declare (type double-float log1)
                   (type fixnum nb-chiffres))
          (if (>= m nb-chiffres)
              n
              (let* ((b (the fixnum (- nb-chiffres m)))
                     (exp1 (expt 10 b)))
                (declare (type fixnum b))
                (nth-value 0 (floor n exp1))))))))

(declaim (ftype (function (fixnum) fixnum) nb-digits))
(defun nb-digits (n)
  "Return the number of digits of fixnum N.

For instance: 999 --> 3."
  (declare (type fixnum n))
  (let ((res 1))
    (declare (type fixnum res))
    (loop while (> n 999)
          do (setq n (the fixnum (floor n 1000))
                   res (the fixnum (+ res 3))))
    (loop while (> n 9)
          do (setq n (the fixnum (floor n 10))
                   res (the fixnum (+ res 1))))
    res))

(declaim (ftype (function (integer) fixnum) nb-digits--bigint))
(defun nb-digits--bigint (n)
  "Return the number of digits of integer N.

For instance: 999 --> 3."
  (declare (type integer n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((res 1))
      (declare (type fixnum res))
      (loop while (> n 999)
            do (setq n (floor n 1000)
                     res (+ res 3)))
      (loop while (> n 9)
            do (setq n (floor n 10)
                     res (+ res 1)))
      res)))

(declaim (ftype (function (fixnum) fixnum) sum-of-digits))
(defun sum-of-digits (n)
  "Return the sum of digits of fixnum N.

For instance 123 --> 6."
  (declare (type fixnum n))
  (let ((res 0))
    (declare (type fixnum res))
    (for-successive-digits-in-reverse-order (d :in n)
                                            (incf res d))
    res))

(defun sum-of-digits--bigint (n)
  "Return the sum of digits of integer N.

For instance 123 --> 6."
  (declare (type integer n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((res 0))
      (while1 (> n 0)
              (multiple-value-bind (f r) (floor n 10)
                (incf res r)
                (setq n f)))
      res)))

(declaim (ftype (function (fixnum) fixnum) product-of-digits))
(defun product-of-digits (n)
  "Return the product of digits of fixnum N.

For instance 1234 --> 24."
  (declare (type fixnum n))
  (let ((res 1))
    (declare (type fixnum res))
    (for-successive-digits-in-reverse-order (d :in n)
                                            (setq res (the fixnum (* res d))))
    res))

(defun contains-all-digits-at-least-once-p (n)
  "Return true if and only if fixnum N contains all digits 0...9 at least once."
  (declare (type fixnum n))
  (>= (length (the list (number->digits-set n))) 10))

(defun contains-all-digits-at-least-once-p--bigint (n)
  "Return true if and only if integer N contains all digits 0...9 at least once."
  (declare (type integer n))
  (>= (length (the list (number->digits-set--bigint n))) 10))

(defun replace-nth-digit (n i d &optional (nb-digits (nb-digits n)))
  "Replace the I-th digit (1-indexed) of fixnum N by digit D and return the result.
Optional NB-DIGITS avoids recalculating the number of digits."
  (declare (type fixnum n i d nb-digits))
  (if (= n 0)
      0
      (loop for x of-type fixnum = n then (the fixnum (floor x 10))
            for current-digit-index of-type fixnum from nb-digits downto 1
            for r of-type fixnum = (mod x 10)
            for r2 of-type fixnum = (if (= current-digit-index i)
                                        d
                                        r)
            for mult of-type fixnum = 1 then (the fixnum (* mult 10))
            for y of-type fixnum = r2 then (the fixnum (+ (the fixnum (* mult r2)) y))
            while (> x 0)
            finally (return y))))

(defun replace-digits (n mask d &optional (nb-digits (nb-digits n)))
  "Replace digits at positions specified in MASK with digit D.
N is the number, MASK is a fixnum vector of digit positions (1-indexed), D is the replacement digit."
  (declare (type fixnum n nb-digits)
           (type (simple-array fixnum) mask))
  (if (= n 0)
      0
      (loop for x of-type fixnum = n then (the fixnum (floor x 10))
            for current-digit-index of-type fixnum from nb-digits downto 1
            for r of-type fixnum = (mod x 10)
            for r2 of-type fixnum = (if (find current-digit-index mask)
                                        d
                                        r)
            for mult of-type fixnum = 1 then (the fixnum (* mult 10))
            for y of-type fixnum = r2 then (the fixnum (+ (the fixnum (* mult r2)) y))
            while (> x 0)
            finally (return y))))

(defun pandigitalp (n &key (from 1) (to 9))
  "Return t if and only if fixnum N is pandigital in the FROM-TO digit range.
For instance: 15234 :from 1 :to 5 --> t.
987654321 --> t (default range 1-9).
It is supposed that 0 <= FROM <= TO <= 9."
  (declare (type fixnum n from to))
  (let ((digits (number->digits n)))
    (declare (type list digits))
    (loop for d of-type fixnum from 0 to 9
          for tmp = (member d digits)
          always (if (<= from d to)
                     (and tmp (not (member d (cdr tmp))))
                     (not tmp)))))

(defun reverse-number--fixnum (n)
  "Reverse the N, which is supposed to be a fixnum >= 0.
For instance: 123 --> 321.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum n))
  (labels ((sub (num acc)
             (declare (type fixnum num acc))
             (if (= num 0)
                 acc
                 (multiple-value-bind (quotient remainder)
                     (floor num 10)
                   (declare (type fixnum quotient remainder))
                   (sub quotient (the fixnum (+ (the fixnum (* 10 acc)) remainder)))))))
    (sub n 0)))

(defun reverse-number--bigint (n)
  "Reverse integer N."
  (declare (type integer n))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (labels ((aux (n &optional (acc 0))
               (if (= n 0)
                   acc
                   (multiple-value-bind (f r) (floor n 10)
                     (aux f (+ (* 10 acc) r))))))
      (if (= n 0)
          0
          (aux n)))))


;;; ===
;;; =================
;;; === MULTIPLES ===
;;; =================
;;; ===

(declaim (ftype (function (fixnum fixnum) fixnum) sum-of-multiples-below))
(defun sum-of-multiples-below (n m)
  "Return the sum of multiples of fixnum N which are (strictly) below fixnum M, N included.

Use exact formula (1/2).n.E((m-1)/n).[ E((m-1)/n) + 1 ]."
  (declare (type fixnum n m))
  (let ((tmp (the fixnum (floor (the fixnum (- m 1)) n))))
    (declare (type fixnum tmp))
    (the fixnum (/ (the fixnum (* n (the fixnum (* tmp (the fixnum (+ tmp 1)))))) 2))))

(declaim (ftype (function (integer integer) integer) sum-of-multiples-below--bigint))
(defun sum-of-multiples-below--bigint (n m)
  "Return the sum of multiples of integer N which are (strictly) below integer M, N included.

Use exact formula (1/2).n.E((m-1)/n).[ E((m-1)/n) + 1 ]."
  (declare (type integer n m))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((tmp (floor (- m 1) n))
           (tmp2 (* n (* tmp (+ tmp 1)))))
      (declare (type integer tmp tmp2))
      (the integer (/ tmp2 2)))))


;;; ===
;;; ================
;;; === DIVISORS ===
;;; ================
;;; ===

(defun gcd--2fixnum (a b)
  "Calculate GCD of arguments, which are supposed to be two fixnums.
Requires SBCL."
  (declare (type fixnum a b))
  (cond ((eql a 0) (abs b))
        ((eql b 0) (abs a))
        (t (sb-kernel::fixnum-gcd a b))))

(defmacro is-divisible-by-m (n d)
  "Return true if and only if N is divisible by D."
  `(= 0 (mod ,n ,d)))

(defmacro divides-m (d n)
  "Return true if and only if D divides N."
  `(= 0 (mod ,n ,d)))

(defun coprimes-p (a b)
  "Return t if and only if fixnum A and B are coprimes.

Method: Euclid's algorithm.

Note: The numbers 1 and -1 are the only integers coprime with every integer, and they are the only integers that are coprime with 0. (Wikipedia)"
  (declare (type fixnum a b))
  (= 1 (gcd--2fixnum a b)))

(defmacro coprimes-p-m (a b)
  "Return t if and only if fixnum A and B are coprimes.

Method: Euclid's algorithm."
  `(= 1 (gcd--2fixnum ,a ,b)))


;;; ===
;;; ================
;;; === TOTIENT ===
;;; ================
;;; ===

(declaim (ftype (function (fixnum) fixnum) totient))
(defun totient (n)
  "Return the totient of fixnum N."
  (declare (type fixnum n))
  (let ((res 1))
    (declare (type fixnum res))
    (loop with already of-type boolean = nil
          do (multiple-value-bind (q r) (floor n 2)
               (declare (type fixnum q r))
               (if (zerop r)
                   (progn
                     (setf n q)
                     (if already
                         (setq res (the fixnum (* res 2)))
                         (setq already t)))
                   (return))))
    (loop for factor of-type fixnum from 3 by 2
          while (and (< 1 n) (<= (the fixnum (* factor factor)) n))
          do (loop with already of-type boolean = nil
                   do (multiple-value-bind (q r) (floor n factor)
                        (declare (type fixnum q r))
                        (if (zerop r)
                            (progn
                              (setf n q)
                              (if already
                                  (setq res (the fixnum (* res factor)))
                                  (setq res (the fixnum (* res (the fixnum (- factor 1))))
                                        already t)))
                            (return)))))
    (when (not (= 1 n)) (setq res (the fixnum (* res (the fixnum (- n 1))))))
    res))

(defun totients-from-1-to-n (n)
  "Return the vector of totient of 1...N as vector of length (N+1).

Source: https://cp-algorithms.com/algebra/phi-function.html#divsum"
  (declare (type fixnum n))
  (let ((phis (make-array (the fixnum (+ n 1)) :element-type 'fixnum)))
    (setf (aref phis 0) 0
          (aref phis 1) 1)
    (loop for i of-type fixnum from 2 to n
          do (setf (aref phis i) (the fixnum (- i 1))))
    (loop for i of-type fixnum from 2 to n
          do (loop for j of-type fixnum from (the fixnum (* i 2)) to n by i do
            (decf (aref phis j) (aref phis i))))
    phis))

(declaim (ftype (function (fixnum) fixnum) totient-summatory))
(defun totient-summatory (n)
  "Return the sum of totients of 1...N.

https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/"
  (declare (type fixnum n))
  (labels-memo-ht
   nil
   ((S (n)
       (declare (type fixnum n))
       (let ((isqrt-n (isqrt n))
             (abc 0)
             (res 0))
         (declare (type fixnum abc res isqrt-n))
         (loop for m of-type fixnum from 2 to isqrt-n
               do (incf res (the fixnum (S (the fixnum (floor n m))))))
         (loop for d of-type fixnum from 1 to isqrt-n
               for floor-n-d of-type fixnum = (the fixnum (floor n d))
               unless (= d floor-n-d)
                 do (incf res (the fixnum (* (the fixnum (S d))
                                             (the fixnum (- floor-n-d
                                                            (the fixnum (floor n (the fixnum (+ d 1))))))))))
         (setq abc (the fixnum (floor (the fixnum (* n (the fixnum (+ n 1)))) 2)))
         (setq res (the fixnum (- abc res)))
         res)))
   (S n)))


;;; ===
;;; ============================================
;;; === PERFECT SQUARES, CUBES, AND SPECIALS ===
;;; ============================================
;;; ===

(defun perfect-square-p (n)
  "Return t if and only if fixnum N is a perfect square."
  (declare (type fixnum n))
  (let ((isqrt (isqrt n)))
    (declare (type fixnum isqrt))
    (= n (the fixnum (* isqrt isqrt)))))

(defmacro perfect-square-p-m (n)
  "Return t if and only if N is a perfect square (macro version)."
  (with-gensyms (isqrt)
    `(let ((,isqrt (isqrt ,n)))
       (declare (type fixnum ,isqrt))
       (= ,n (the fixnum (* ,isqrt ,isqrt))))))

(defun perfect-square-p-specific (n)
  "Return sqrt(N) if the argument N is a perfect square, and nil otherwise.
For instance: 15 --> nil ; 16 --> 4."
  (declare (type fixnum n))
  (let ((isqrt (isqrt n)))
    (declare (type fixnum isqrt))
    (if (= n (the fixnum (* isqrt isqrt))) isqrt nil)))

(defun perfect-cube-p-specific (n)
  "Return the cubic root of fixnum N if the argument N is a perfect cube, and nil otherwise.
For instance: 15 --> nil ; 27 --> 3."
  (declare (type fixnum n))
  (if (= n 0)
      t
      (let* ((racine1 (expt (the (double-float 0.0d0) (* 1.0d0 n)) (/ 1.0d0 3.0d0)))
             (racine2 (the fixnum (round racine1))))
        (declare (type double-float racine1)
                 (type fixnum racine2))
        (if (= n (the fixnum (* racine2 racine2 racine2))) racine2 nil))))


;;; ===
;;; ===========================
;;; === NUMBERS AND STRINGS ===
;;; ===========================
;;; ===

(defun string-to-integer-list (str)
  "Convert a string to a list of integers.
For instance \"1 2 3\" --> '(1 2 3)."
  (loop for (integer position) = (multiple-value-list
                                  (parse-integer str
                                                 :start (or position 0)
                                                 :junk-allowed t))
        while integer
        collect integer))


;;; ===
;;; ===================
;;; === CONVERSIONS ===
;;; ===================
;;; ===

(defun integer->english (n)
  "Convert integer to English text (with 'and', British usage).
For instance: 123 --> \"one hundred and twenty-three\"."
  (declare (type fixnum n))
  (cond ((< n 100) (format nil "~R" n))
        ((= 0 (mod n 100)) (format nil "~R" n))
        (t (format nil "~R and ~R" (- n (mod n 100)) (mod n 100)))))

(defun %roman-character->integer (roman-character)
  "Convert a roman numeral character to its integer value.
For instance: #\\M --> 1000."
  (declare (type character roman-character))
  (ccase roman-character
    ((#\I #\i) 1)
    ((#\V #\v) 5)
    ((#\X #\x) 10)
    ((#\L #\l) 50)
    ((#\C #\c) 100)
    ((#\D #\d) 500)
    ((#\M #\m) 1000)))

(defun roman->integer (roman-numeral)
  "Convert a roman number (as a string) to an integer.
For instance \"MC\" --> 1100."
  (declare (type simple-string roman-numeral))
  (let ((nb-char (length roman-numeral))
        (sum 0))
    (declare (type fixnum nb-char sum))
    (loop for i of-type fixnum from 0 below nb-char do
      (if (= i (the fixnum (- nb-char 1)))
          (let ((A (%roman-character->integer (aref roman-numeral i))))
            (declare (type fixnum A))
            (incf sum A))
          (let ((A (%roman-character->integer (aref roman-numeral i)))
                (B (%roman-character->integer (aref roman-numeral (the fixnum (+ i 1))))))
            (declare (type fixnum A B))
            (incf sum (if (< A B) (- A) A)))))
    sum))

(defun integer->roman (n)
  "Convert an integer into a roman representation (as a string).
For instance: 1100 --> \"MC\".
The argument is supposed to be an integer comprised between 1 and 4999, both included."
  (declare (type fixnum n))
  (cond ((> n 4000) (coerce (format nil "MMMM~@r" (- n 4000)) '(simple-array character (*))))
        ((= n 4000) "MMMM")
        (t (coerce (format nil "~@r" n) '(simple-array character (*))))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-numbers-integers ()
  "Demonstrate integer utilities."
  (format t "~%~%======~%=== NUMBERS-INTEGERS~%======~%")
  (format t "~%")
  (format t "--- reverse-number --~%")
  (format t "123456 --> ~a~%" (reverse-number--fixnum 123456))
  (format t "~%--- power ---~%")
  (format t "2^10 = ~a~%" (power 2 10))
  (format t "~%--- floor/ceiling to power of 10 ---~%")
  (format t "floor-to-power-of-10(123) = ~a~%" (floor-to-power-of-10 123))
  (format t "ceiling-to-power-of-10(123) = ~a~%" (ceiling-to-power-of-10 123))
  (format t "~%--- fibonacci ---~%")
  (format t "fib(10) = ~a (iteration)~%" (fibonacci-through-iteration 10))
  (format t "fib(10) = ~a (formula)~%" (fibonacci-through-explicit-formula 10))
  (format t "~%--- digits ---~%")
  (format t "number->digits(123) = ~a~%" (number->digits 123))
  (format t "sum-of-digits(123) = ~a~%" (sum-of-digits 123))
  (format t "nb-digits(999) = ~a~%" (nb-digits 999))
  (format t "~%--- combinatorics ---~%")
  (format t "5! = ~a~%" (fact 5))
  (format t "C(10,3) = ~a~%" (combin 10 3))
  (format t "~%--- totient ---~%")
  (format t "totient(12) = ~a~%" (totient 12))
  (format t "~%--- conversions ---~%")
  (format t "roman->integer(\"MCMXCIX\") = ~a~%" (roman->integer "MCMXCIX"))
  (format t "integer->roman(1999) = ~a~%" (integer->roman 1999))
  (format t "~%"))

;; end
