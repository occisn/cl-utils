;;;; Tests for the numbers integers utilities.

(in-package :cl-utils-tests)

;;; === power

(parachute:define-test power
  (parachute:is = 1 (power 0 0))
  (parachute:is = 1 (power 2 0))
  (parachute:is = 0 (power 0 3))
  (parachute:is = 8 (power 2 3))
  (parachute:is = 81 (power 3 4)))

;;; === floor-to-power-of-10

(parachute:define-test floor-to-power-of-10
  (parachute:is = 1 (floor-to-power-of-10 1))
  (parachute:is = 1 (floor-to-power-of-10 9))
  (parachute:is = 10 (floor-to-power-of-10 10))
  (parachute:is = 10 (floor-to-power-of-10 99))
  (parachute:is = 100 (floor-to-power-of-10 100))
  (parachute:is = 100 (floor-to-power-of-10 999))
  (parachute:is = 1000 (floor-to-power-of-10 1000))
  (parachute:is = 100000000 (floor-to-power-of-10 123456789)))

;;; === fibonacci

(parachute:define-test fibonacci
  (parachute:is = 0 (fibonacci-through-iteration 0))
  (parachute:is = 1 (fibonacci-through-iteration 1))
  (parachute:is = 1 (fibonacci-through-iteration 2))
  (parachute:is = 55 (fibonacci-through-iteration 10))
  (parachute:is = 55 (fibonacci-through-explicit-formula 10)))

;;; === digits

(parachute:define-test number->digits
  (parachute:is equal '(0) (number->digits 0))
  (parachute:is equal '(1 2 3) (number->digits 123))
  (parachute:is equal '(5 6 7 8 9) (number->digits 56789)))

(parachute:define-test nb-digits
  (parachute:is = 1 (nb-digits 0))
  (parachute:is = 1 (nb-digits 9))
  (parachute:is = 3 (nb-digits 999))
  (parachute:is = 9 (nb-digits 123456789)))

(parachute:define-test sum-of-digits
  (parachute:is = 6 (sum-of-digits 123))
  (parachute:is = 45 (sum-of-digits 123456789)))

(parachute:define-test product-of-digits
  (parachute:is = 24 (product-of-digits 1234))
  (parachute:is = 0 (product-of-digits 102)))

(parachute:define-test list-of-digits->number-test
  (parachute:is = 123 (list-of-digits->number '(1 2 3)))
  (parachute:is = 0 (list-of-digits->number '(0))))

(parachute:define-test number->sorted-digits-test
  (parachute:is equal '(3 4 5 6) (number->sorted-digits 5643)))

;;; === combinatorics

(parachute:define-test fact-test
  (parachute:is = 1 (fact 0))
  (parachute:is = 1 (fact 1))
  (parachute:is = 120 (fact 5))
  (parachute:is = 3628800 (fact 10)))

(parachute:define-test combin-test
  (parachute:is = 1 (combin 5 0))
  (parachute:is = 5 (combin 5 1))
  (parachute:is = 10 (combin 5 2))
  (parachute:is = 10 (combin 5 3))
  (parachute:is = 252 (combin 10 5)))

;;; === totient

(parachute:define-test totient-test
  (parachute:is = 1 (totient 1))
  (parachute:is = 1 (totient 2))
  (parachute:is = 4 (totient 12))
  (parachute:is = 40 (totient 100)))

;;; === perfect square

(parachute:define-test perfect-square-p-test
  (parachute:true (perfect-square-p 0))
  (parachute:true (perfect-square-p 1))
  (parachute:true (perfect-square-p 4))
  (parachute:true (perfect-square-p 16))
  (parachute:false (perfect-square-p 15)))

;;; === roman conversions

(parachute:define-test roman->integer-test
  (parachute:is = 1999 (roman->integer "MCMXCIX"))
  (parachute:is = 1100 (roman->integer "MC"))
  (parachute:is = 4 (roman->integer "IV")))

(parachute:define-test integer->roman-test
  (parachute:is string= "IV" (integer->roman 4))
  (parachute:is string= "MCMXCIX" (integer->roman 1999)))

;;; === ceiling-to-power-of-10

(parachute:define-test ceiling-to-power-of-10-test
  (parachute:is = 10 (ceiling-to-power-of-10 1))
  (parachute:is = 10 (ceiling-to-power-of-10 9))
  (parachute:is = 100 (ceiling-to-power-of-10 10))
  (parachute:is = 100 (ceiling-to-power-of-10 99))
  (parachute:is = 1000 (ceiling-to-power-of-10 100))
  (parachute:is = 10000 (ceiling-to-power-of-10 1000))
  (parachute:is = 1000000000 (ceiling-to-power-of-10 123456789)))

;;; === concatenate-numbers

(parachute:define-test concatenate-numbers-test
  (parachute:is = 415 (concatenate-numbers 41 5))
  (parachute:is = 123 (concatenate-numbers 0 123))
  (parachute:is = 1234 (concatenate-numbers 12 34))
  (parachute:is = 10 (concatenate-numbers 1 0))
  (parachute:is = 99100 (concatenate-numbers 99 100)))

;;; === first-digit

(parachute:define-test first-digit-test
  (parachute:is = 1 (first-digit 1))
  (parachute:is = 2 (first-digit 29))
  (parachute:is = 5 (first-digit 56789))
  (parachute:is = 1 (first-digit 100))
  (parachute:is = 9 (first-digit 9999)))

;;; === last-digit

(parachute:define-test last-digit-test
  (parachute:is = 0 (last-digit 10))
  (parachute:is = 9 (last-digit 9))
  (parachute:is = 3 (last-digit 123))
  (parachute:is = 1 (last-digit 1))
  (parachute:is = 0 (last-digit 1000)))

;;; === nth-digit

(parachute:define-test nth-digit-test
  (parachute:is = 5 (nth-digit 56789 1))
  (parachute:is = 7 (nth-digit 56789 3))
  (parachute:is = 9 (nth-digit 56789 5))
  (parachute:is = 1 (nth-digit 123 1))
  (parachute:is = 3 (nth-digit 123 3)))

;;; === char->digit

(parachute:define-test char->digit-test
  (parachute:is = 0 (char->digit #\0))
  (parachute:is = 5 (char->digit #\5))
  (parachute:is = 9 (char->digit #\9)))

;;; === contains-all-digits-at-least-once-p

(parachute:define-test contains-all-digits-at-least-once-p-test
  (parachute:true (contains-all-digits-at-least-once-p 1234567890))
  (parachute:false (contains-all-digits-at-least-once-p 123456789))
  (parachute:true (contains-all-digits-at-least-once-p 10234567891)))

;;; === pandigitalp

(parachute:define-test pandigitalp-test
  (parachute:true (pandigitalp 987654321))
  (parachute:true (pandigitalp 123456789))
  (parachute:false (pandigitalp 12345678))
  (parachute:true (pandigitalp 15234 :from 1 :to 5))
  (parachute:false (pandigitalp 1123 :from 1 :to 3)))

;;; === coprimes-p

(parachute:define-test coprimes-p-test
  (parachute:true (coprimes-p 1 1))
  (parachute:true (coprimes-p 3 7))
  (parachute:false (coprimes-p 4 6))
  (parachute:true (coprimes-p 13 17))
  (parachute:false (coprimes-p 12 18)))

;;; === gcd--2fixnum

(parachute:define-test gcd--2fixnum-test
  (parachute:is = 6 (gcd--2fixnum 12 18))
  (parachute:is = 1 (gcd--2fixnum 7 13))
  (parachute:is = 5 (gcd--2fixnum 0 5))
  (parachute:is = 3 (gcd--2fixnum 3 0))
  (parachute:is = 1 (gcd--2fixnum 1 100)))

;;; === iota

(parachute:define-test iota-test
  (parachute:is equal '(1 2 3 4 5) (iota 5))
  (parachute:is equal '(3 4 5) (iota 5 :from 3))
  (parachute:is equal '(1) (iota 1))
  (parachute:is equal nil (iota 0)))

;;; === make-list-within-range

(parachute:define-test make-list-within-range-test
  (parachute:is equal '(1 2 3) (make-list-within-range 1 3))
  (parachute:is equal '(5) (make-list-within-range 5 5))
  (parachute:is equal '(-2 -1 0 1 2) (make-list-within-range -2 2)))

;;; === non-decreasing-p

(parachute:define-test non-decreasing-p-test
  (parachute:true (non-decreasing-p '(1 2 3 4 5)))
  (parachute:true (non-decreasing-p '(1 1 2 2 3)))
  (parachute:false (non-decreasing-p '(1 3 2)))
  (parachute:true (non-decreasing-p '(5)))
  (parachute:true (non-decreasing-p nil)))

;;; === group-factors

(parachute:define-test group-factors-test
  (parachute:is equal '((2 . 3) (3 . 1)) (group-factors '(2 2 2 3)))
  (parachute:is equal '((5 . 2)) (group-factors '(5 5)))
  (parachute:is equal '((2 . 1) (3 . 1) (5 . 1)) (group-factors '(2 3 5))))

;;; === mod-expt--fixnum

(parachute:define-test mod-expt--fixnum-test
  (parachute:is = (mod (expt 2 10) 1000) (mod-expt--fixnum 2 10 1000))
  (parachute:is = (mod (expt 3 7) 13) (mod-expt--fixnum 3 7 13))
  (parachute:is = (mod (expt 7 100) 97) (mod-expt--fixnum 7 100 97))
  (parachute:is = 0 (mod-expt--fixnum 10 5 10)))

;;; === sum-of-multiples-below

(parachute:define-test sum-of-multiples-below-test
  (parachute:is = 18 (sum-of-multiples-below 3 10))
  (parachute:is = 735 (sum-of-multiples-below 7 100))
  (parachute:is = 0 (sum-of-multiples-below 5 5)))

;;; === totients-from-1-to-n

(parachute:define-test totients-from-1-to-n-test
  (let ((phis (totients-from-1-to-n 10)))
    (parachute:is = 1 (aref phis 1))
    (parachute:is = 1 (aref phis 2))
    (parachute:is = 2 (aref phis 3))
    (parachute:is = 2 (aref phis 4))
    (parachute:is = 4 (aref phis 5))
    (parachute:is = 4 (aref phis 10))
    ;; Cross-check with scalar totient
    (loop for i from 1 to 10
          do (parachute:is = (totient i) (aref phis i)))))

;;; === integer->english

(parachute:define-test integer->english-test
  (parachute:is string= "one" (integer->english 1))
  (parachute:is string= "one hundred" (integer->english 100))
  (parachute:is string= "one hundred and twenty-three" (integer->english 123))
  (parachute:is string= "forty-two" (integer->english 42)))

;;; === string-to-integer-list

(parachute:define-test string-to-integer-list-test
  (parachute:is equal '(1 2 3) (string-to-integer-list "1 2 3"))
  (parachute:is equal '(42) (string-to-integer-list "42"))
  (parachute:is equal nil (string-to-integer-list "abc")))

;;; === fact-0-9

(parachute:define-test fact-0-9-test
  (parachute:is = 1 (fact-0-9 0))
  (parachute:is = 1 (fact-0-9 1))
  (parachute:is = 120 (fact-0-9 5))
  (parachute:is = 362880 (fact-0-9 9)))

;;; === replace-nth-digit

(parachute:define-test replace-nth-digit-test
  (parachute:is = 193 (replace-nth-digit 123 2 9))
  (parachute:is = 923 (replace-nth-digit 123 1 9))
  (parachute:is = 129 (replace-nth-digit 123 3 9)))

;;; === perfect-square-p-specific

(parachute:define-test perfect-square-p-specific-test
  (parachute:is = 4 (perfect-square-p-specific 16))
  (parachute:is = 1 (perfect-square-p-specific 1))
  (parachute:is = 0 (perfect-square-p-specific 0))
  (parachute:false (perfect-square-p-specific 15)))

;;; === perfect-cube-p-specific

(parachute:define-test perfect-cube-p-specific-test
  (parachute:is = 3 (perfect-cube-p-specific 27))
  (parachute:is = 1 (perfect-cube-p-specific 1))
  (parachute:true (perfect-cube-p-specific 0))
  (parachute:false (perfect-cube-p-specific 15)))

;;; === reverse-number--bigint

(parachute:define-test reverse-number--bigint-test
  (parachute:is = 0 (reverse-number--bigint 0))
  (parachute:is = 321 (reverse-number--bigint 123))
  (parachute:is = 1 (reverse-number--bigint 100))
  ;; Cross-check with fixnum version
  (parachute:is = (reverse-number--fixnum 123456789) (reverse-number--bigint 123456789)))

;;; === number->vector-of-digits / vector-of-digits->number

(parachute:define-test vector-of-digits-roundtrip-test
  (parachute:is = 123 (vector-of-digits->number (number->vector-of-digits 123)))
  (parachute:is = 56789 (vector-of-digits->number (number->vector-of-digits 56789))))

;;; === reverse-number--fixnum

(parachute:define-test reverse-number--fixnum-tests

  ;; Zero
  (parachute:is = 0 (reverse-number--fixnum 0))

  ;; Single digits
  (parachute:is = 1 (reverse-number--fixnum 1))
  (parachute:is = 5 (reverse-number--fixnum 5))
  (parachute:is = 9 (reverse-number--fixnum 9))

  ;; Two digits
  (parachute:is = 21 (reverse-number--fixnum 12))
  (parachute:is = 54 (reverse-number--fixnum 45))
  (parachute:is = 99 (reverse-number--fixnum 99))

  ;; Three digits
  (parachute:is = 321 (reverse-number--fixnum 123))
  (parachute:is = 654 (reverse-number--fixnum 456))
  (parachute:is = 1 (reverse-number--fixnum 100))    ; Trailing zeros
  (parachute:is = 21 (reverse-number--fixnum 120))   ; Trailing zeros

  ;; Four digits
  (parachute:is = 4321 (reverse-number--fixnum 1234))
  (parachute:is = 9876 (reverse-number--fixnum 6789))

  ;; Numbers with trailing zeros (leading zeros disappear)
  (parachute:is = 1 (reverse-number--fixnum 1000))
  (parachute:is = 54 (reverse-number--fixnum 4500))
  (parachute:is = 321 (reverse-number--fixnum 12300))

  ;; Palindromes
  (parachute:is = 121 (reverse-number--fixnum 121))
  (parachute:is = 1221 (reverse-number--fixnum 1221))

  ;; Larger numbers (but within fixnum range)
  (parachute:is = 7654321 (reverse-number--fixnum 1234567))
  (parachute:is = 987654321 (reverse-number--fixnum 123456789))

  ;; Edge case: all same digits
  (parachute:is = 111 (reverse-number--fixnum 111))
  (parachute:is = 9999 (reverse-number--fixnum 9999))

  ;; Type checking
  (parachute:of-type fixnum (reverse-number--fixnum 123)))

;; end
