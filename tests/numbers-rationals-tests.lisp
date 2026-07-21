;;;; Tests for the numbers rationals utilities.

(in-package :cl-utils-tests)

(parachute:define-test convert-to-proper-fraction-m-tests
  
  ;; Test basic fraction reduction (50/15 -> 10/3)
  (parachute:define-test reduce-basic-fraction
    (let ((a 50) (b 15))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 10 a)
      (parachute:is = 3 b)))
  
  ;; Test already proper fraction (no change needed)
  (parachute:define-test already-proper-fraction
    (let ((a 3) (b 5))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 3 a)
      (parachute:is = 5 b)))
  
  ;; Test GCD of 1 (coprime numbers)
  (parachute:define-test coprime-numbers
    (let ((a 7) (b 11))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 7 a)
      (parachute:is = 11 b)))
  
  ;; Test simple reduction (10/5 -> 2/1)
  (parachute:define-test simple-reduction
    (let ((a 10) (b 5))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 2 a)
      (parachute:is = 1 b)))
  
  ;; Test larger GCD (24/36 -> 2/3)
  (parachute:define-test larger-gcd
    (let ((a 24) (b 36))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 2 a)
      (parachute:is = 3 b)))
    
  ;; Test equal numbers (reduces to 1/1)
  (parachute:define-test equal-numbers
    (let ((a 12) (b 12))
      (declare (type fixnum a b))
      (convert-to-proper-fraction-m a b)
      (parachute:is = 1 a)
      (parachute:is = 1 b))))

(parachute:define-test length-of-recurring-cycle-tests
  
  ;; Test 1/1 = 1.0 (no recurring cycle)
  (parachute:define-test denom-1
    (parachute:is = 0 (length-of-recurring-cycle 1)))
  
  ;; Test 1/2 = 0.5 (terminates, no cycle)
  (parachute:define-test denom-2
    (parachute:is = 0 (length-of-recurring-cycle 2)))
  
  ;; Test 1/3 = 0.333... (recurring 3)
  (parachute:define-test denom-3
    (parachute:is = 1 (length-of-recurring-cycle 3)))
  
  ;; Test 1/4 = 0.25 (terminates, no cycle)
  (parachute:define-test denom-4
    (parachute:is = 0 (length-of-recurring-cycle 4)))
  
  ;; Test 1/5 = 0.2 (terminates, no cycle)
  (parachute:define-test denom-5
    (parachute:is = 0 (length-of-recurring-cycle 5)))
  
  ;; Test 1/6 = 0.1666... (recurring 6)
  (parachute:define-test denom-6
    (parachute:is = 1 (length-of-recurring-cycle 6)))
  
  ;; Test 1/7 = 0.142857142857... (recurring cycle of 6 digits)
  (parachute:define-test denom-7
    (parachute:is = 6 (length-of-recurring-cycle 7)))
  
  ;; Test 1/8 = 0.125 (terminates, no cycle)
  (parachute:define-test denom-8
    (parachute:is = 0 (length-of-recurring-cycle 8)))
  
  ;; Test 1/9 = 0.111... (recurring 1)
  (parachute:define-test denom-9
    (parachute:is = 1 (length-of-recurring-cycle 9)))
  
  ;; Test 1/10 = 0.1 (terminates, no cycle)
  (parachute:define-test denom-10
    (parachute:is = 0 (length-of-recurring-cycle 10)))
  
  ;; Test 1/11 = 0.090909... (recurring cycle of 2 digits)
  (parachute:define-test denom-11
    (parachute:is = 2 (length-of-recurring-cycle 11)))
  
  ;; Test 1/12 = 0.08333... (recurring 3)
  (parachute:define-test denom-12
    (parachute:is = 1 (length-of-recurring-cycle 12)))
  
  ;; Test 1/13 = 0.076923076923... (recurring cycle of 6 digits)
  (parachute:define-test denom-13
    (parachute:is = 6 (length-of-recurring-cycle 13)))
  
  ;; Test powers of 2 (should all terminate)
  (parachute:define-test powers-of-2
    (parachute:is = 0 (length-of-recurring-cycle 16))
    (parachute:is = 0 (length-of-recurring-cycle 32))
    (parachute:is = 0 (length-of-recurring-cycle 64)))
  
  ;; Test powers of 5 (should all terminate)
  (parachute:define-test powers-of-5
    (parachute:is = 0 (length-of-recurring-cycle 25))
    (parachute:is = 0 (length-of-recurring-cycle 125)))
  
  ;; Test mixed powers of 2 and 5 (should all terminate)
  (parachute:define-test powers-of-2-and-5
    (parachute:is = 0 (length-of-recurring-cycle 20))  ; 2^2 * 5
    (parachute:is = 0 (length-of-recurring-cycle 50))  ; 2 * 5^2
    (parachute:is = 0 (length-of-recurring-cycle 100)))) ; 2^2 * 5^2

;;; end
