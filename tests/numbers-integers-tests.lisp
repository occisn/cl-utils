(in-package :cl-utils-tests)

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
