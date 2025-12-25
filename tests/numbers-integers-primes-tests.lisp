(in-package :cl-utils-tests)

(parachute:define-test test-largest-prime-factor
  (parachute:is = (largest-prime-factor 13195) 29)
  (parachute:is = (largest-prime-factor 600851475143) 6857))

(parachute:define-test primep-tests
  
  ;; Edge cases: numbers <= 1
  (parachute:false (primep -5))
  (parachute:false (primep -1))
  (parachute:false (primep 0))
  (parachute:false (primep 1))
  
  ;; Small primes (explicitly handled in function)
  (parachute:true (primep 2))
  (parachute:true (primep 3))
  (parachute:true (primep 5))
  (parachute:true (primep 7))
  
  ;; Small composites
  (parachute:false (primep 4))
  (parachute:false (primep 6))
  (parachute:false (primep 8))
  (parachute:false (primep 9))
  (parachute:false (primep 10))
  (parachute:false (primep 12))
  (parachute:false (primep 14))
  (parachute:false (primep 15))
  (parachute:false (primep 16))
  (parachute:false (primep 18))
  (parachute:false (primep 20))
  (parachute:false (primep 21))
  (parachute:false (primep 22))
  (parachute:false (primep 24))
  (parachute:false (primep 25))
  (parachute:false (primep 26))
  (parachute:false (primep 27))
  (parachute:false (primep 28))
  (parachute:false (primep 30))
  
  ;; Primes in range 11-100
  (parachute:true (primep 11))
  (parachute:true (primep 13))
  (parachute:true (primep 17))
  (parachute:true (primep 19))
  (parachute:true (primep 23))
  (parachute:true (primep 29))
  (parachute:true (primep 31))
  (parachute:true (primep 37))
  (parachute:true (primep 41))
  (parachute:true (primep 43))
  (parachute:true (primep 47))
  (parachute:true (primep 53))
  (parachute:true (primep 59))
  (parachute:true (primep 61))
  (parachute:true (primep 67))
  (parachute:true (primep 71))
  (parachute:true (primep 73))
  (parachute:true (primep 79))
  (parachute:true (primep 83))
  (parachute:true (primep 89))
  (parachute:true (primep 97))
  
  ;; Perfect squares of primes
  (parachute:false (primep 49))   ; 7²
  (parachute:false (primep 121))  ; 11²
  (parachute:false (primep 169))  ; 13²
  (parachute:false (primep 289))  ; 17²
  
  ;; Products of two distinct primes
  (parachute:false (primep 35))   ; 5*7
  (parachute:false (primep 55))   ; 5*11
  (parachute:false (primep 77))   ; 7*11
  (parachute:false (primep 91))   ; 7*13
  (parachute:false (primep 143))  ; 11*13
  (parachute:false (primep 221))  ; 13*17
  (parachute:false (primep 247))  ; 13*19
  (parachute:false (primep 253))  ; 11*23
  
  ;; Primes in range 101-300
  (parachute:true (primep 101))
  (parachute:true (primep 103))
  (parachute:true (primep 107))
  (parachute:true (primep 109))
  (parachute:true (primep 113))
  (parachute:true (primep 127))
  (parachute:true (primep 131))
  (parachute:true (primep 139))
  (parachute:true (primep 149))
  (parachute:true (primep 151))
  (parachute:true (primep 157))
  (parachute:true (primep 163))
  (parachute:true (primep 167))
  (parachute:true (primep 173))
  (parachute:true (primep 179))
  (parachute:true (primep 181))
  (parachute:true (primep 191))
  (parachute:true (primep 193))
  (parachute:true (primep 197))
  (parachute:true (primep 199))
  (parachute:true (primep 211))
  (parachute:true (primep 223))
  (parachute:true (primep 227))
  (parachute:true (primep 229))
  (parachute:true (primep 233))
  (parachute:true (primep 239))
  (parachute:true (primep 241))
  (parachute:true (primep 251))
  (parachute:true (primep 257))
  (parachute:true (primep 263))
  (parachute:true (primep 269))
  (parachute:true (primep 271))
  (parachute:true (primep 277))
  (parachute:true (primep 281))
  (parachute:true (primep 283))
  (parachute:true (primep 293))
  
  ;; Larger composites
  (parachute:false (primep 100))
  (parachute:false (primep 111))
  (parachute:false (primep 123))
  (parachute:false (primep 133))
  (parachute:false (primep 155))
  (parachute:false (primep 161))
  (parachute:false (primep 187))
  (parachute:false (primep 195))
  (parachute:false (primep 299))  ; 13*23
  
  ;; Large primes (4-digit)
  (parachute:true (primep 1009))
  (parachute:true (primep 1013))
  (parachute:true (primep 1019))
  (parachute:true (primep 1021))
  (parachute:true (primep 1031))
  (parachute:true (primep 9973))
  
  ;; Large composites (4-digit)
  (parachute:false (primep 1000))
  (parachute:false (primep 1001))  ; 7*11*13
  (parachute:false (primep 1023))  ; 3*11*31
  (parachute:false (primep 9999))) ; 3²*11*101

(parachute:define-test next-prime-tests
  
  ;; Test edge cases: n <= 1 should return 2
  (parachute:is = 2 (next-prime -5))
  (parachute:is = 2 (next-prime 0))
  (parachute:is = 2 (next-prime 1))
  
  ;; Test n = 2 (first prime)
  (parachute:is = 3 (next-prime 2))
  
  ;; Test small primes returning next prime
  (parachute:is = 5 (next-prime 3))
  (parachute:is = 7 (next-prime 5))
  (parachute:is = 11 (next-prime 7))
  (parachute:is = 13 (next-prime 11))
  (parachute:is = 17 (next-prime 13))
  (parachute:is = 19 (next-prime 17))
  (parachute:is = 23 (next-prime 19))
  (parachute:is = 29 (next-prime 23))
  
  ;; Test even numbers
  (parachute:is = 5 (next-prime 4))
  (parachute:is = 7 (next-prime 6))
  (parachute:is = 11 (next-prime 8))
  (parachute:is = 11 (next-prime 10))
  (parachute:is = 13 (next-prime 12))
  (parachute:is = 101 (next-prime 100))
  
  ;; Test composite odd numbers
  (parachute:is = 11 (next-prime 9))   
  (parachute:is = 17 (next-prime 15))  
  (parachute:is = 23 (next-prime 21))  
  (parachute:is = 29 (next-prime 25))  
  (parachute:is = 29 (next-prime 27))  
  
  ;; Test numbers right before a prime
  (parachute:is = 97 (next-prime 96))
  (parachute:is = 101 (next-prime 99))
  
  ;; Test larger primes
  (parachute:is = 103 (next-prime 101))
  (parachute:is = 109 (next-prime 107))
  (parachute:is = 113 (next-prime 109))
  
  ;; Test gaps between primes
  (parachute:is = 127 (next-prime 113)) ; Large gap
  (parachute:is = 131 (next-prime 127))
  
  ;; Verify that when n is prime, result is NOT n
  (parachute:is /= 2 (next-prime 2))
  (parachute:is /= 3 (next-prime 3))
  (parachute:is /= 5 (next-prime 5))
  (parachute:is /= 97 (next-prime 97)))

(parachute:define-test nth-prime-test
  
  ;; Test first few primes
  (parachute:is = 2 (nth-prime 1))
  (parachute:is = 3 (nth-prime 2))
  (parachute:is = 5 (nth-prime 3))
  (parachute:is = 7 (nth-prime 4))
  (parachute:is = 11 (nth-prime 5))
  (parachute:is = 13 (nth-prime 6))
  
  ;; Test some additional known primes
  (parachute:is = 17 (nth-prime 7))
  (parachute:is = 19 (nth-prime 8))
  (parachute:is = 23 (nth-prime 9))
  (parachute:is = 29 (nth-prime 10))
  
  ;; Test larger indices
  (parachute:is = 53 (nth-prime 16))
  (parachute:is = 67 (nth-prime 19))
  (parachute:is = 97 (nth-prime 25))
  (parachute:is = 541 (nth-prime 100))
  (parachute:is = 997 (nth-prime 168))
  
  ;; Test that result is always a prime
  (parachute:true (primep (nth-prime 50)))
  (parachute:true (primep (nth-prime 75)))
  
  ;; Test that results are in increasing order
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (parachute:true (< (nth-prime 10) (nth-prime 11)))
    (parachute:true (< (nth-prime 20) (nth-prime 21)))
    (parachute:true (< (nth-prime 50) (nth-prime 51)))))

;;; end
