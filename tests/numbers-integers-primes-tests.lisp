(in-package :cl-utils-tests)

(parachute:define-test test-largest-prime-factor
  (parachute:is = (largest-prime-factor 13195) 29)
  (parachute:is = (largest-prime-factor 600851475143) 6857)
  )

;;; end
