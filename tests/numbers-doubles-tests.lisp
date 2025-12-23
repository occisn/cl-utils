(in-package :cl-utils-tests)

(parachute:define-test test-floor-of-positive-df

  (parachute:is = 0 (floor-of-positive-df 0.0d0))
  (parachute:is = 1 (floor-of-positive-df 1.0d0))
  (parachute:is = 1 (floor-of-positive-df 1.9d0))
  (parachute:is = 42 (floor-of-positive-df 42.1d0))
  (parachute:is = 42 (floor-of-positive-df 42.999999d0))

  (loop for x in '(0.1d0 0.5d0 0.9d0)
        do (parachute:true (= 0 (floor-of-positive-df x))))

  (loop for x in '(10.1d0 10.5d0 10.9d0)
        do (parachute:true (= 10 (floor-of-positive-df x)))))

;; end
