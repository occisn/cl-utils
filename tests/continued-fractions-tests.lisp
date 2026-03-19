(in-package :cl-utils-tests)

;;; === integer-sqrt-to-continued-fraction

(parachute:define-test integer-sqrt-to-continued-fraction
  (parachute:is equal '(1 (2)) (integer-sqrt-to-continued-fraction 2))
  (parachute:is equal '(1 (1 2)) (integer-sqrt-to-continued-fraction 3))
  (parachute:is equal '(2 (4)) (integer-sqrt-to-continued-fraction 5))
  (parachute:is equal '(2 (2 4)) (integer-sqrt-to-continued-fraction 6))
  (parachute:is equal '(2 (1 1 1 4)) (integer-sqrt-to-continued-fraction 7))
  (parachute:is equal '(2 (1 4)) (integer-sqrt-to-continued-fraction 8))
  (parachute:is equal '(3 (6)) (integer-sqrt-to-continued-fraction 10))
  (parachute:is equal '(3 (3 6)) (integer-sqrt-to-continued-fraction 11))
  (parachute:is equal '(3 (2 6)) (integer-sqrt-to-continued-fraction 12))
  (parachute:is equal '(3 (1 1 1 1 6)) (integer-sqrt-to-continued-fraction 13)))

;;; === length-continued-fraction-isqrt

(parachute:define-test length-continued-fraction-isqrt
  (parachute:is = 1 (length-continued-fraction-isqrt 2))
  (parachute:is = 2 (length-continued-fraction-isqrt 3))
  (parachute:is = 1 (length-continued-fraction-isqrt 5))
  (parachute:is = 2 (length-continued-fraction-isqrt 6))
  (parachute:is = 4 (length-continued-fraction-isqrt 7))
  (parachute:is = 2 (length-continued-fraction-isqrt 8))
  (parachute:is = 1 (length-continued-fraction-isqrt 10))
  (parachute:is = 2 (length-continued-fraction-isqrt 11))
  (parachute:is = 2 (length-continued-fraction-isqrt 12))
  (parachute:is = 5 (length-continued-fraction-isqrt 13)))

;;; === with-successive-convergents

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parachute:define-test with-successive-convergents
    (parachute:is equal
                  '((1 1) (3 2) (7 5) (17 12) (41 29) (99 70) (239 169) (577 408) (1393 985) (3363 2378))
                  (with-collector (collect)
                    (block outer
                      (with-successive-convergents (i num denom :of (integer-sqrt-to-continued-fraction 2))
                        (collect (list num denom))
                        (when (>= i 10) (return-from outer))))))
    (parachute:is equal
                  '((2 1) (3 1) (8 3) (11 4) (19 7) (87 32) (106 39) (193 71) (1264 465) (1457 536))
                  (with-collector (collect)
                    (block outer
                      (with-successive-convergents (i num denom :of '(2 (1 2 1 1 4 1 1 6 1 1 8 1 1 10)))
                        (collect (list num denom))
                        (when (>= i 10) (return-from outer))))))))

;;; === solve-pell-equation

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parachute:define-test solve-pell-equation
    (parachute:is-values (solve-pell-equation 2)
      (= 3)
      (= 2))
    (parachute:is-values (solve-pell-equation 3)
      (= 2)
      (= 1))
    (parachute:is-values (solve-pell-equation 5)
      (= 9)
      (= 4))
    (parachute:is-values (solve-pell-equation 6)
      (= 5)
      (= 2))
    (parachute:is-values (solve-pell-equation 7)
      (= 8)
      (= 3))))

;;; === end
