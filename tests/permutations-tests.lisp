;;;; Tests for the permutations utilities.

(in-package :cl-utils-tests)

(parachute:define-test next-distinct-lexicographic-permutation--string-test
  (parachute:is = 120
                (let ((sum 0))
                  (declare (type fixnum sum))
                  (loop for a = "12345" then (next-distinct-lexicographic-permutation--string a #'char<)
                        while a
                        do (incf sum))
                  sum)))

(parachute:define-test next-distinct-lexicographic-permutation--fixnum-vector-test
  (parachute:is = 120
                (let ((sum 0))
                  (declare (type fixnum sum))
                  (loop for a = (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5)) then (next-distinct-lexicographic-permutation--fixnum-vector a #'<)
                        while a
                        do (incf sum))
                  sum)))

(parachute:define-test list-of-distinct-rotated-numbers-test
  (parachute:is equal '(0) (list-of-distinct-rotated-numbers 0))
  (parachute:is equal '(1) (list-of-distinct-rotated-numbers 1))
  (parachute:is equal '(5) (list-of-distinct-rotated-numbers 5))
  (parachute:is equal '(1 10 100) (sort (the list (list-of-distinct-rotated-numbers 100)) #'<))
  (parachute:is equal '(12345 23451 34512 45123 51234) (sort (the list (list-of-distinct-rotated-numbers 12345)) #'<)))

(parachute:define-test have-permutated-digits-p-test
  (parachute:true (have-permutated-digits-p 123 321))
  (parachute:true (not (have-permutated-digits-p 123 324))))

;; end
