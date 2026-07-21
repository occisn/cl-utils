;;;; Tests for the numbers doubles utilities.

(in-package :cl-utils-tests)

(defun %df-for-test-= (a b &optional (tolerance 0.00000000001d0))
  "Test if A = B within TOLERANCE for double-float comparison."
  (declare (type double-float a b tolerance))
  (< (abs (- a b)) tolerance))

;;; === string-to-doublefloat-m

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((tmp-box (make-box 0.0d0)))
    (parachute:define-test string-to-doublefloat-m
      (parachute:is
       %df-for-test-= 145.0d0 (the double-float (string-to-doublefloat-m tmp-box "145." #\.)))
      (parachute:is
       %df-for-test-= 145.0d0 (string-to-doublefloat-m tmp-box "145," #\,))
      (parachute:is
       %df-for-test-= 145.256d0 (string-to-doublefloat-m tmp-box "145,256" #\,))
      (parachute:is
       %df-for-test-= 0.256d0 (string-to-doublefloat-m tmp-box "0;256" #\;))
      (parachute:is
       %df-for-test-= 0.256d0 (string-to-doublefloat-m tmp-box ",256" #\,)))))

;;; === vec-variance-m

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parachute:define-test vec-variance-m
    (parachute:is
     %df-for-test-=
     5.0d0
     (let ((tmp-box (make-box 0.0d0))
           (input-vec (make-array 4 :initial-contents (list 1.0d0 3.0d0 5.0d0 7.0d0) :element-type 'double-float)))
       (vec-variance-m tmp-box input-vec)))))

;;; === vec-highest-m

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((tmp-box (make-box 0.0d0)))
    (parachute:define-test vec-highest-m
      (parachute:is
       %df-for-test-=
       4.0d0
       (vec-highest-m tmp-box (make-array 4 :element-type 'double-float :initial-contents (list 4.0d0 3.0d0 2.0d0 1.0d0))))
      (parachute:is
       %df-for-test-=
       3.0d0
       (vec-highest-m tmp-box (make-array 4 :element-type 'double-float :initial-contents (list 4.0d0 3.0d0 2.0d0 1.0d0)) :first-index 1)))))

;;; === vec-lowest-m

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((tmp-box (make-box 0.0d0)))
    (parachute:define-test vec-lowest-m
      (parachute:is
       %df-for-test-=
       1.0d0
       (vec-lowest-m tmp-box (make-array 6 :element-type 'double-float :initial-contents (list 1.0d0 1.5d0 4.0d0 3.0d0 2.0d0 1.0d0))))
      (parachute:is
       %df-for-test-=
       1.5d0
       (vec-lowest-m tmp-box (make-array 6 :element-type 'double-float :initial-contents (list 1.0d0 1.5d0 4.0d0 3.0d0 2.0d0 7.0d0)) :first-index 1)))))

;;; === test-floor-of-positive-df

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
