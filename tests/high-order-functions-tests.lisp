(in-package :cl-utils-tests)

;;; === first-which

(parachute:define-test first-which-test
  (parachute:is
   equal '(8 64)
   (first-which
    :generator (loop for n of-type fixnum from 1 do (cl-utils::submit n))
    :fn (lambda (n)
          (declare (type fixnum n))
          (the fixnum (* n n)))
    :target-reached-fn (lambda (y)
                         (declare (type fixnum y))
                         (> y 60)))))

;;; === all-which

(parachute:define-test all-which-test
  (parachute:is equal '((2 4) (4 16) (6 36))
                (all-which :fn (lambda (n)
                                 (declare (type fixnum n))
                                 (the fixnum (* n n)))
			   :target-reached-fn #'evenp
			   :from 1
			   :arg-step-fn #'1+
			   :while-arg-fn (lambda (n)
                                           (declare (type fixnum n))
                                           (< n 10))
			   :while-res-fn (lambda (y)
                                           (declare (type fixnum y))
                                           (< y 60)))))

;;; === maximizing--fixnum

(parachute:define-test maximizing--fixnum-test
  (parachute:is equal
                '(5 (0))
                (maximizing--fixnum
                 (loop for n from -3 to 3
                       for y = (- 5 (* n n))
                       do (cl-utils::maximize y n)))))

;;; === minimizing--fixnum

(parachute:define-test minimizing--fixnum-test
  (parachute:is equal
                '(5 (0))
                (minimizing--fixnum
                 (loop for n from -3 to 3
                       for y = (+ 5 (* n n))
                       do (cl-utils::minimize y n)))))

;;; === max1D

(parachute:define-test max1D-test
  (parachute:is equal '(25 (0)) (max1D (lambda (n)
                                         (declare (type fixnum n))
                                         (- 25 (the fixnum (* n n))))
                                       -5 5))
  (parachute:is equal '(25 (0)) (max1D (lambda (n)
                                         (declare (type fixnum n))
                                         (- 25 (the fixnum (* n n))))
                                       -5 5 :predicate #'>
                                       :key (lambda (n)
                                              (declare (type fixnum n))
                                              (- n))))
  (parachute:is equal '(24 (-1 1)) (max1D (lambda (n)
                                            (declare (type fixnum n))
                                            (- 25 (the fixnum (* n n))))
                                          -5 5 :filter-on-value #'evenp))
  (parachute:is equal '(25 (-5 5)) (max1D (lambda (n)
                                            (declare (type fixnum n))
                                            (the fixnum (* n n)))
                                          -5 5))
  (parachute:is equal '(16 (-4 4)) (max1D (lambda (n)
                                            (declare (type fixnum n))
                                            (the fixnum (* n n)))
                                          -5 5 :filter-on-n #'evenp))
  (parachute:is equal '(3 (-1 0 1)) (max1D (constantly 3) -1 1))
  (parachute:is equal '(3 (0)) (max1D (lambda (_x)
                                        (declare (ignorable _x))
                                        3)
                                      -1 1 :filter-on-n #'evenp))
  (parachute:is equal '(nil nil) (max1D (lambda (_x)
                                          (declare (ignorable _x))
                                          3)
                                        0 0 :filter-on-n #'oddp)))

;;; === min1D

(parachute:define-test min1D-test
  (parachute:is equal '(0 (0)) (min1D (lambda (n)
                                        (declare (type fixnum n))
                                        (the fixnum (* n n)))
                                      -5 5))
  (parachute:is equal '(0 (0)) (min1D (lambda (n)
                                        (declare (type fixnum n))
                                        (the fixnum (* n n)))
                                      -5 5 :predicate #'>
                                      :key (lambda (n)
                                             (declare (type fixnum n))
                                             (- n)))))

;;; === max2D

(parachute:define-test max2D-test
  (parachute:is equal '(50 ((0 0)))
                (max2D (lambda (x y)
                         (declare (type fixnum x y))
                         (the fixnum (- 50 (+ (the fixnum (* x x)) (the fixnum (* y y))))))
                       -5 5 -5 5))
  (parachute:is equal '(50 ((0 0)))
                (max2D (lambda (x y)
                         (declare (type fixnum x y))
                         (the fixnum (- 50 (the fixnum (* x x)) (the fixnum (* y y)))))
                       -5 5 -5 5 :predicate #'>
                       :key (lambda (n)
                              (declare (type fixnum n))
                              (- n))))
  (parachute:is equal '(50 ((-5 -5) (-5 5) (5 -5) (5 5)))
                (max2D (lambda (x y)
                         (declare (type fixnum x y))
                         (the fixnum (+ (the fixnum (* x x)) (the fixnum (* y y)))))
                       -5 5 -5 5)))

;;; === min2D

(parachute:define-test min2D-test
  (parachute:is equal '(0 ((0 0)))
                (min2D (lambda (x y)
                         (declare (type fixnum x y))
                         (the fixnum (+ (the fixnum (* x x)) (the fixnum (* y y)))))
                       -5 5 -5 5))
  (parachute:is equal '(0 ((0 0)))
                (min2D (lambda (x y)
                         (declare (type fixnum x y))
                         (the fixnum (+ (the fixnum (* x x)) (the fixnum (* y y)))))
                       -5 5 -5 5 :predicate #'>
                       :key (lambda (n)
                              (declare (type fixnum n))
                              (- n)))))

;;; === end
