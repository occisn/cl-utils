;;;; Tests for the sorting utilities.

(in-package :cl-utils-tests)

(parachute:define-test vec-qsortd-doublefloat-test
  (parachute:true
   (let* ((n 1000)
          (vec (random-dfvec n :limit 100.0d0))
          (first-index 5)
          (last-index (- n 5)))
     (vec-qsortd-doublefloat vec :first-index first-index :last-index last-index)
     (loop for i from first-index below last-index
           always (<= (aref vec i) (aref vec (+ i 1)))))))

(parachute:define-test vec-qsortds-doublefloat-slave-doublefloat-test
  (parachute:true
   (let* ((n 1000)
          (slave (random-dfvec n :limit 100.0d0))
          (vec (make-array n :element-type 'double-float))
          (first-index 5)
          (last-index (- n 5)))
     (loop for i from 0 below n do
       (setf (aref vec i) (* 2.0d0 (aref slave i))))
     (vec-qsortds-doublefloat-slave-doublefloat vec slave :first-index first-index :last-index last-index)
     (and
      (loop for i from first-index below last-index
            always (<= (aref vec i) (aref vec (+ i 1))))
      (loop for i from first-index below last-index
            always (<= (aref slave i) (aref slave (+ i 1))))))))

(parachute:define-test vec-qsortdsi-doublefloat-slave-fixnum-test
  (parachute:true
   (let* ((n 1000)
          (slave (new-random-fixnum-vector n :mini 0 :maxi 100))
          (vec (make-array n :element-type 'double-float))
          (first-index 5)
          (last-index (- n 5)))
     (declare (type (simple-array fixnum) slave)
              (type (simple-array double-float) vec))
     (loop for i from 0 below n do
       (setf (aref vec i) (* 1.0d0 (aref slave i))))
     (vec-qsortdsi-doublefloat-slave-fixnum vec slave :first-index first-index :last-index last-index)
     (and
      (loop for i from first-index below last-index
            always (<= (aref vec i) (aref vec (+ i 1))))
      (loop for i from first-index below last-index
            always (<= (aref slave i) (aref slave (+ i 1))))))))

(parachute:define-test vec-qsortssi-singlefloat-slave-fixnum-test
  (parachute:true
   (let* ((n 1000)
          (slave (new-random-fixnum-vector n :mini 0 :maxi 100))
          (vec (make-array n :element-type 'single-float))
          (first-index 5)
          (last-index (- n 5)))
     (declare (type (simple-array fixnum) slave)
              (type (simple-array single-float) vec))
     (loop for i from 0 below n do
       (setf (aref vec i) (* 1.0 (aref slave i))))
     (vec-qsortssi-singlefloat-slave-fixnum vec slave :first-index first-index :last-index last-index)
     (and
      (loop for i from first-index below last-index
            always (<= (aref vec i) (aref vec (+ i 1))))
      (loop for i from first-index below last-index
            always (<= (aref slave i) (aref slave (+ i 1))))))))

(parachute:define-test vec-qsortisi-fixnum-slave-fixnum-test
  (parachute:true
   (let* ((n 1000)
          (slave (new-random-fixnum-vector n :mini 0 :maxi 100))
          (vec (make-array n :element-type 'fixnum))
          (first-index 5)
          (last-index (- n 5)))
     (declare (type (simple-array fixnum) slave vec))
     (loop for i from 0 below n do
       (setf (aref vec i) (aref slave i)))
     (vec-qsortisi-fixnum-slave-fixnum vec slave :first-index first-index :last-index last-index)
     (and
      (loop for i from first-index below last-index
            always (<= (aref vec i) (aref vec (+ i 1))))
      (loop for i from first-index below last-index
            always (<= (aref slave i) (aref slave (+ i 1))))))))

;; end
