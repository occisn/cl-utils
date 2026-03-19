(in-package :cl-utils-tests)

;;; ===
;;; === Legendre 2: verify two approaches give same results
;;; ===

(declaim (inline %legendre2-direct-polynomial))
(defun %legendre2-direct-polynomial (n work2)
  "6x2-6x+1 — direct polynomial evaluation for Legendre 2nd order."
  (declare (type fixnum n)
           (type (simple-array double-float (*)) work2))
  (let ((x 0.0d0)
        (sum 0.0d0)
        (mean 0.0d0))
    (declare (type double-float x sum mean))
    (loop for i from 0 below n do
      (setf x (/ i (- n 1.0d0))
            (aref work2 i) (- (* 6.0d0 (* x (- x 1.0d0))) 1.0d0)
            sum (+ sum (aref work2 i))))
    (setq mean (/ sum n)
          sum 0.0d0)
    (loop for i from 0 below n do
      (setf (aref work2 i) (- (aref work2 i) mean)
            sum (+ sum (* (aref work2 i) (aref work2 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i from 0 below n do
      (setf (aref work2 i) (/ (aref work2 i) sum)))))

(parachute:define-test legendre2-same-results
  (parachute:true
   (block outer
     (let* ((limit 1000)
            (eps8 1.0d-8)
            (work1 (make-array limit :element-type 'double-float))
            (work2A (make-array limit :element-type 'double-float))
            (work2B (make-array limit :element-type 'double-float)))
       (loop for n from 4 to limit
             do
                (legendre2 n work1 work2A)
                (%legendre2-direct-polynomial n work2B)
                (loop for j from 0 below n
                      do
                         (when (not (< (abs (- (aref work2A j) (aref work2B j))) eps8))
                           (return-from outer nil)))))
     t)))

;;; ===
;;; === Legendre 3: verify two approaches give same results
;;; ===

(declaim (inline %legendre3-direct-polynomial))
(defun %legendre3-direct-polynomial (n work1 work3)
  "20x^3-30x^2+12x-1 — direct polynomial evaluation for Legendre 3rd order."
  (declare (type fixnum n)
           (type (simple-array double-float (*)) work1 work3))
  (let ((x 0.0d0)
        (sum 0.0d0)
        (mean 0.0d0)
        (proj 0.0d0))
    (declare (type double-float x sum mean proj))
    (legendre1 n work1)
    (setq sum 0.0d0)
    (loop for i from 0 below n do
      (setf x (/ i (- n 1.0d0))
            (aref work3 i) (- (* x (+ 12.0d0 (* x (- (* x 20.0d0) 30.0d0)))) 1.0d0)
            sum (+ sum (aref work3 i))))
    (setq mean (/ sum n)
          sum 0.0d0)
    (loop for i from 0 below n do
      (setf (aref work3 i) (- (aref work3 i) mean)
            sum (+ sum (* (aref work3 i) (aref work3 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i from 0 below n do
      (setf (aref work3 i) (/ (aref work3 i) sum)))
    (setq proj 0.0d0
          sum 0.0d0)
    (loop for i from 0 below n do
      (incf proj (* (aref work1 i) (aref work3 i))))
    (loop for i from 0 below n do
      (setf (aref work3 i) (- (aref work3 i) (* proj (aref work1 i)))
            sum (+ sum (* (aref work3 i) (aref work3 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i from 0 below n do
      (setf (aref work3 i) (/ (aref work3 i) sum)))))

(parachute:define-test legendre3-same-results
  (parachute:true
   (block outer
     (let* ((limit 1000)
            (eps8 1.0d-8)
            (work1A (make-array limit :element-type 'double-float))
            (work1B (make-array limit :element-type 'double-float))
            (work2 (make-array limit :element-type 'double-float))
            (work3A (make-array limit :element-type 'double-float))
            (work3B (make-array limit :element-type 'double-float)))
       (loop for n from 4 to limit
             do
                (legendre3 n work1A work2 work3A)
                (%legendre3-direct-polynomial n work1B work3B)
                (loop for j from 0 below n
                      do (when (not (< (abs (- (aref work3A j) (aref work3B j))) eps8))
                           (return-from outer nil)))))
     t)))

;;; === end
