(in-package :cl-utils-tests)



(parachute:define-test test-unliteral--fixnum-vector
    (let* ((input #(1 2 3 4 5 6 7 8 9 10))
           (result (unliteral--fixnum-vector input)))
      (loop for i of-type fixnum from 0 below (length input)
            do (parachute:is = (aref input i) (aref result i)))))

;; === benchmark related to SVREF:

;; array:
(defun %svref-foo-1 ()
  ""
  (let ((arr (make-array 1500000 :element-type 'fixnum :initial-element 0))
        (sum1 0)
        (res 0))
    (declare (type fixnum sum1 res))
    (dotimes (j 1500)
      (setq sum1 0)
      (dotimes (i 1500000)
        (setf (aref arr i) i))
      (dotimes (i 1500000)
        (incf sum1 (aref arr i)))
      (incf res sum1))
    res))
;; 2.5s

;; array with hint:
(defun %svref-foo-2 ()
  ""
  (let ((arr (make-array 1500000 :element-type 'fixnum :initial-element 0))
        (sum1 0)
        (res 0))
    (declare (type (simple-array fixnum (*)) arr) ; <------
             (type fixnum sum1 res))
    (dotimes (j 1500)
      (setq sum1 0)
      (dotimes (i 1500000)
        (setf (aref arr i) i))
      (dotimes (i 1500000)
        (incf sum1 (aref arr i)))
      (incf res sum1))
    res))
;; 2.5s

;; simple-vector:
(defun %svref-foo-3 ()
  ""
  (let ((v (make-array 1500000 :element-type 'fixnum :initial-element 0))
        (sum1 0)
        (res 0))
    (declare (type fixnum sum1 res))
    (setq v (coerce v 'simple-vector))  ; <---
    (dotimes (j 1500)
      (setq sum1 0)
      (dotimes (i 1500000)
        (setf (svref v i) i))
      (dotimes (i 1500000)
        (incf sum1 (the fixnum (svref v i))) ; <---
        )
      (incf res sum1))
    res))
;; 2.6s


(defun compare-duration-svref ()
  ""
  (compare-durations
   (%svref-foo-1
    %svref-foo-2
    %svref-foo-3)
   :context (lambda (fn)
              (funcall fn))
   :start-up start-up-1))

;;; === Conclusion: svref does not bring extra performance, at least with (speed 3)

;;; end
