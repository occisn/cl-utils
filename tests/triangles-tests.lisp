(in-package :cl-utils-tests)

(parachute:define-test triangle-to-2d-array-tests
  
  ;; Test basic triangle conversion
  (parachute:define-test basic-triangle
    (let ((result (triangle-to-2d-array '((1) (2 3) (4 5 6)))))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:is equalp #2A((1 0 0) (2 3 0) (4 5 6)) result)
      (parachute:is = 3 (array-dimension result 0))
      (parachute:is = 3 (array-dimension result 1))))
  
  ;; Test single element triangle
  (parachute:define-test single-element
    (let ((result (triangle-to-2d-array '((5)))))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:is equalp #2A((5)) result)
      (parachute:is = 1 (array-dimension result 0))
      (parachute:is = 1 (array-dimension result 1))))
  
  ;; Test already complete triangle (no padding needed)
  (parachute:define-test complete-triangle
    (let ((result (triangle-to-2d-array '((1 2) (3 4)))))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:is equalp #2A((1 2) (3 4)) result)
      (parachute:is = 2 (array-dimension result 0))
      (parachute:is = 2 (array-dimension result 1))))
  
  ;; Test larger triangle
  (parachute:define-test larger-triangle
    (let ((result (triangle-to-2d-array '((1) (2 3) (4 5 6) (7 8 9 10)))))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:is equalp #2A((1 0 0 0) (2 3 0 0) (4 5 6 0) (7 8 9 10)) result)
      (parachute:is = 4 (array-dimension result 0))
      (parachute:is = 4 (array-dimension result 1))))
  
  ;; Test triangle with gaps in middle rows
  (parachute:define-test triangle-with-gaps
    (let ((result (triangle-to-2d-array '((1) (2) (3 4 5 6)))))
      (parachute:is equalp #2A((1 0 0 0) (2 0 0 0) (3 4 5 6)) result)))
  
  ;; Test that element-type is fixnum
  (parachute:define-test array-element-type
    (let ((result (triangle-to-2d-array '((1) (2 3)))))
      (parachute:is eq 'fixnum (array-element-type result)))))

;;; end
