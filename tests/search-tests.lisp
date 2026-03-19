(in-package :cl-utils-tests)

;;; === binary-search

(parachute:define-test binary-search-test

  ;; Found at various positions
  (parachute:is = 0 (binary-search 1 (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5))))
  (parachute:is = 2 (binary-search 3 (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5))))
  (parachute:is = 4 (binary-search 5 (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5))))

  ;; Not found
  (parachute:false (binary-search 0 (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5))))
  (parachute:false (binary-search 6 (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5))))

  ;; Single element array
  (parachute:is = 0 (binary-search 42 (make-array 1 :element-type 'fixnum :initial-contents '(42))))
  (parachute:false (binary-search 43 (make-array 1 :element-type 'fixnum :initial-contents '(42))))

  ;; Two element array
  (parachute:is = 0 (binary-search 1 (make-array 2 :element-type 'fixnum :initial-contents '(1 2))))
  (parachute:is = 1 (binary-search 2 (make-array 2 :element-type 'fixnum :initial-contents '(1 2))))
  (parachute:false (binary-search 3 (make-array 2 :element-type 'fixnum :initial-contents '(1 2))))

  ;; Larger array (duplicate value: returns first match found by binary search)
  (parachute:is = 4 (binary-search 50 (make-array 10 :element-type 'fixnum :initial-contents '(10 20 30 40 50 50 60 70 80 90))))

  ;; With gaps (value between elements)
  (parachute:false (binary-search 15 (make-array 5 :element-type 'fixnum :initial-contents '(10 20 30 40 50)))))

;;; === binary-search--with-initial-bounds

(parachute:define-test binary-search--with-initial-bounds-test

  ;; Search within a subrange
  (let ((arr (make-array 7 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6 7))))
    (parachute:is = 3 (binary-search--with-initial-bounds 4 arr 2 5))
    (parachute:false (binary-search--with-initial-bounds 1 arr 2 5))
    (parachute:is = 2 (binary-search--with-initial-bounds 3 arr 2 5))
    (parachute:is = 5 (binary-search--with-initial-bounds 6 arr 2 5))))

;;; end
