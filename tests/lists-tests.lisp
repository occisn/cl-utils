(in-package :cl-utils-tests)

;;; === delete-nth

(parachute:define-test delete-nth-tests
  
  ;; Test deleting from middle of list
  (parachute:define-test delete-middle
    (parachute:is equal '(1 2 4 5) (delete-nth 2 '(1 2 3 4 5))))
  
  ;; Test deleting first element (n=0)
  (parachute:define-test delete-first
    (parachute:is equal '(2 3 4) (delete-nth 0 '(1 2 3 4))))
  
  ;; Test deleting last element
  (parachute:define-test delete-last
    (parachute:is equal '(1 2 3) (delete-nth 3 '(1 2 3 4))))
  
  ;; Test single element list
  (parachute:define-test delete-single-element
    (parachute:is equal '() (delete-nth 0 '(42))))
  
  ;; Test empty list
  (parachute:define-test delete-from-empty
    (parachute:is equal '() (delete-nth 0 '())))
  
  ;; Test n out of bounds (larger than list length)
  (parachute:define-test delete-out-of-bounds
    (parachute:is equal '(1 2 3) (delete-nth 10 '(1 2 3))))
  
  ;; Test n negative (shouldn't match any index)
  (parachute:define-test delete-negative-index
    (parachute:is equal '(1 2 3) (delete-nth -1 '(1 2 3))))
  
  ;; Test non-destructive behavior
  (parachute:define-test delete-non-destructive
    (let ((original '(1 2 3 4 5)))
      (delete-nth 2 original)
      (parachute:is equal '(1 2 3 4 5) original)))
  
  ;; Test with different data types
  (parachute:define-test delete-mixed-types
    (parachute:is equal '(a "hello" 42) (delete-nth 1 '(a :symbol "hello" 42)))))


;; replace-nth

(parachute:define-test replace-nth-tests
  
  ;; Test replacing in middle of list
  (parachute:define-test replace-middle
    (parachute:is equal '(1 2 99 4 5) (replace-nth 2 99 '(1 2 3 4 5))))
  
  ;; Test replacing first element (n=0)
  (parachute:define-test replace-first
    (parachute:is equal '(99 2 3 4) (replace-nth 0 99 '(1 2 3 4))))
  
  ;; Test replacing last element
  (parachute:define-test replace-last
    (parachute:is equal '(1 2 3 99) (replace-nth 3 99 '(1 2 3 4))))
  
  ;; Test single element list
  (parachute:define-test replace-single-element
    (parachute:is equal '(99) (replace-nth 0 99 '(42))))
  
  ;; Test empty list
  (parachute:define-test replace-in-empty
    (parachute:is equal '() (replace-nth 0 99 '())))
  
  ;; Test n out of bounds (larger than list length)
  (parachute:define-test replace-out-of-bounds
    (parachute:is equal '(1 2 3) (replace-nth 10 99 '(1 2 3))))
  
  ;; Test n negative (shouldn't match any index)
  (parachute:define-test replace-negative-index
    (parachute:is equal '(1 2 3) (replace-nth -1 99 '(1 2 3))))
  
  ;; Test non-destructive behavior
  (parachute:define-test replace-non-destructive
    (let ((original '(1 2 3 4 5)))
      (replace-nth 2 99 original)
      (parachute:is equal '(1 2 3 4 5) original)))
  
  ;; Test replacing with different data types
  (parachute:define-test replace-with-symbol
    (parachute:is equal '(1 foo 3) (replace-nth 1 'foo '(1 2 3))))
  
  (parachute:define-test replace-with-string
    (parachute:is equal '(1 "hello" 3) (replace-nth 1 "hello" '(1 2 3))))
  
  (parachute:define-test replace-with-nil
    (parachute:is equal '(1 nil 3) (replace-nth 1 nil '(1 2 3))))
  
  (parachute:define-test replace-number-with-list
    (parachute:is equal '(1 (a b c) 3) (replace-nth 1 '(a b c) '(1 2 3)))))

;;; === arg-min

(parachute:define-test test-arg-min
  (parachute:is = 3 (arg-min '(4 5 6 1 3) #'<))
  (parachute:is = 2 (arg-min '(4.0 5.0 -5.0 6.0 1.0 3.0) #'<))
  (parachute:is = 3 (arg-min '(4.0d0 5.0d0 -5.0d0 -10.0d0 6.0d0 1.0d0 3.0d0) #'<)))

;;; === arg-max

(parachute:define-test test-arg-max
    (parachute:is = 2 (arg-max '(4 5 6 1 3) #'<)) 
  (parachute:is = 3 (arg-max '(4.0 5.0 -5.0 6.0 1.0 3.0) #'<)) 
  (parachute:is = 4 (arg-max '(4.0d0 5.0d0 -5.0d0 -10.0d0 6.0d0 1.0d0 3.0d0) #'<))) 

;;; === nb-of-occurrences-of-sublist-in-list

(parachute:define-test test-nb-of-occurrences-of-sublist-in-list
  (parachute:is = 6 (nb-of-occurrences-of-sublist-in-list '(4 5) '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5)))
 (parachute:is = 0 (nb-of-occurrences-of-sublist-in-list '(0 1) '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5))))

;;; === sublist-knowing-indexes-as-list

(parachute:define-test sublist-knowing-indexes-as-list-test
  (parachute:is equal '(c d f) (sublist-knowing-indexes-as-list '(a b c d e f) '(2 3 5)))
  (parachute:is equal '(a) (sublist-knowing-indexes-as-list '(a b c) '(0)))
  (parachute:is equal nil (sublist-knowing-indexes-as-list '(a b c) nil))
  ;; Unsorted indexes should still work
  (parachute:is equal '(a c) (sublist-knowing-indexes-as-list '(a b c d) '(2 0))))

;;; === shuffle

(parachute:define-test shuffle-test
  ;; Shuffle preserves all elements (same set)
  (let* ((original (list 1 2 3 4 5))
         (shuffled (shuffle (copy-list original))))
    (parachute:is = (length original) (length shuffled))
    (parachute:is equal (sort (copy-list original) #'<) (sort (copy-list shuffled) #'<)))
  ;; Single element list
  (parachute:is equal '(42) (shuffle (list 42)))
  ;; Empty list
  (parachute:is equal nil (shuffle nil)))

;;; === fixnump

(parachute:define-test fixnump-test
  (parachute:true (fixnump 42))
  (parachute:true (fixnump 0))
  (parachute:true (fixnump -1))
  (parachute:false (fixnump 3.14))
  (parachute:false (fixnump "hello")))

;;; === list-of-fixnums-p

(parachute:define-test list-of-fixnums-p-test
  (parachute:true (list-of-fixnums-p '(1 2 3)))
  (parachute:false (list-of-fixnums-p '(1 2.0 3)))
  (parachute:false (list-of-fixnums-p nil))
  (parachute:false (list-of-fixnums-p '())))

;;; === double-float-p

(parachute:define-test double-float-p-test
  (parachute:true (double-float-p 3.14d0))
  (parachute:false (double-float-p 3.14))
  (parachute:false (double-float-p 42))
  (parachute:false (double-float-p "hello")))

;;; === list-of-double-floats-p

(parachute:define-test list-of-double-floats-p-test
  (parachute:true (list-of-double-floats-p '(1.0d0 2.0d0 3.0d0)))
  (parachute:false (list-of-double-floats-p '(1.0d0 2.0 3.0d0)))
  (parachute:false (list-of-double-floats-p nil)))

;;; === circular-lists

(parachute:define-test test-circular-lists
  (parachute:is = 3 (circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (unliteral--fixnum-list '(1 2 3)))))
  (parachute:is = 3 (circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (unliteral--fixnum-list '(1 2 3 1 4 5))))))

;;; end
