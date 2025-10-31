(in-package :cl-utils-tests)

;;; === arg-min

(parachute:define-test test-arg-min
  (parachute:is = 3 (cl-utils::arg-min '(4 5 6 1 3) #'<))
  (parachute:is = 2 (cl-utils::arg-min '(4.0 5.0 -5.0 6.0 1.0 3.0) #'<))
  (parachute:is = 3 (cl-utils::arg-min '(4.0d0 5.0d0 -5.0d0 -10.0d0 6.0d0 1.0d0 3.0d0) #'<)))

;;; === arg-max

(parachute:define-test test-arg-max
    (parachute:is = 2 (cl-utils::arg-max '(4 5 6 1 3) #'<)) 
  (parachute:is = 3 (cl-utils::arg-max '(4.0 5.0 -5.0 6.0 1.0 3.0) #'<)) 
  (parachute:is = 4 (cl-utils::arg-max '(4.0d0 5.0d0 -5.0d0 -10.0d0 6.0d0 1.0d0 3.0d0) #'<))) 

;;; === nb-of-occurrences-of-sublist-in-list

(parachute:define-test test-nb-of-occurrences-of-sublist-in-list
  (parachute:is = 6 (cl-utils::nb-of-occurrences-of-sublist-in-list '(4 5) '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5)))
 (parachute:is = 0 (cl-utils::nb-of-occurrences-of-sublist-in-list '(0 1) '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5))))

;;; === circular-lists

(parachute:define-test test-circular-lists
  (parachute:is = 3 (cl-utils::circular-list-length (cl-utils::make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (cl-utils::unliteral--fixnum-list '(1 2 3)))))
  (parachute:is = 3 (cl-utils::circular-list-length (cl-utils::make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (cl-utils::unliteral--fixnum-list '(1 2 3 1 4 5))))))

;;; end
