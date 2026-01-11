(in-package :cl-utils)

(defun SHOW-lists-general ()
  ""

  ;; Caution: LAST does not return last element, but last cdr

  ;; n-th element:
  ;; (nth 3 list)

  ;; Caution, shared structures:
  ;; a) if x <- '(1 2 3) and y <- (list x x x), modifying x also modifies y
  ;; b) if x <- 1, y <- (2 3 4) and z <- (cons x y), modifying y also modifies z
  ;; c) if x <- (1 2 3), y <- (4 5 6), z <- (append x y), modifying y (and not x) also modifies z

  )

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
Not optimized.
(v1 as of 2025-05-17, available in occisn/cl-utils GitHub repository)"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (loop for elt in lst
          for i from 0
          unless (= i n) collect elt)))

(defun replace-nth (n new-value lst)
  "Return a list which is the original LST where N-th element is replaced by NEW-VALUE. Not destructive.
Not optimized.
(v1 as of 2025-05-18, available in occisn/cl-utils GitHub repository)"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (loop for elt in lst
          for i from 0
          when (= i n) collect new-value
          unless (= i n) collect elt)))

(defun list-preview (lst)
  "Print preview of the content of list LST.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type list lst))
  (let ((n (length lst)))
    (cond ((null lst)
           (format t "Null list~%"))
          ((<= n 6)
           (format t "[~a] ~{~a~^ ~}~%" n lst)
           )
          (t
           (format t "[~a] ~a ~a ~a ... ~a ~a ~a~%"
                   n
                   (elt lst 0)
                   (elt lst 1)
                   (elt lst 2)
                   (elt lst (- n 3))
                   (elt lst (- n 2))
                   (elt lst (- n 1)))))))

(defun arg-min (lst predicate)
  "Return the position (starting at 0) of the lowest element in list LST according to predicate PREDICATE.
For instance: '(1 3 2 0 5) #'< --> 3
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type function predicate))
  (let ((index-min 0)
        (value-min (car lst)))
    (declare (type fixnum index-min))
    (loop for x in (cdr lst)
          for i of-type fixnum from 1
          when (funcall predicate x value-min)
          do (setq index-min i
                   value-min x))
    index-min))

(defun arg-max (lst predicate)
  "Return the position (starting at 0) of the highest element in list LST according to predicate PREDICATE.
For instance: '(1 3 2 0 5) #'< --> 4
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type function predicate))
  (let ((index-max 0)
        (value-max (car lst)))
    (declare (type fixnum index-max))
    (loop for x in (cdr lst)
          for i of-type fixnum from 1
          when (funcall predicate value-max x)
          do (setq index-max i
                   value-max x))
    index-max))

(defun nb-of-occurrences-of-sublist-in-list (sub L)
  "Return the number of occurrences of list SUB within list L.
Example:
'(4 5) and '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5) --> 6.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type list sub L))
  (the fixnum
       (loop
             with nb of-type fixnum = 0
             for idx of-type fixnum = 0 then (+ idx2 1) 
             for idx2 = (search sub L :start2 idx) then (search sub L :start2 idx)
             until (null idx2)
             do (incf nb)
             finally (return nb))))

(defun make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (list)
  "Return a circular list based on list LST.
For instance: '(1 2 3) --> '(1 2 3 1 2 3 1 2 ...).
The argument should not be a litteral. Use 'copy-seq' if necessary.
Do not print the result.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type list list))
  (setf (cdr (last list)) list)
  list)

(defun circular-list-length (list) ; for list of numbers
  "Return the length of a circular list of *different* numbers.
For instance: (circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (copy-seq '(1 2 3)))) --> 3.
but (circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (copy-seq '(1 2 3 1 4 5)))) --> 3 (and not 5).
The argument is supposed to be a list of fixnum.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type (cons (integer)) list))
  (let ((first-element (car list)))
    (declare (type fixnum first-element))
    (loop for n of-type fixnum from 1
          for x of-type (cons (integer)) on (cdr list)
          when (= first-element (car x))
          return n)))

(declaim (ftype (function (list) list) unliteral--fixnum-list))
(defun unliteral--fixnum-list (lst)
  "Return an unliteral version of fixnum list LST.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type list lst))
  (loop for i of-type fixnum in lst collect i))

(defun SHOW-all-lists ()
  ""
  (format t "~%~%======~%=== LISTS~%======~%")
  (format t "~%")
  (SHOW-lists-general)
  (format t "~%")
  (format t "(delete-nth 2 '(1 2 3 4 5)) --> ~a~%" (delete-nth 2 '(1 2 3 4 5)))
  (format t "~%")
  (format t "(replace-nth 2 33 '(1 2 3 4 5)) --> ~a~%" (replace-nth 2 33 '(1 2 3 4 5)))
  (format t "~%")
  (list-preview '(1 2 3 4 5 6 7 8 9))
  (format t "~%")
  (format t "(arg-min '(1 3 2 0 5) #'<) --> ~a~%" (arg-min '(1 3 2 0 5) #'<))
  (format t "~%")
  (format t "(arg-max '(1 3 2 0 5) #'<) --> ~a~%" (arg-max '(1 3 2 0 5) #'<))
  (format t "~%")
  (format t "nb-of-occurrences-of-sublist-in-list: ~a~%" (nb-of-occurrences-of-sublist-in-list '(4 5) '(1 2 3 4 5 6 7 4 5 6 7 4 5 8 9 4 5 6 4 5 4 5)))
  (format t "~%")
  (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (list 1 2 3))
  (format t "make-circular-DO-NOT-PRINT--AND-NOT-LITERAL... done~%")
  (format t "~%")
  (format t "circular-list-length: ~a~%" (circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (list 1 2 3))))
  (format t "~%")
  (format t "unliteral--fixnum-list: ~a~%" (unliteral--fixnum-list '(1 2 3)))
  )

;;; end
