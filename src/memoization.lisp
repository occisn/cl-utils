;;;; Utilities for memoization.

(in-package :cl-utils)

(defmacro labels-memo-ht (memo-test-fn fn-definition &body body)
  "Variant of 'labels' which allows memoization by hash table.

Example:
 (defun fibonacci (n)
  (labels-memo-ht
   nil
   ((fibo (m)
          (if (or (= m 1) (= m 2))
              1
              (+ (fibo (- m 2)) (fibo (- m 1))))))
   (fibo n)))"
  (when (null memo-test-fn) (setq memo-test-fn '#'equal))
  (with-gensyms (cache cached found-p)
    (let* ((fn-def2 (car fn-definition))
	   (fn-name (car fn-def2))
	   (fn-args (cadr fn-def2)) ; between ()
	   (fn-body (cddr fn-def2))
           (declare-forms nil))

       (setq fn-body
            (loop for forms on fn-body
                  for form1 = (car forms)
                  while (equal 'declare (car form1))
                  do (push form1 declare-forms)
                  finally (return forms)))
      (setq declare-forms (reverse declare-forms))

      `(let ((,cache (make-hash-table :test ,memo-test-fn)))

         (labels ((,fn-name ,fn-args

                    ,@declare-forms

		    (multiple-value-bind (,cached ,found-p) (gethash (list ,@fn-args) ,cache)
		      (if ,found-p
			  ,cached
			  (setf (gethash (list ,@fn-args) ,cache)
			        (progn
				    ,@fn-body))))))
	   ,@body)))))

(defun SHOW-labels-memo-ht (&optional (n 20))
  "Example of usage of labels-memo-ht, on Fibonacci."
  (declare (type fixnum n))
  (labels-memo-ht
   nil
   ((fibo (m)
          (declare (type fixnum m))
          (if (or (= m 1) (= m 2))
              1
              (the fixnum (+ (the fixnum (fibo (the fixnum (- m 2))))
                             (the fixnum (fibo (the fixnum (- m 1)))))))))
   (fibo n)))
;;; --> 6765


(defmacro labels-memo-array (dimensions fn-definition &body body)
  "Variant of 'labels' which allows memoization by array.

Example:
 (defun fibonacci ()
  (labels-memo-array
   21
   ((fibo (m)
          (if (or (= m 1) (= m 2))
              1
              (+ (fibo (- m 2)) (fibo (- m 1))))))
   (fibo 20)))"

  (with-gensyms (cache cached)
    (let* ((fn-def2 (car fn-definition))
	   (fn-name (car fn-def2))
	   (fn-args (cadr fn-def2)) ; between ()
	   (fn-body (cddr fn-def2))
           (declare-forms nil))

       (setq fn-body
            (loop for forms on fn-body
                  for form1 = (car forms)
                  while (equal 'declare (car form1))
                  do (push form1 declare-forms)
                  finally (return forms)))
      (setq declare-forms (reverse declare-forms))

      `(let* ((,cache (make-array ,dimensions :initial-element nil)))

         (labels ((,fn-name ,fn-args

                    ,@declare-forms

		    (let ((,cached (aref ,cache ,@fn-args)))

		      (if ,cached
			  ,cached
			  (setf (aref ,cache ,@fn-args)
			        (progn
				    ,@fn-body))))))
	   ,@body)))))

(defun SHOW-labels-memo-array-1D ()
  "Example of usage of labels-memo-array-1D, on Fibonacci."
  (labels-memo-array
   21
   ((fibo (m)
          (declare (type fixnum m))
          (if (or (= m 1) (= m 2))
              1
              (the fixnum (+ (the fixnum (fibo (the fixnum (- m 2))))
                             (the fixnum (fibo (the fixnum (- m 1)))))))))
   (fibo 20)))
;;; --> 6765

(defun SHOW-labels-memo-array-2D (&optional (n 20))
  "Example of usage of labels-memo-array-2D."

  (declare (type fixnum n))

  (labels-memo-array
   (list (+ 1 n) (+ 1 n))
   ((sub (x y)
         (declare (type fixnum x y))
	 (cond ((= x n) 1)
	       ((= y n) 1)
	       (t (the fixnum (+ (the fixnum (sub (the fixnum (+ 1 x)) y))
                                 (the fixnum (sub x (the fixnum (+ 1 y))))))))))

   (sub 0 0)))

(defmacro labels-memo-mix (memo-test-fn pivot1 fn-definition &body body)
  "Variant of 'labels' which allows memoization by array below PIVOT, and hash table above."
  (when (null memo-test-fn) (setq memo-test-fn 'equal))

  (with-gensyms (cache1 cache2 res pivot)
    (let* ((fn-def2 (car fn-definition))
	   (fn-name (car fn-def2))
	   (fn-args (cadr fn-def2)) ; between ()
           (first-arg (car fn-args))
	   (fn-body (cddr fn-def2))
           (declare-forms nil))

      (setq fn-body
            (loop for forms on fn-body
                  for form1 = (car forms)
                  while (equal 'declare (car form1))
                  do (push form1 declare-forms)
                  finally (return forms)))
      (setq declare-forms (reverse declare-forms))

      `(let* ((,pivot ,pivot1)
              (,cache1 (make-array ,pivot :element-type '(or null fixnum) :initial-element nil))
              (,cache2 (make-hash-table :test #',memo-test-fn)))

         (declare (type fixnum ,pivot))

         (labels ((,fn-name ,fn-args

                    ,@declare-forms

                    (let ((,res (if (< ,first-arg ,pivot)
		                    (aref ,cache1 ,first-arg)
		                    (gethash ,first-arg ,cache2))))
                      (declare (type (or null fixnum) ,res))
                      (if ,res
                          ,res
                          (progn
                            (setq ,res (progn ,@fn-body))
                            (if (< ,first-arg ,pivot)
		                (setf (aref ,cache1 ,first-arg) ,res)
		                (setf (gethash ,first-arg ,cache2) ,res))
                            ,res)))))
	   ,@body)))))

(defun SHOW-labels-memo-mix (&optional (n 20))
  "Example of usage of labels-memo-mix."

  (declare (type fixnum n))

  (labels-memo-mix
   equal
   10
   ((fibo (m)
          (declare (type fixnum m))
          (if (or (= m 1) (= m 2))
              1
              (the fixnum (+ (the fixnum (fibo (the fixnum (- m 2))))
                             (the fixnum (fibo (the fixnum (- m 1)))))))))
   (fibo n)))
;;; --> 6765

(defun SHOW-all-memoization ()
  ""
  (format t "~%~%======~%=== MEMOIZATION~%======~%")
  (format t "~%")
  (format t "labels-memo-ht fibo(20) = ~a~%" (SHOW-labels-memo-ht))
  (format t "labels-memo-array-1D fibo(20) = ~a~%" (SHOW-labels-memo-array-1D))
  (format t "labels-memo-array-2D(20) = ~a~%" (SHOW-labels-memo-array-2D))
  (format t "labels-memo-mix fibo(20) = ~a~%" (SHOW-labels-memo-mix)))

;;; end
