(in-package :cl-utils)

(defun %long-function-A (&optional (nb-times 800))
  "This function is used for tests of time measurement.
Its execution last around 0.1s for NB-TIMES = 15 (default)."
  (declare (type fixnum nb-times))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((n 1999999973))
    (loop
      repeat nb-times
      do (loop for m from 3 by 2
               while (<= (* m m) n)
               always (not (= 0 (mod n m)))))))

(defun %for-bench-fact-1 (n)
  ;; Recursive
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (cond
    ((zerop n) 1)
    (t (* n (%for-bench-fact-1 (- n 1))))))

(defun %for-bench-fact-2 (n)
  ;; Recursive with tail recursion
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (labels
      ((%for-bench-fact-2-help (n acc)
	 (cond
	   ((zerop n) acc)
	   (t (%for-bench-fact-2-help (- n 1) (* acc n))))))
    (%for-bench-fact-2-help n 1)))

(defun %for-bench-fact-2b (n &optional (acc 1))
  ;; Recursive with tail recursion, and optional argument
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (cond
    ((zerop n) acc)
    (t (%for-bench-fact-2b (- n 1) (* acc n)))))

(defun %for-bench-fact-3 (n)
  ;; Iterative: dotimes and assignment
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((prod 1))
    (dotimes (i n prod)               ; result-form = prod
      (setf prod (* prod (+ i 1))))))

(defun %for-bench-fact-4 (n)
  ;; Iterative: do and implicit assignment
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (do ((i n (- i 1))
       (result 1 (* result i)))
      ((zerop i) result))) ; pas de body dans le do

(defun %for-bench-fact-5 (n)
  ;; Iterative: do* and implicit assignment
  ;; (check-type n (integer 0 *))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (do* ((i n (- i 1))
	(result 1 (* result (+ i 1))))
       ((zerop i) result))) ; pas de body dans le do

;;; ===
;;; === Function start-up-1
;;; ===

(defun start-up-1 ()
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((res 0))
    (loop for i from 1 to 10000000 do (incf res (* i (+ i 1))))
    res))

;;; ===
;;; === my-time
;;; ===


(defmacro %my-time-run (exp)
  "Return the duration in run time for the execution of the expression EXP.

Example: (my-time-run (+ 2 2))"
  (let ((run-base (gensym)))
    `(let* ((,run-base (get-internal-run-time)))
       ,exp
       (/ (- (get-internal-run-time) ,run-base) internal-time-units-per-second 1.0))))
;;; (my-time-run (%long-function-A 30))

(defmacro %my-time-real (exp)
  "Return the duration in real time for the execution of the expression EXP.

Example: (my-time-real (+ 2 2))
"
  (let ((real-base (gensym)))
    `(let* ((,real-base (get-internal-real-time)))
       ,exp
       (/ (- (get-internal-real-time) ,real-base) internal-time-units-per-second 1.0))))
;;; (my-time-real (%long-function-A 30))


;;; ===
;;; === Compare durations in REPL, with exemples
;;; ===

(defmacro %my-time (exp)
  "Return the duration in real time and run time spent for the execution of the expression EXP (v2, 19.3.2017).

Example: (my-time (+ 2 2))
         (my-time (%long-function-A))"
;;; see https://rosettacode.org/wiki/Time_a_function#Common_Lisp
  (let ((real-base (gensym))
	(run-base (gensym)))
    `(let* ((,real-base (get-internal-real-time))
	    (,run-base (get-internal-run-time)))
       ,exp
       (values
        (/ (- (get-internal-real-time) ,real-base) internal-time-units-per-second 1.0)
        (/ (- (get-internal-run-time) ,run-base) internal-time-units-per-second 1.0)))))

(defmacro compare-durations (list-of-functions &key (args '()) (repeat 1) (start-up '(lambda () (values))) context (show-doc nil))
  "Compare the duration of the execution of several functions, with the same arguments (v3, 19.2.2017).
Require 'my-time' macro
LIST-OF-FUNCTIONS: list of functions
ARGS: list of arguments (by default: '())
REPEAT: nb of times each function is executed (by default: 1)
START-UP: function to be called at the beginning (by default: ~values)
CONTEXT: context around the function (by default: nil) ; supersedes ARGS
SHOW-DOC: show documentation (by default: nil)

Usage: see examples below."

  (flet ((my-time2 (body)
           `(let* ((real-base (get-internal-real-time))
                   (run-base (get-internal-run-time)))
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
              ,body
              (values
               (/ (- (get-internal-real-time) real-base) internal-time-units-per-second 1.0)
               (/ (- (get-internal-run-time) run-base) internal-time-units-per-second 1.0)))))

      `(let ((results '()))

         (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

         (labels ((substring-after-last (str chr)
                    "Return the last substring of STR after character CHAR.
For instance: 'abc::def' #\: --> 'def'
(v1, available in occisn/cl-utils GitHub repository)"
                    (declare (type character chr)
                             ;; (type (simple-array character) str)
                             )
                    (if
                     (= 0 (length str))
                     str
                     (let ((idx
                             (loop with res of-type fixnum = 0
                                   for c of-type character across str
                                   for i of-type fixnum from 0
                                   when (char= c chr)
                                     do (setq res i)
                                   finally (return res))))
                       (if (= idx 0)
                           str
                           (subseq str (+ idx 1) )))))

                  (function-to-string-no-package (fn)
                    "Return a string corresponding to function FN without the possible initial part corresponding to the package. This initial part is identified through ':'.
(v1 available in occisn/cl-utils GitHub repository)"
                    (declare (type function fn))

                    (substring-after-last
                     (format nil "~a" (nth-value 2 (function-lambda-expression fn)))
                     #\:))

                  (print-and-store-results (fn0 real-time run-time)
                    ,(if show-doc
		         
		         `(format t "~%Real-time: ~s run-time: ~s for ~a (~s)"
			          real-time
			          run-time
			          (function-to-string-no-package fn0)
			          (documentation fn0 'function))
                         `(format t "~%Real-time: ~s run-time: ~s for ~a"
			          real-time
			          run-time
			          (function-to-string-no-package fn0)))

                    (push (list fn0 real-time run-time) results))) ; end of labels definition

           (funcall #',start-up)
           
           (format t "~%~%(1) One after the other~%---")
           ,@(mapcar #'(lambda (fn)
		         
		         (cond
		           
		           ((not (null context))
		            `(multiple-value-bind (real-time run-time)
			         ,(my-time2 `(dotimes (i ,repeat) (funcall ,context #',fn)))
			       (print-and-store-results #',fn real-time run-time)))
		           (t
		            `(multiple-value-bind (real-time run-time)
			         ,(my-time2 `(dotimes (i, repeat) (funcall #',fn ,@args)))
			       (print-and-store-results #',fn real-time run-time)))))
		     
		     list-of-functions)

           (setf results (sort results #'< :key #'cadr))

           (format t "~%~%(2) Sorted by increasing real-time~%---")
           (dolist (one-result results)
             ,(if show-doc
                  `(format t "~%Real-time: ~s run-time: ~s for ~a (~s)"
		           (second one-result) ; real-time
		           (third one-result)  ; run-time
		           (function-to-string-no-package (first one-result)) ; name
		           (documentation (first one-result) 'function))
                  `(format t "~%Real-time: ~s run-time: ~s for ~a"
		           (second one-result) ; real-time
		           (third one-result)  ; run-time
		           (function-to-string-no-package (first one-result)))) ; name
	     )))))

(defun SHOW-1-compare-durations ()
  "Example of usage of 'compare-durations'."
  (compare-durations (%long-function-A
                      %long-function-A)))

(defun SHOW-2-compare-durations ()
  "Example of usage of 'compare-durations'."
  (compare-durations
   (%long-function-A %long-function-A)
   :repeat 10
   :start-up (lambda () (loop for i from 1 to 10))
   :context (lambda (f) (loop for i from 15 to 16 do (funcall f i)))))

(defun SHOW-3-compare-durations ()
  "Example of usage of 'compare-durations'."
  (compare-durations
   (%for-bench-fact-1
    %for-bench-fact-2
    %for-bench-fact-2b
    %for-bench-fact-3
    %for-bench-fact-4
    %for-bench-fact-5)
   :args (10000)))

(defun SHOW-4-compare-durations ()
  "Example of usage of 'compare-durations'."
  (compare-durations
   (%for-bench-fact-1
    %for-bench-fact-2
    %for-bench-fact-2b
    %for-bench-fact-3
    %for-bench-fact-4
    %for-bench-fact-5)
   :args (10000)
   :repeat 10))

(defun SHOW-5-compare-durations ()
  "Example of usage of 'compare-durations'."
  (compare-durations
   (%for-bench-fact-1
    %for-bench-fact-2
    %for-bench-fact-2b
    %for-bench-fact-3
    %for-bench-fact-4
    %for-bench-fact-5)
   :context (lambda (fn) (dotimes (i 1000) (funcall fn i)))))

;;; end
