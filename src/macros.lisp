(in-package :cl-utils)

(defmacro with-gensyms (syms &body body)
  "Usual 'with-gensyms' macro.
(v1 available in occisn/cl-utils GitHub repository)"
  `(let ,(mapcar #'(lambda (s)
                     (declare (type symbol s))
                     `(,s (gensym ,(string s))))
          syms)
     ,@body))

(defmacro OLD-while (condition &body body)
  "While macro based on CONDITION and BODY.
(v1, available in occisn/cl-utils GitHub repository)"
  `(loop while ,condition
         do (progn ,@body)))

(defmacro while (condition &body body)
  "While macro based on CONDITION and BODY.
(v2, available in occisn/cl-utils GitHub repository)"
  (let ((start (gensym "START")))
    `(tagbody
       ,start
       (when ,condition
         (progn ,@body)
         (go ,start)))))

(defun SHOW-while ()
  (let ((a 0))
    (while (< a 5)
           (format t "~a " a)
           (incf a))
    a))
;; prints 0 1 2 3 4 and returns 5

(defmacro OLD-while1 (test &body body)
  "Typical 'while' macro. Same as 'while', but named as 'while1' to be used within 'loop' block, where 'while' is overshadowed.
(v1 available in occisn/cl-utils GitHub repository)"
  (let ((beg-tag (gensym))
        (end-tag (gensym)))
    `(tagbody
	,beg-tag
        (unless ,test (go ,end-tag))
        ,@body
        (go ,beg-tag)
	,end-tag)))

(defmacro while1 (condition &body body)
  "Typical 'while' macro. Same as 'while', but named as 'while1' to be used within 'loop' block, where 'while' is overshadowed.
(v2 available in occisn/cl-utils GitHub repository)"
  (let ((start (gensym "START")))
    `(tagbody
       ,start
       (when ,condition
         (progn ,@body)
         (go ,start)))))

(defun SHOW-while1 ()
  (let ((a 0))
    (while1 (< a 5)
            (format t "~a " a)
            (incf a))
    a))
;; prints 0 1 2 3 4 and returns 5

(defmacro repeat-until (&body body)
  "Usage: (repeat-until sexp1 sexp2 ... sexpn :until condition)
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type list body))
  (when (<= (length body) 2)
    (error "Not enough arguments in repeat macro"))
  (when (not (eql :until (elt body (- (length body) 2))))
    (error "Keyword :until missing in repeat macro"))
  (let ((beg-tag (gensym))
	(body2 (butlast (butlast body)))
	(until-test (car (last body))))
    `(tagbody
	,beg-tag
	,@body2
	(unless ,until-test (go ,beg-tag))) ))

(defun SHOW-repeat-until ()
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((a 0))
        (repeat-until
         (incf a)
         :until (> (* a a) 10))
        a)))
;; returns 4

(defmacro do-while (&body body)
  "Usage: (do-while sexp1 sexp2 ... sexpn :while condition)
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type cons body))
  (when (<= (length body) 2)
    (error "Not enough arguments in do-while macro"))
  (when (not (eql :while (elt body (- (length body) 2))))
    (error "Keyword :while missing in do-while macro"))
  (let ((beg-tag (gensym))
	(body2 (butlast (butlast body)))
	(while-test (car (last body))))
    `(tagbody
	,beg-tag
	,@body2
	(when ,while-test (go ,beg-tag)))))

(defun SHOW-do-while ()
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((a 0))
        (do-while
            (incf a)
          :while (<= (* a a) 10))
        a)))
;; returns 4

(defmacro aprogn (&rest args)
  "Anaphoric progn.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type cons args))
  (macrolet ((%alambda (parms &body body)
               "Anaphoric lambda."
               `(labels ((self ,parms ,@body))
                  #'self)))             ; end of macrolet definitions
    (funcall (%alambda (args)
                       (declare (type cons args))
	               (case (length args)
	                 (0 nil)
	                 (1 (car args))
	                 (t `(let ((it ,(car args)))
		               ,(self (cdr args))))))
             args)))
;; See "On Lisp" book

(defun SHOW-aprogn ()
  (aprogn 5
          (+ 1 it)
          (* 3 it)))
;; returns 18

(defmacro ablock (tag &rest args)
  "Anaphoric block.
Source : On Lisp"
  (declare (type cons args))
  (macrolet ((%alambda (parms &body body)
               "Anaphoric lambda."
               `(labels ((self ,parms ,@body))
                  #'self))) ; end of macrolet definitions
      `(block ,tag
         ,(funcall (%alambda (args)
                             (declare (type cons args))
                             (case (length args)
		               (0 nil)
		               (1 (car args))
		               (t `(let ((it ,(car args)))
			             ,(self (cdr args))))))
	           args))))
;; See "On Lisp" book

(defun SHOW-ablock ()
  (ablock it
          (dotimes (i 10)
            (when (= i 5)
              (return-from it i))))) ; it = current block
; returns 5

(defmacro setf-min (x y)
  "Put at place X the min of X and Y.
(v1 available in occisn/cl-utils GitHub repository)"
  `(when (< ,y ,x) (setf ,x ,y)))

(defmacro setf-max (x y)
  "Put at place X the max of X and Y.
(v1 available in occisn/cl-utils GitHub repository)"
  `(when (> ,y ,x) (setf ,x ,y)))

(defmacro setf-min--df (x y)
  "Put at place X the min of X and Y. Y is supposed to be a double-float.
(v1 available in occisn/cl-utils GitHub repository)"
  (with-gensyms (b)
    `(let ((,b ,y))
       (declare (type double-float ,b))
       (when (< ,b ,x) (setf ,x ,b)))))

(defmacro setf-max--df (x y)
  "Put at place X the max of X and Y. Y is supposed to be a double-float.
(v1 available in occisn/cl-utils GitHub repository)"
  (with-gensyms (b)
    `(let ((,b ,y))
       (declare (type double-float ,b))
       (when (> ,b ,x) (setf ,x ,b)))))

(defmacro collecting--reversed-order (&body body)
  "Return the list of x generated by (collect1 x) within BODY.
Order is reversed (quicker).
(v1 available in occisn/cl-utils GitHub repository)"
  (let ((lst (gensym)))
    `(let ((,lst nil))
       (declare (type list ,lst))
       (labels ((collect1 (x)
                  (push x ,lst)))
         ,@body
         ,lst))))

(defun SHOW-collecting--reversed-order ()
  (collecting--reversed-order
   (loop for i of-type fixnum from -5 to 5
     when (> (* i i) 10)
       do (collect1 i))))
;; --> (5 4 -4 -5)

(defmacro collecting (&body body)
  "Return the list of x generated by (collect1 x) within BODY.
In the right order (using REVERSE, so slower than the previous macro).
(v1 available in occisn/cl-utils GitHub repository)"
  (let ((lst (gensym)))
    `(let ((,lst nil))
       (declare (type list ,lst))
       (labels ((collect1 (x)
                  (push x ,lst)))
         ,@body
         (reverse ,lst)))))

(defun SHOW-collecting ()
  (collecting
   (loop for i of-type fixnum from -5 to 5
     when (> (* i i) 10)
       do (collect1 i))))
;; --> (-5 -4 4 5)

;;; end
