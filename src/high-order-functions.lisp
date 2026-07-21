;;;; Utilities for high order functions.

(in-package :cl-utils)


;;; ===
;;; ==============
;;; === REDUCE ===
;;; ==============
;;; ===

(defun reduce-recursive-sequence (&key (from 0) (rec-fn #'1+) (while0 (lambda (x) (declare (ignore x)) t)) (fn #'identity) aggregate-fn)
  (declare (type function aggregate-fn rec-fn while0 fn))
  "Reduce function AGGREGATE-FN on each term of the recursive sequence starting at START (default:
1) and built with REC (default: #'1+), processed by FN (default: #'identity), while WHILE0 condition
is met.

Example: see below."
  (loop for current-value = (funcall fn from)
          then (funcall aggregate-fn current-value (funcall fn next0))
        for next0 = from then (funcall rec-fn next0)
        while (funcall while0 next0)
        finally (return current-value)))

(defun SHOW-reduce-recursive-sequence ()
  "Example of usage of 'reduce-recursive-sequence' to calculate the sum of integers from 1 to 100."
  (reduce-recursive-sequence
   :from 0
   :rec-fn #'1+
   :while0 (lambda (n) (declare (type fixnum n)) (<= n 100))
   :fn (lambda (n) (declare (type fixnum n)) n)
   :aggregate-fn #'+))
;;; --> 5050


;;; ===
;;; =================
;;; === ALL-WHICH ===
;;; =================
;;; ===

(defun all-which (&key
                    (from 1)
		    fn
                    (target-reached-fn (lambda (_x) (declare (ignorable _x)) t))
                    (arg-step-fn #'1+)
		    (while-arg-fn (lambda (_x) (declare (ignorable _x)) t))
		    (while-res-fn (lambda (_x) (declare (ignorable _x)) t)))
  "'arg' starts at FROM (default: 1) and is incremented through ARG-STEP-FN. The loop stops when
(WHILE-ARG-FN ARG) or (WHILE-RES-FN (FN arg)) is false. The function returns the list of (arg (FN
arg)) for which (TARGET-REACHED-FN (FN arg)) is true.

Example: see below."
  (declare (type function fn target-reached-fn arg-step-fn while-arg-fn while-res-fn))
  (let ((res '()))
    (block outer
      (loop for arg1 = from then (funcall arg-step-fn arg1)
            do
               (unless (funcall while-arg-fn arg1) (return-from outer nil))
               (let ((fn-arg1 (funcall fn arg1)))
                 (unless (funcall while-res-fn fn-arg1) (return-from outer nil))
                 (when (funcall target-reached-fn fn-arg1) (push (list arg1 fn-arg1) res)))))
    (reverse res)))

(defun SHOW-all-which ()
  "Example of usage of 'all-which'. Return integers of [-3...3] the square of which is 4."
  (all-which
   :from -3
   :fn (lambda (n) (declare (type fixnum n)) (the fixnum (* n n)))
   :target-reached-fn (lambda (m) (declare (type fixnum m)) (= m 4))
   :while-arg-fn (lambda (n) (declare (type fixnum n)) (<= n 3))))
;;; --> ((-2 4) (2 4))

;;; ===
;;; ===================
;;; === FIRST-WHICH ===
;;; ===================
;;; ===

(defmacro first-which (&key generator fn (target-reached-fn '(lambda (_x) (declare (ignorable _x)) _x)))
  "Execute the GENERATOR, which contains a 'submit', for instance: (loop for i from 1 do (submit
i)).
For each submitted value i, apply FN.
Return the first (i FN i)) for which (TARGET-REACHED-FN (FN i)) is true.


Example: see below."
  (with-gensyms (tmp1 outer)
    `(block ,outer

       ,(sexp-replace-sexp-beginning-by
         generator
         'submit
         (lambda (submit-sexp)
           (let ((i (cadr submit-sexp)))
             `(let ((,tmp1 (funcall ,fn ,i)))
                (when (funcall ,target-reached-fn ,tmp1)
                  (return-from ,outer (list ,i ,tmp1))))))))))

(defun SHOW-first-which ()
  "Example of usage of 'first-which'. Return first integer the square of which is > 100"
  (first-which
   :generator (loop for i from 1 do (submit i))
   :fn (lambda (i)
         (declare (type fixnum i))
         (the fixnum (* i i)))
   :target-reached-fn (lambda (m)
                        (declare (type fixnum m))
                        (> m 100))))
 ;;; --> (11 121)


;;; ===
;;; ===============================
;;; === MAXIMIZING / MINIMIZING ===
;;; ===============================
;;; ===

(defmacro %maximizing-minimizing-base (predicate type sub-name &body body)
  "Generalist sub-macro used by the following specialized ones.
Execute BODY which contains a (maximize y x) or (minimize y x) sexp (SUB-NAME = maximize or
minimize).
Return the maximum/minimum value of y and the list of corresponding x.
For maximum, PREDICATE shall be >. For minimum: <.
TYPE is the type of y."
  (with-gensyms (xs-max y-max foundp)
    (let ((verbose nil))
      (when (and (>= (length (the list body)) 2)
                 (equal :verbose (car body)))
        (setq verbose (cadr body)))
      `(let ((,foundp nil)
             (,xs-max nil)
             ,(if (eq 'double-float type)
                  `(,y-max 0.0d0)
                  `(,y-max 0)))
         (declare (type boolean ,foundp)
                  (type list ,xs-max)
                  (type ,type ,y-max))
         (labels ((,sub-name (y x)
                    (declare (type ,type y))
                    (cond ((not ,foundp)
                           (progn
                             (setq ,foundp t
                                   ,xs-max (list x)
                                   ,y-max y)
                             ,(when verbose `(format t "Possible optimum: ~s @ ~s~%" y x))))

                          (,(if (eq 'fixnum type)
                                `(,predicate y ,y-max)
                                `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) (,predicate y ,y-max)))
                           (progn
                             (setq ,xs-max (list x)
                                   ,y-max y)
                             ,(when verbose `(format t "Possible optimum: ~s @ ~s~%" y x))))

                          (,(if (eq 'fixnum type)
                                `(= y ,y-max)
                                `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) (= y ,y-max)))
                           (progn
                             (push x ,xs-max)
                             ,(when verbose `(format t "Possible optimum: ~s @ ~s~%" y x)))))))
           ,@body
           (if ,foundp
               (list ,(if (eq 'double-float type)
                          `(make-array 1 :element-type 'double-float :initial-contents (list ,y-max))
                          `,y-max)
                     ,xs-max)
               nil))))))

(defmacro maximizing--fixnum (&body body)
  "Execute BODY which contains a (maximize y x) sexp. Return the maximum value of fixnum y and the
list of corresponding x."
  `(%maximizing-minimizing-base > fixnum maximize ,@body))

(defun SHOW-maximizing--fixnum ()
  "Demonstrate MAXIMIZING--FIXNUM."
  (maximizing--fixnum :verbose t
                      (loop for n from -3 to 3
                            for y = (- 5 (* n n))
                            do (maximize y n))))
;;; --> (5 (0))

(defmacro minimizing--fixnum (&body body)
  "Execute BODY which contains a (minimize y x) sexp. Return the minimum value of fixnum y and the
list of corresponding x."
    `(%maximizing-minimizing-base < fixnum minimize ,@body))

(defun SHOW-minimizing--fixnum ()
  "Demonstrate MINIMIZING--FIXNUM."
  (minimizing--fixnum :verbose t
                      (loop for n from -3 to 3
                            for y = (+ 5 (* n n))
                            do (minimize y n))))
;;; --> (5 (0))

(defmacro maximizing--bigint (&body body)
  "Execute BODY which contains a (maximize y x) sexp. Return the maximum value of integer y and the
list of corresponding x."
  `(%maximizing-minimizing-base > integer maximize ,@body))

(defmacro minimizing--bigint (&body body)
  "Execute BODY which contains a (minimize y x) sexp. Return the minimum value of integer y and the
list of corresponding x."
  `(%maximizing-minimizing-base < integer minimize ,@body))

(defmacro maximizing--rational (&body body)
  "Execute BODY which contains a (maximize y x) sexp. Return the maximum value of rational y and the
list of corresponding x."
  `(%maximizing-minimizing-base > rational maximize ,@body))

(defmacro minimizing--rational (&body body)
  "Execute BODY which contains a (minimize y x) sexp. Return the minimum value of rational y and the
list of corresponding x."
  `(%maximizing-minimizing-base < rational minimize ,@body))

(defmacro maximizing--df (&body body)
  "Execute BODY which contains a (maximize y x) sexp. Return the maximum value of double-float y and
the list of corresponding x."
  `(%maximizing-minimizing-base > double-float maximize ,@body))

(defmacro minimizing--df (&body body)
  "Execute BODY which contains a (minimize y x) sexp. Return the minimum value of double-float y and
the list of corresponding x."
  `(%maximizing-minimizing-base < double-float minimize ,@body))


;;; ===
;;; ======================
;;; ===  MAX/MIN 1D/2D ===
;;; ======================
;;; ===

(defun max1D (fn nmin nmax &key (predicate #'<) (key #'identity) (filter-on-n (lambda (x) (declare (ignore x)) t)) (filter-on-value (lambda (x) (declare (ignore x)) t)))
  "Return the maximum value, with the meaning of PREDICATE, of FN on [[NMIN, NMAX]] processed by
KEY, restricted to FILTER-ON-N and FILTER-ON-VALUE."
  (declare (type function fn predicate key filter-on-n filter-on-value)
	   (type fixnum nmin nmax))
  (let ((max-value nil)
	(max-keyed-value nil)
	(max-ns nil))
    (loop for n of-type fixnum from nmin to nmax
          for value = (funcall fn n)
          for keyed-value = (funcall key value)
          when (and (funcall filter-on-n n)
		    (funcall filter-on-value value))
            do (cond ((null max-value) ; first n
		      (setq max-value value
		            max-keyed-value keyed-value
		            max-ns (list n)))
		     ((funcall predicate max-keyed-value keyed-value) ; new max
		      (setq max-value value
		            max-keyed-value keyed-value
		            max-ns (list n)))
		     ((and (not (funcall predicate max-keyed-value keyed-value))
		           (not (funcall predicate keyed-value max-keyed-value)))
		      ;; new n for same max
		      (setq max-ns (append max-ns (list n)))))
          finally (return (list max-value max-ns)))))

(defun SHOW-max1D ()
  ""
  (max1D (lambda (n) (declare (type fixnum n)) (- 25 (the fixnum (* n n))))
         -5 5))
;; --> (25 0)

(defun min1D (fn nmin nmax &key (predicate #'<) (key #'identity) (filter-on-n (lambda (x) (declare (ignore x)) t)) (filter-on-value (lambda (x) (declare (ignore x)) t)))
  "Return the minimum value, with the meaning of PREDICATE, of FN on [[NMIN, NMAX]] processed by
KEY, restricted to FILTER-ON-N and FILTER-ON-VALUE."
  (declare (type function fn predicate key filter-on-n filter-on-value)
	   (type fixnum nmin nmax))
  (let ((min-value nil)
	(min-keyed-value nil)
	(min-ns nil))
    (loop for n of-type fixnum from nmin to nmax
          for value = (funcall fn n)
          for keyed-value = (funcall key value)
          when (and (funcall filter-on-n n)
		    (funcall filter-on-value value))
            do (cond ((null min-value) ; first n
		      (setq min-value value
		            min-keyed-value keyed-value
		            min-ns (list n)))
		     ((funcall predicate keyed-value min-keyed-value) ; new min
		      (setq min-value value
		            min-keyed-value keyed-value
		            min-ns (list n)))
		     ((and (not (funcall predicate min-keyed-value keyed-value))
		           (not (funcall predicate keyed-value min-keyed-value)))
		      ;; new n for same min
		      (setq min-ns (append min-ns (list n)))))
          finally (return (list min-value min-ns)))))

(defun SHOW-min1D ()
  ""
  (min1D (lambda (n) (declare (type fixnum n)) (the fixnum (* n n)))
         -5 5))

(defun max2D (fn xmin xmax ymin ymax &key (predicate #'<) (key #'identity) (filter-on-xy (lambda (x y) (declare (ignore x y)) t)) (filter-on-value (lambda (x) (declare (ignore x)) t)) (no-ymin nil))
  "Returns the maximum value, with the meaning of PREDICATE, of FN on [[XMIN, XMAX]] x [[YMIN,
YMAX]], processed by KEY, restricted to FILTER-ON-XY and FILTER-ON-VALUE.
  If NO-YMIN is t, YMIN = x+1."
  (declare (type function fn predicate key filter-on-xy filter-on-value)
	   (type fixnum xmin xmax ymax)
           (type (or null fixnum) ymin))
  (let ((max-value nil)
	(max-keyed-value nil)
	(max-xys nil))
    (loop for x of-type fixnum from xmin to xmax do
      (loop for y of-type fixnum from (if no-ymin (+ x 1) ymin) to ymax
	    for value = (funcall fn x y)
	    for keyed-value = (funcall key value)
       	    when (and (funcall filter-on-xy x y)
		      (funcall filter-on-value value))
	      do (cond ((null max-value) ; first xy
		        (setq max-value value
			      max-keyed-value keyed-value
			      max-xys (list (list x y))))
		       ((funcall predicate max-keyed-value keyed-value) ; new max
		        (setq max-value value
			      max-keyed-value keyed-value
			      max-xys (list (list x y))))
		       ((and (not (funcall predicate max-keyed-value keyed-value))
			     (not (funcall predicate keyed-value max-keyed-value)))
		        ;; new xy for same max
		        (setq max-xys (append max-xys (list (list x y)))))))
          finally (return (list max-value max-xys)))))

(defun SHOW-max2D ()
  ""
  (max2D (lambda (x y)
           (declare (type fixnum x y))
           (- 50 (the fixnum (+  (the fixnum (* x x)) (the fixnum (* y y))))))
         -5 5 -5 5))

(defun min2D (fn xmin xmax ymin ymax &key (predicate #'<) (key #'identity) (filter-on-xy (lambda (x y) (declare (ignore x y)) t)) (filter-on-value (lambda (x) (declare (ignore x)) t)) (no-ymin nil))
    "Returns the minimum value, with the meaning of PREDICATE, of FN on [[XMIN, XMAX]] x [[YMIN,
YMAX]], processed by KEY, restricted to FILTER-ON-XY and FILTER-ON-VALUE.
  If NO-YMIN is t, YMIN = x+1."
  (declare (type function fn predicate key filter-on-xy filter-on-value)
	   (type fixnum xmin xmax ymin ymax))
  (let ((min-value nil)
	(min-keyed-value nil)
	(min-xys nil))
    (loop for x of-type fixnum from xmin to xmax do
      (loop for y of-type fixnum from (if no-ymin (+ x 1) ymin) to ymax
	    for value = (funcall fn x y)
	    for keyed-value = (funcall key value)
	    when (and (funcall filter-on-xy x y)
		      (funcall filter-on-value value))
	      do (cond ((null min-value) ; first xy
		        (setq min-value value
			      min-keyed-value keyed-value
			      min-xys (list (list x y))))
		       ((funcall predicate keyed-value min-keyed-value) ; new min
		        (setq min-value value
			      min-keyed-value keyed-value
			      min-xys (list (list x y))))
		       ((and (not (funcall predicate min-keyed-value keyed-value))
			     (not (funcall predicate keyed-value min-keyed-value)))
		        ;; new xy for same min
		        (setq min-xys (append min-xys (list (list x y)))))))
          finally (return (list min-value min-xys)))))

(defun SHOW-min2D ()
  ""
  (min2D (lambda (x y)
           (declare (type fixnum x y))
           (the fixnum (+ (the fixnum (* x x)) (the fixnum (* y y)))))
         -5 5 -5 5))

(defun SHOW-all-high-order-functions ()
  ""
  (format t "~%~%======~%=== HIGH-ORDER-FUNCTIONS~%======~%")
  (format t "~%")
  (format t "reduce-recursive-sequence sum 1..100 = ~a~%" (SHOW-reduce-recursive-sequence))
  (format t "all-which square=4 in [-3..3] = ~a~%" (SHOW-all-which))
  (format t "first-which square>100 = ~a~%" (SHOW-first-which))
  (format t "maximizing--fixnum 5-n^2 = ~a~%" (SHOW-maximizing--fixnum))
  (format t "minimizing--fixnum 5+n^2 = ~a~%" (SHOW-minimizing--fixnum))
  (format t "max1D 25-n^2 = ~a~%" (SHOW-max1D))
  (format t "min1D n^2 = ~a~%" (SHOW-min1D))
  (format t "max2D 50-x^2-y^2 = ~a~%" (SHOW-max2D))
  (format t "min2D x^2+y^2 = ~a~%" (SHOW-min2D)))

;;; end
