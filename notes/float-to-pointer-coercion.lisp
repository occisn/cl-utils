(defpackage float-issue
  (:use :cl))

(in-package float-issue)

(declaim (optimize (debug 0) 
                   (safety 0)
                   (speed 3)))

(defconstant +nb-iterations+ 1000000000)
(declaim (type fixnum +nb-iterations+))

;;; === A: reference ===
;;; with compilation warning

(declaim (notinline sub-A))
(declaim (ftype (function (fixnum) double-float) sub-A))
(defun sub-A (i)
  (declare (type fixnum i))
  (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
         (denominator (+ 1.0d0 (* 2.0d0 i)))
         (res (/ sign denominator)))
    (declare (type double-float sign denominator res))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      res)))
;; doing float to pointer coercion (cost 13) to "<return value>"

(defun compute-pi-leibniz-A ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((add (sub-A i)))
        (declare (type double-float add))
        (incf sum add)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "pi = ~a~%" (* 4.0d0 sum))))))

;;; === B: inline

(declaim (inline sub-B))
(declaim (ftype (function (fixnum) double-float) sub-B))
(defun sub-B (i)
  (declare (type fixnum i))
  (let ((sign (if (evenp i) 1.0d0 -1.0d0))
        (denominator (+ 1.0d0 (* 2.0d0 i))))
    (declare (type double-float sign denominator))
    (the double-float (/ sign denominator))))
;; doing float to pointer coercion (cost 13) to "<return value>"

(defun compute-pi-leibniz-B ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((add (sub-B i)))
        (declare (type double-float add))
        (incf sum add)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === C: double-float returned within a cons cell

(declaim (ftype (function (fixnum) list) sub-C))
(declaim (notinline sub-C))
(defun sub-C (i)
  (declare (type fixnum i))
  (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
         (denominator (+ 1.0d0 (* 2.0d0 i)))
         (res (/ sign denominator)))
    (declare (type double-float sign denominator res))
    (list res)))
;; doing float to pointer coercion (cost 13), for:
;;       the first argument of CONS

(defun compute-pi-leibniz-C ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((add (sub-C i)))
        (declare (type list add))
        (incf sum (the double-float (car add)))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === D: sub-function modifies a cons cell

(declaim (ftype (function (fixnum cons) t) sub-D))
(declaim (notinline sub-D))
(defun sub-D (i df-cell)
  (declare (type fixnum i)
           (type cons df-cell))
  (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
         (denominator (+ 1.0d0 (* 2.0d0 i)))
         (res (/ sign denominator)))
    (declare (type double-float sign denominator res))
    (rplaca df-cell res)
    nil))
;; doing float to pointer coercion (cost 13), for:
;;       the second argument of SET-SLOT

(defun compute-pi-leibniz-D ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0)
        (df-cell (list 0.0d0)))
    (declare (type double-float sum)
             (type cons df-cell))
    (dotimes (i +nb-iterations+)
      (sub-D i df-cell)
      (incf sum (the double-float (car df-cell))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === E: double-float returned within a 1-cell array

(declaim (ftype (function (fixnum) (SIMPLE-ARRAY DOUBLE-FLOAT (1))) sub-E))
(declaim (notinline sub-E))
(defun sub-E (i)
  (declare (type fixnum i))
  (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
         (denominator (+ 1.0d0 (* 2.0d0 i)))
         (res (/ sign denominator)))
    (declare (type double-float sign denominator res))
    (make-array 1
                :element-type 'double-float
                :initial-element res)))

(defun compute-pi-leibniz-E ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((add (sub-E i)))
        (declare (type (SIMPLE-ARRAY DOUBLE-FLOAT (1)) add))
        (incf sum (the double-float (aref add 0)))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === F: sub-function modifies a 1-cell array

(declaim (ftype (function (fixnum (SIMPLE-ARRAY DOUBLE-FLOAT (1))) t) sub-F))
(declaim (notinline sub-F))
(defun sub-F (i df-cell)
  (declare (type fixnum i)
           (type (SIMPLE-ARRAY DOUBLE-FLOAT (1)) df-cell))
  (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
         (denominator (+ 1.0d0 (* 2.0d0 i)))
         (res (/ sign denominator)))
    (declare (type double-float sign denominator res))
    (setf (aref df-cell 0) res)
    nil))
;; doing float to pointer coercion (cost 13), for:
;;       the second argument of SET-SLOT

(defun compute-pi-leibniz-F ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  (let ((sum 0.0d0)
        (df-cell (make-array 1
                             :element-type 'double-float
                             :initial-element 0.0d0)))
    (declare (type double-float sum)
             (type (SIMPLE-ARRAY DOUBLE-FLOAT (1)) df-cell))
    (dotimes (i +nb-iterations+)
      (sub-F i df-cell)
      (incf sum (the double-float (aref df-cell 0))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === G: flet

(defun compute-pi-leibniz-G ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  
  
  (flet ((sub-G (i)
           (declare (type fixnum i))
           (let ((sign (if (evenp i) 1.0d0 -1.0d0))
                 (denominator (+ 1.0d0 (* 2.0d0 i))))
             (declare (type double-float sign denominator))
             (the double-float (/ sign denominator)))))

    (let ((sum 0.0d0))
      (declare (type double-float sum))
      (dotimes (i +nb-iterations+)
        (let ((add (sub-G i)))
          (declare (type double-float add))
          (incf sum add)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "pi = ~a~%" (* 4.0d0 sum))))))

;;; === H: integrated

(defun compute-pi-leibniz-H ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((sign (if (evenp i) 1.0d0 -1.0d0))
            (denominator (+ 1.0d0 (* 2.0d0 i))))
        (declare (type double-float sign denominator))
        (incf sum (/ sign denominator))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; === I: integrated with boxing/unboxing within tight loop

(defun compute-pi-leibniz-I ()
  "Compute an approximation of pi using the Leibniz formula.
   Takes ITERATIONS steps and returns a double-float."
  (let ((sum 0.0d0))
    ;; (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let ((sign (if (evenp i) 1.0d0 -1.0d0))
            (denominator (+ 1.0d0 (* 2.0d0 i))))
        ;; (declare (type double-float sign denominator))
        (incf sum (/ sign denominator))))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))

;;; end
