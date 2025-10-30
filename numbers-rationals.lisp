(defpackage cl-utils--numbers-rationals
  (:use :cl ))

(in-package :cl-utils--numbers-rationals)

(defmacro convert-to-proper-fraction-m (a b)
  "Modify A and B to transform A/B into a proper fraction.
(v1, available in occisn/cl-utils GitHub repository)"

  (labels () ; end of labels definitions
    
    (let ((gcd (gensym)))

      `(labels ((gcd--2fixnum (a b)
                  "Calculate GCD of arguments, which are supposed to be two fixnums.
Requires SBCL.
(v1, available in occisn/cl-utils GitHub repository)"
                  (declare (type fixnum a b))
                  (cond ((eql a 0) (abs b))
                        ((eql b 0) (abs a))
                        (t (sb-kernel::fixnum-gcd a b)))))
         
         (let ((,gcd (gcd--2fixnum ,a ,b)))
           (declare (type fixnum ,gcd))
           (when (> ,gcd 1)
             (setf ,a (floor ,a ,gcd)
                   ,b (floor ,b ,gcd))))))))


(defun SHOW-convert-to-proper-fraction-m (&optional (a 50) (b 15))
  (declare (type fixnum a b))
  (format t "~s / ~s" a b)
  (convert-to-proper-fraction-m a b)
  (format t " --> ~s / ~s~%" a b))
;; 50 / 15 --> 10 / 3


(defun length-of-recurring-cycle (denom)
  "Return the length of the recurring cycle of rational 1/DENOM.
The argument is supposed to be a fixnum >= 1.

For instance :
   denom = 1 --> 0
   denom = 6 --> 1
   denom = 7 --> 6 (1/7 = 0.142857 142857 14285...)

(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum denom))

  (labels ((%divide-by-m-as-much-as-possible (n m)
             "Return the argument N, divided by M as much as possible.
For instance: 10 5 --> 2.
The argument is supposed to be a fixnum >= 0."
             (declare (type fixnum n m))
             (cond ((= n 0) 0)
                   ((= n 1) 1)
	           ((= n m) 1)
                   ((> (mod n m) 0) n)
                   (t (%divide-by-m-as-much-as-possible (the fixnum (floor n m)) m)))))
    
    (let* ((tmp1 (%divide-by-m-as-much-as-possible denom 2))
           (denom2 (%divide-by-m-as-much-as-possible tmp1 5))) ; pour être sur que le cycle démarre après la virgule
      (declare (type fixnum tmp1 denom2))
      (if (= 1 denom2)
          0
          (locally
              ;; mult9 can ve very high:
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (loop for i of-type fixnum from 1
	          for mult9 = 9 then (+ 9 (* mult9 10))
	          until (zerop (mod mult9 denom2))
	          finally (return i)))))))

;;; end
