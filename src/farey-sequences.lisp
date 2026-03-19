(in-package :cl-utils)

;;; ========================
;;; === FAREY SEQUENCES  ===
;;; ========================

(defmacro with-successive-farey ((a b _order n) &body body)
  "Execute BODY for successive fractions in N-Farey sequence, including 0/1 and 1/1.
In BODY, the fractions are presented as A/B.

Example: (with-successive-farey (a b :order 100)
    (format t \"~s / ~s~%%\" a b))

This function generates Farey sequence from 2 first _adjacent_ terms.
http://www.scientificlib.com/en/Mathematics/NumberTheory/FareySequence.html"
  (declare (ignorable _order))
  (with-gensyms (c d k tmp1 tmp2 n2 ascending)
    `(let ((,n2 ,n)
           (,a -1)
           (,b -1)
           (,c -1)
           (,d -1)
           (,k -1)
           (,tmp1 -1)
           (,tmp2 -1)
           (,ascending t))
       (declare (type fixnum ,c ,d ,k ,tmp1 ,tmp2)
                (type boolean ,ascending))
       (if ,ascending
           (setq ,a 0
                 ,b 1
                 ,c 1 ,d
                 ,n2)
           (setq ,a 1
                 ,b 1
                 ,c (the fixnum (- ,n2 1))
                 ,d ,n2))
       ,@body
       (loop while (or (and ,ascending (< ,c ,n2)) (and (not ,ascending) (> ,a ,0)))
             do (setq ,k (the fixnum (floor (the fixnum (+ ,n2 ,b)) ,d))
                      ,tmp1 ,c
                      ,tmp2 ,d
                      ,c (the fixnum (- (the fixnum (* ,k ,c)) ,a))
                      ,d (the fixnum (- (the fixnum (* ,k ,d)) ,b))
                      ,a ,tmp1
                      ,b ,tmp2)
             ,@body))))

(defun farey-immediately-on-left-of (n c d)
  "Return the fraction immediately on the left of C/D in N-Farey sequence.

For instance: 8 3 7 --> (2 5)
              1000000 3 7 --> (428570 999997)

This function works via successive Farey mediants."
  (declare (type fixnum n c d))
  (let ((a 0)
        (b 1)
        (a2 -1)
        (b2 -1))
    (declare (type fixnum a b a2 b2))
    (block outer
      (loop do (setq a2 (the fixnum (+ a c))
                     b2 (the fixnum (+ b d)))
               (convert-to-proper-fraction-m a2 b2)
               (when (> b2 n) (return-from outer))
               (setq a a2
                     b b2)))
    (list a b)))

(declaim (ftype (function (fixnum) fixnum) farey-length))
(defun farey-length (n)
  "Return number of fractions in N-Farey sequence, including 0/1 and 1/1.

For instance: 8 --> 23

Uses totient summatory function."
  (declare (type fixnum n))
  (the fixnum (+ 1 (totient-summatory n))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-farey-sequences ()
  "Demonstrate Farey sequence utilities."
  (format t "~%~%======~%=== FAREY-SEQUENCES~%======~%")
  (format t "~%--- with-successive-farey (order 5) ---~%")
  (with-successive-farey (a b :order 5)
    (format t "~s/~s " a b))
  (format t "~%~%--- farey-immediately-on-left-of 8 3 7 ---~%")
  (format t "~s~%" (farey-immediately-on-left-of 8 3 7))
  (format t "~%--- farey-length 8 ---~%")
  (format t "~s~%" (farey-length 8))
  (format t "~%"))

;;; === end
