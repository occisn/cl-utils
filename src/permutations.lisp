(in-package :cl-utils)

;;; ====================
;;; === PERMUTATIONS ===
;;; ====================

;;; ===
;;; === Random permutation ===
;;; ===

(declaim (inline permute-randomly-fixnum-array-in-place))
(defun permute-randomly-fixnum-array-in-place (v)
  "Randomly permute fixnum vector V in-place. Returns nil."
  (let* ((i (length v))
         (j 0))
    (declare (type fixnum i)
             (type (simple-array fixnum (*)) v))
    (while1 (> i 1)
            (setf j (random i))
            (when (>= j i) (setf j (the fixnum (1- i))))
            (decf i)
            (rotatef (aref v i) (aref v j)))))

(declaim (inline permute-randomly-doublefloat-array-in-place))
(defun permute-randomly-doublefloat-array-in-place (v length &optional (first-index 0))
  "Randomly permute double-float vector V of length LENGTH in-place, starting at optional FIRST-INDEX (default: 0)."
  (let* ((i (the fixnum (- length first-index)))
         (j 0))
    (declare (type fixnum i length first-index)
             (type (simple-array double-float (*)) v))
    (while1 (> i 1)
            (setf j (random i))
            (when (>= j i) (setf j (the fixnum (1- i))))
            (decf i)
            (rotatef (aref v (the fixnum (+ i first-index))) (aref v (the fixnum (+ j first-index)))))))

(defmacro with-random-fixnum-permutations ((permutated-array-symbol _from initial-array _times nb-permutations0) &body body)
  "Execute BODY NB-PERMUTATIONS times on a random permutation of fixnum INITIAL-ARRAY. Within BODY, this random permutation is accessible through PERMUTATED-ARRAY-SYMBOL."
  (declare (ignorable _from _times))
  (with-gensyms (i length nb-permutations)
    `(let* ((,nb-permutations ,nb-permutations0)
            (,length (length ,initial-array))
            (,permutated-array-symbol (make-array ,length :element-type 'fixnum)))
       (declare (type fixnum ,nb-permutations ,length)
                (type (simple-array fixnum) ,permutated-array-symbol))
       (loop for ,i of-type fixnum from 0 below ,length
             do (setf (aref ,permutated-array-symbol ,i) (aref ,initial-array ,i)))
       (loop repeat ,nb-permutations
             do (progn
                  (permute-randomly-fixnum-array-in-place ,permutated-array-symbol)
                  ,@body)))))

(defmacro with-random-doublefloat-permutations ((permutated-array-symbol _from initial-array _times nb-permutations0) &body body)
  "Execute BODY NB-PERMUTATIONS times on a random permutation of double-float INITIAL-ARRAY. Within BODY, this random permutation is accessible through PERMUTATED-ARRAY-SYMBOL."
  (declare (ignorable _from _times))
  (with-gensyms (i length nb-permutations)
    `(let* ((,nb-permutations ,nb-permutations0)
            (,length (length ,initial-array))
            (,permutated-array-symbol (make-array ,length :element-type 'double-float)))
       (declare (type fixnum ,length ,nb-permutations)
                (type (simple-array double-float) ,permutated-array-symbol))
       (loop for ,i of-type fixnum from 0 below ,length
             do (setf (aref ,permutated-array-symbol ,i) (aref ,initial-array ,i)))
       (loop repeat ,nb-permutations
             do (progn
                  (permute-randomly-doublefloat-array-in-place ,permutated-array-symbol ,length)
                  ,@body)))))


;;; ===
;;; === Random rotation ===
;;; ===

(declaim (inline rotate-randomly-doublefloat-array-in-place))
(defun rotate-randomly-doublefloat-array-in-place (v length work)
  "Randomly rotate double-float vector V of length LENGTH in-place, by using WORK array."
  (declare (type fixnum length)
           (type (simple-array double-float (*)) v work))
  (let ((j (random length)))
    (when (>= j length) (setf j (the fixnum (1- length))))
    (loop for i of-type fixnum from 0 below length
          do (setf (aref work i) (aref v (mod (the fixnum (+ i j)) length))))
    (loop for i of-type fixnum from 0 below length
          do (setf (aref v i) (aref work i)))))


;;; ===
;;; === Distinct lexicographic permutations ===
;;; ===

(defun next-distinct-lexicographic-permutation--string (vec cmp)
  "Return the next distinct permutation of string VEC in lexicographic order, using CMP predicate for comparison. VEC is modified.

Source: https://www.nayuki.io/page/next-lexicographical-permutation-algorithm"
  (declare (type (simple-array character (*)) vec)
           (type (function (character character) boolean) cmp))
  (macrolet ((el (i) `(aref vec ,i))
             (cmp (i j) `(funcall cmp (el ,i) (el ,j))))
    (loop with len of-type fixnum = (the fixnum (- (length vec) 1))
          for i of-type fixnum from (the fixnum (- len 1)) downto 0
          when (cmp i (the fixnum (+ 1 i)))
            do (loop for k of-type fixnum from len downto i
                     when (cmp i k) do
                       (rotatef (el i) (el k))
                       (setf k (the fixnum (+ 1 len)))
                       (loop while (< (incf i) (decf k)) do
                         (rotatef (el i) (el k)))
                       (return-from next-distinct-lexicographic-permutation--string vec)))))

(defun next-distinct-lexicographic-permutation--fixnum-vector (vec cmp)
  "Return the next distinct permutation of fixnum VEC in lexicographic order, using CMP predicate for comparison. VEC is modified."
  (declare (type (simple-array fixnum (*)) vec)
           (type (function (fixnum fixnum) boolean) cmp))
  (macrolet ((el (i) `(aref vec ,i))
             (cmp (i j) `(funcall cmp (el ,i) (el ,j))))
    (loop with len of-type fixnum = (the fixnum (- (length vec) 1))
          for i of-type fixnum from (the fixnum (- len 1)) downto 0
          when (cmp i (the fixnum (+ 1 i)))
            do (loop for k of-type fixnum from len downto i
                     when (cmp i k) do
                       (rotatef (el i) (el k))
                       (setf k (the fixnum (+ 1 len)))
                       (loop while (< (incf i) (decf k)) do
                         (rotatef (el i) (el k)))
                       (return-from next-distinct-lexicographic-permutation--fixnum-vector vec)))))


;;; ===
;;; === All permutations (Heap's algorithm) ===
;;; ===

(defmacro with-permutations--fixnum-list ((permutated-vec-symbol _from lst) &body body)
  "Execute BODY successively for each permutation of LST.
Within BODY, the permutation is accessible through PERMUTATED-VEC-SYMBOL, vector of fixnum.
There are necessarily n! permutations, even if some elements of LST are the same.
Method: Heap's algorithm."
  (declare (ignorable _from))
  (with-gensyms (vec0 n1 n2 swap sub size n i)
    `(let ((,permutated-vec-symbol (make-array (length ,lst) :element-type 'fixnum :initial-contents ,lst)))
       (declare (type (simple-array fixnum) ,permutated-vec-symbol))
       (labels ((,swap (,n1 ,n2 ,vec0)
                  (declare (type fixnum ,n1 ,n2)
                           (type (simple-array fixnum) ,vec0))
                  (rotatef (aref ,vec0 ,n1) (aref ,vec0 ,n2)))
                (,sub (,size ,n)
                  (declare (type fixnum ,size ,n))
                  (cond
                    ((= ,size 1) (progn ,@body))
                    (t (loop for ,i of-type fixnum from 0 below ,size
                             do (,sub (the fixnum (- ,size 1)) ,n)
                                (if (oddp ,size)
                                    (,swap 0 (the fixnum (- ,size 1)) ,permutated-vec-symbol)
                                    (,swap ,i (the fixnum (- ,size 1)) ,permutated-vec-symbol)))))))
         (,sub (length ,permutated-vec-symbol) (length ,permutated-vec-symbol))))))


;;; ===
;;; === Rotated numbers ===
;;; ===

(defmacro with-distinct-rotated-numbers ((m-symbol _from n0) &body body)
  "Execute BODY for all distinct rotated numbers of N, including N. Within BODY, the said numbers are accessible by M-SYMBOL.

Example: (with-distinct-rotated-numbers (m :from 123) (print m))
;; --> 123 312 231"
  (declare (ignorable _from))
  (with-gensyms (f q r n)
    `(let* ((,n ,n0)
            (,m-symbol ,n)
            (,f (floor-to-power-of-10 ,n)))
       (declare (type fixnum ,n ,m-symbol ,f))
       (repeat-until
        ,@body
        (multiple-value-bind (,q ,r) (floor ,m-symbol 10)
          (setq ,m-symbol (the fixnum (+ ,q (the fixnum (* ,r ,f))))))
        :until (= ,m-symbol ,n)))))

(defun list-of-distinct-rotated-numbers (n)
  "Return the list of numbers obtained by rotation of digits of fixnum N, including N.
For instance: 12345 --> (12345 51234 45123 34512 23451)."
  (declare (type fixnum n))
  (let ((res '()))
    (declare (type list res))
    (with-distinct-rotated-numbers (m :from n)
      (push m res))
    res))


;;; ===
;;; === Permutations of digits ===
;;; ===

(defmacro with-permutations-of-digits ((m-symbol _of n0) &body body)
  "Execute BODY for all numbers obtained by permutations of digits of N, including N. Within BODY, those numbers are accessible by M-SYMBOL."
  (declare (ignorable _of))
  (with-gensyms (v0 v w n)
    `(let* ((,n ,n0)
            (,v0 (number->vector-of-digits ,n))
            (,v (sort ,v0 #'<)))
       (declare (type fixnum ,n)
                (type (simple-array fixnum (*)) ,v0 ,v))
       (loop for ,w of-type (or null (simple-array fixnum (*))) = ,v then (next-distinct-lexicographic-permutation--fixnum-vector ,w #'<)
             while ,w
             do (let ((,m-symbol (vector-of-digits->number ,w))) ,@body)))))

(defmacro with-permutations-of-digits-no-leading-zero ((m-symbol _of n0) &body body)
  "Execute BODY for all numbers obtained by permutations of digits of N, including N. Within BODY, those numbers are accessible by M-SYMBOL.
All permutations beginning by 0 are skipped (all permutations have the same number of digits)."
  (declare (ignorable _of))
  (with-gensyms (v0 v w n)
    `(let* ((,n ,n0)
            (,v0 (number->vector-of-digits ,n))
            (,v (sort ,v0 #'<)))
       (declare (type fixnum ,n)
                (type (simple-array fixnum (*)) ,v0 ,v))
       (loop for ,w of-type (or null (simple-array fixnum (*))) = ,v then (next-distinct-lexicographic-permutation--fixnum-vector ,w #'<)
             while ,w
             when (not (= 0 (aref ,w 0)))
               do (let ((,m-symbol (vector-of-digits->number ,w))) ,@body)))))

(defun have-permutated-digits-p (n m)
  "Return t if and only if N and M are the same number, modulo a permutation of digits.
Uses a prime product trick for speed."
  (declare (type fixnum n m))
  (let ((primes10 (make-array 10 :element-type 'fixnum :initial-contents (list 2 3 5 7 11 13 17 19 23 29)))
        (tmp1 1)
        (tmp2 1))
    (declare (type (simple-array fixnum) primes10)
             (type fixnum tmp1 tmp2))
    (for-successive-digits-in-reverse-order (d :in n)
      (setf tmp1 (the fixnum (* tmp1 (aref primes10 d)))))
    (for-successive-digits-in-reverse-order (d :in m)
      (setf tmp2 (the fixnum (* tmp2 (aref primes10 d)))))
    (= tmp1 tmp2)))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-permutations ()
  "Demonstrate permutation utilities."
  (format t "~%~%======~%=== PERMUTATIONS~%======~%")
  (format t "~%--- permute-randomly-fixnum-array-in-place ---~%")
  (let ((v (make-array 6 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6))))
    (permute-randomly-fixnum-array-in-place v)
    (format t "~s~%" v))
  (format t "~%--- next-distinct-lexicographic-permutation --~%")
  (loop for i of-type fixnum from 1
        for a = (copy-seq "123") then (next-distinct-lexicographic-permutation--string a #'char<)
        while a
        do (format t "(~s) ~s  " i a))
  (format t "~%~%--- list-of-distinct-rotated-numbers ---~%")
  (format t "123 --> ~s~%" (list-of-distinct-rotated-numbers 123))
  (format t "~%"))

;;; === end
