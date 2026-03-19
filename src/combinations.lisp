(in-package :cl-utils)

;;; ====================
;;; === COMBINATIONS ===
;;; ====================

(defmacro with-combinations-of-index ((comb-vec-symbol _of n1 k1) &body body)
  "Execute BODY for all combinations of K within |[ 0...(N-1) ]|. In BODY, these combinations are presented as an array COMB-VEC-SYMBOL of fixnum. Combinations are generated in ascending lexicographic order.

Example:
(with-combinations-of-index (comb :of 5 3)
            (print comb))
;; --> #(0 1 2) ... #(2 3 4)

Source: Rosetta code + adaptations"
  (declare (ignorable _of))
  (with-gensyms (kk m i j i2 n k outer)
    `(let* ((,n ,n1)
            (,k ,k1)
            (,comb-vec-symbol (make-array ,k1 :element-type 'fixnum)))
       (declare (type fixnum ,n ,k)
                (type (simple-array fixnum) ,comb-vec-symbol))
       (labels ((next-combination ()
                  (let ((,kk ,k)
                        (,m 0))
                    (declare (type fixnum ,kk ,m))
                    (block ,outer
                      (loop for ,i of-type fixnum from 1 do
                        (when (> ,i ,kk) (return-from ,outer nil))
                        (when (< (aref ,comb-vec-symbol (the fixnum (- ,kk ,i))) (the fixnum (- ,n ,i)))
                          (setf ,m (aref ,comb-vec-symbol (the fixnum (- ,kk ,i))))
                          (loop for ,j of-type fixnum from ,i downto 1 do
                            (incf ,m)
                            (setf (aref ,comb-vec-symbol (the fixnum (- ,kk ,j))) ,m))
                          (return-from ,outer t)))))))
         (if (and (>= ,k 0) (>= ,n ,k))
             (progn
               (loop for ,i2 of-type fixnum below ,k do (setf (aref ,comb-vec-symbol ,i2) ,i2))
               (loop do (progn ,@body)
                     while (next-combination))))))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-combinations ()
  "Demonstrate combination utilities."
  (format t "~%~%======~%=== COMBINATIONS~%======~%")
  (format t "~%--- with-combinations-of-index (5 choose 3) ---~%")
  (with-combinations-of-index (comb :of 5 3)
    (format t "~s " comb))
  (format t "~%~%"))

;;; === end
