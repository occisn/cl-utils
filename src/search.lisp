(in-package :cl-utils)

;;; ==============
;;; === SEARCH ===
;;; ==============

(declaim (inline binary-search--with-initial-bounds))
(defun binary-search--with-initial-bounds (value array low high)
  "Return the index of VALUE in ARRAY through binary search, or NIL if VALUE is not in ARRAY. ARRAY should be a sorted (simple-array fixnum).
LOW and HIGH are initial index bounds.

Source: Rosetta code"
  (declare (type fixnum value low high)
           (type (simple-array fixnum) array))
  (do () ((< high low) nil)
    (let ((middle (the fixnum (floor (the fixnum (+ low high)) 2))))
      (declare (type fixnum middle))
      (cond ((> (aref array middle) value)
             (setf high (the fixnum (1- middle))))
            ((< (aref array middle) value)
             (setf low (the fixnum (1+ middle))))
            (t (return middle))))))

(defun binary-search (value array)
  "Return the index of VALUE in ARRAY through binary search, or NIL if VALUE is not in ARRAY. ARRAY should be a sorted (simple-array fixnum).

Source: Rosetta code"
  (declare (type fixnum value)
           (type (simple-array fixnum) array))
  (let ((low 0)
        (high (the fixnum (1- (length array)))))
    (declare (type fixnum low high))
    (binary-search--with-initial-bounds value array low high)))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-search ()
  "Demonstrate search utilities."
  (format t "~%~%======~%=== SEARCH~%======~%")
  (format t "~%--- binary-search ---~%")
  (format t "4 in #(1 2 3 4 5 6 7) --> index ~s~%" (binary-search 4 (make-array 7 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6 7))))
  (format t "~%"))

;;; === end
