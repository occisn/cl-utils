(defpackage cl-utils--bit-vectors
  (:use :cl ))

(in-package :cl-utils--bit-vectors)

(defun fixnum->bit-vector (n)
  "Return left-endian bit-vector representing fixnum N.
Exemple: 10 --> #*0101
(v1, available in occisn/cl-utils GitHub repository)
"
  (declare (type fixnum n))
  (let* ((v (make-array (integer-length n) :element-type 'bit)))
    (loop for idx of-type fixnum from 0
          for i of-type fixnum = n then (ash i -1)
          until (zerop i)
          do (setf (sbit v idx) (logand i 1)))
    v))
;; Inspiration: https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits

(defun bit-vector->fixnum (v)
  "Return fixnum corresponding to left-endian bit-vector V.
Exemple : #*0101 --> 10
Requires SBCL.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type (simple-array bit) v))
  (the fixnum (sb-kernel:%vector-raw-bits v 0)))
;; Inspiration: https://stackoverflow.com/questions/62478318/sbcl-optimization-can-we-compile-an-efficient-population-count-for-bit-vectors

(defun bit-vector-logcount (v)
  "Return the number of 1 in bit-vector V.
Requires SBCL.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type (simple-array bit) v))
  (logcount (sb-kernel:%vector-raw-bits v 0)))
;; Inspiration: https://stackoverflow.com/questions/62478318/sbcl-optimization-can-we-compile-an-efficient-population-count-for-bit-vectors

;;; end
