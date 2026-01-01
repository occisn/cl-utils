(in-package :cl-utils)

(defun vec-view-all-content (v)
  "Print all content of vector V.
(v1, available in occisn/cl-utils GitHub repository)"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "~%VECTOR VIEW ALL CONTENT~%")
    (let ((n (length v)))
      (cond ((null v)
             (format t "Null vector~%"))
            ((= n 0)
             (format t "Vector of length 0~%"))
            (t
             (format t "Length: ~s~%" n)
             (format t "Content: ~{~a~^ ~}~%" (loop for i across v collect i)))))))

(defun SHOW-vec-view-all-content ()
  (let ((v (make-array 8 :initial-contents '(1 2 3 4 5 6 7 8))))
    (vec-view-all-content v)))

(defun vec-preview (v)
  "Preview the content of vector V.
(v1, available in occisn/cl-utils GitHub repository)"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (format t "~%VECTOR PREVIEW~%")
    (let ((n (length v)))
      (cond ((null v)
             (format t "Null vector~%"))
            ((= n 0)
             (format t "Vector of length 0~%"))
            ((<= n 6)
             (format t "Length: ~s~%" n)
             (format t "Content: ~{~a~^ ~}~%" (loop for i across v collect i))
             )
            (t
             (format t "Length: ~s~%" n)
             (format t "3 first: ~a ~a ~a~%" (aref v 0) (aref v 1) (aref v 2))
             (format t "3 last:  ~a ~a ~a~%" (aref v (- n 3)) (aref v (- n 2)) (aref v (- n 1))))))))

(defun SHOW-vec-preview ()
  (let ((v (make-array 8 :initial-contents '(1 2 3 4 5 6 7 8))))
    (vec-preview v)))

(declaim (ftype (function (simple-vector) (simple-array fixnum)) unliteral--fixnum-vector))
(defun unliteral--fixnum-vector (vec)
  "Return an unliteral version of fixnum vector VEC.
(v1, available in occisn/cl-utils GitHub repository)"
    (declare (type simple-vector vec))
    (make-array (length vec) :element-type 'fixnum :initial-contents vec))

(defun SHOW-unliteral--fixnum-vector ()
  (format t "unliteral--fixnum-vector: ~A~%" (unliteral--fixnum-vector #(1 2 3 4 5 6))))

(defun SHOW-arrays-and-vectors ()
  (format t "~%~%======~%=== ARRAYS-AND-VECTORS~%======~%")
  (SHOW-vec-view-all-content)
  (SHOW-vec-preview)
  (format t "~%")
  (SHOW-unliteral--fixnum-vector))

;;; end
