(in-package :cl-utils-tests)

;;; === fixnum->bit-vector

(defun %fixnum->bit-vector-v1 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let* ((len (integer-length n))
         (v (make-array len :element-type 'bit)))
    (loop repeat len
          for idx of-type fixnum from 0
          for i of-type fixnum = n then (ash i -1)
          do (setf (sbit v idx) (logand i 1)))
    v))

(defun %fixnum->bit-vector-v2 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let* ((len (integer-length n))
         (v (make-array len :element-type 'bit)))
    (loop repeat len
          for idx of-type fixnum from 0
          for i of-type fixnum = n then (ash i -1)
          do (when (not (zerop (logand i 1))) (setf (sbit v idx) 1)))
    v))

(defun %fixnum->bit-vector-v3 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let* ((v (make-array (integer-length n) :element-type 'bit)))
    (loop for idx of-type fixnum from 0
          for i of-type fixnum = n then (ash i -1)
          until (zerop i)
          do (setf (sbit v idx) (logand i 1)))
    v))

(defun %fixnum->bit-vector-v4 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let* ((v (make-array (integer-length n) :element-type 'bit)))
    (loop for idx of-type fixnum from 0
          for i of-type fixnum = n then (ash i -1)
          until (zerop i)
          do (when (not (zerop (logand i 1))) (setf (sbit v idx) 1)))
    v))

(defun %fixnum->bit-vector-v5 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let* ((len (integer-length n))
         (bits-list (loop repeat len
                          for i of-type fixnum  = n then (ash i -1)
                          collect (logand i 1))))
    (coerce bits-list 'simple-bit-vector)))

(defun %fixnum->bit-vector-v6 (n)
  "LEFT-ENDIAN
https://lisptips.com/post/44261316742/how-do-i-convert-an-integer-to-a-list-of-bits
"
  (declare (type fixnum n))
  (let ((bits-list (loop for i of-type fixnum  = n then (ash i -1)
                         until (zerop i)
                         collect (logand i 1))))
    (coerce bits-list 'simple-bit-vector)))


(test-variants-give-same-result
 fixnum->bit-vector---same-results
 (%fixnum->bit-vector-v1
  %fixnum->bit-vector-v2
  %fixnum->bit-vector-v3
  %fixnum->bit-vector-v4
  %fixnum->bit-vector-v5
  %fixnum->bit-vector-v6)
 :predicate equal
 :context (loop for i from 1000 to 2000
                do (compare1 fn i)))

(defun compare-duration-fixnum->bit-vector ()
  ""
  (cl-utils::compare-durations
   (%fixnum->bit-vector-v1
    %fixnum->bit-vector-v2
    %fixnum->bit-vector-v3
    %fixnum->bit-vector-v4
    %fixnum->bit-vector-v5
    %fixnum->bit-vector-v6)
   :context (lambda (fn)
              ;; (declare (ftype (function (fixnum) simple-bit-vector) fn))
              (loop for i of-type fixnum from 1 to 10000000
                    do (funcall fn i)))
   :start-up cl-utils::start-up-1))
;;; v5 et v6 with coerce are longer
;;; v3 is quicker; v1 et v4 juste after

;;; === bit-vector->fixnum

(defun %bit-vector->fixnum-v1 (v)
  "LEFT-ENDIAN https://stackoverflow.com/questions/62478318/sbcl-optimization-can-we-compile-an-efficient-population-count-for-bit-vectors"
  (declare (type (simple-array bit) v))
  (the fixnum (sb-kernel:%vector-raw-bits v 0)))

(defun %bit-vector->fixnum-v2 (v)
  "LEFT-ENDIAN"
  (declare (type (simple-array bit) v))
  ;; (format4 t "length(v) = ~s~%" (length v))
  (loop for i of-type fixnum from (- (length v) 1) downto 0
        for b of-type bit = (sbit v i)
        for res of-type fixnum = b then (+ (* res 2) b)
        ;; do (format4 t "i = ~s ; b = ~s ; res = ~s~%" i b res)
        finally (return res)))

(test-variants-give-same-result
 bit-vector->fixnum--same-results
 (%bit-vector->fixnum-v1
  %bit-vector->fixnum-v2)
 :predicate =
 :context (loop for i from 1000 to 2000
                for v of-type (simple-array bit) = (cl-utils::fixnum->bit-vector i)
                do (compare1 fn v)))

(defun compare-duration-bit-vector->fixnum ()
  ""
  (cl-utils::compare-durations
   (%bit-vector->fixnum-v1
    %bit-vector->fixnum-v2)
   :context (lambda (fn)
              (loop for i of-type fixnum from 1 to 10000000
                    for v of-type (simple-array bit) = (cl-utils::fixnum->bit-vector i)
                    for j of-type fixnum = (funcall fn v)
                    do (assert (= i j))))
   :start-up cl-utils::start-up-1))
;; v1 is the quickest

;;; === bit-vector-logcount

(defun %bit-vector-logcount-v1 (v)
  "https://stackoverflow.com/questions/62478318/sbcl-optimization-can-we-compile-an-efficient-population-count-for-bit-vectors"
  (declare (type (simple-array bit) v))
  (logcount (sb-kernel:%vector-raw-bits v 0)))

(defun %bit-vector-logcount-v2 (v)
  (declare (type (simple-array bit) v))
  (let ((res 0))
    (declare (type fixnum res))
    (loop for b of-type bit across v
          when (= 1 b)
            do (incf res))
    res))

(defun %bit-vector-logcount-v3 (v)
  (declare (type (simple-array bit) v))
  (let ((res 0))
    (declare (type fixnum res))
    (loop for b of-type bit across v
          when (not (zerop b))
            do (incf res))
    res))

(test-variants-give-same-result
 bit-vector-logcount--same-results
 (%bit-vector-logcount-v1
    %bit-vector-logcount-v2
    %bit-vector-logcount-v3)
 :predicate =
 :context (loop for i from 1000 to 2000
                for v of-type (simple-array bit) = (cl-utils::fixnum->bit-vector i)
                do (compare1 fn v)))

(defun compare-duration-bit-vector-logcount ()
  ""
  (cl-utils::compare-durations
   (%bit-vector-logcount-v1
    %bit-vector-logcount-v2
    %bit-vector-logcount-v3)
   :context (lambda (fn)
              (loop for i of-type fixnum from 1 to 10000000
                    for v of-type (simple-array bit) = (cl-utils::fixnum->bit-vector i)
                    do (funcall fn v)))
   :start-up cl-utils::start-up-1))
;;; v1 is the quickest

;;; === Conclusion: To reset a bit vector to zero, it is less costly to create a new one than to set each bit to zero individually; the opposite is true for other types of vectors.

;;; see https://stackoverflow.com/questions/61372322/how-to-zerop-000-in-common-lisp

(defun %SHOW-vector-zeroisation-double-float-v1 ()
  (let* ((size 100000)
         (vec (make-array size :element-type 'double-float)))
    (declare (type fixnum size)
             (type (simple-array double-float) vec))
    (labels ((%fn1 ()
               (loop for i from 0 below size do (setf (aref vec i) 0.0d0))
               (loop repeat 3 do (setf (aref vec (random size)) 1.0d0)))
             (%fn2 ()
               (setf vec (make-array size :element-type 'double-float :initial-element 0.0d0))
               (loop repeat 3 do (setf (aref vec (random size)) 1.0d0))))
      (cl-utils::compare-durations
       (%fn1
        %fn2)
       :context (lambda (fn) (loop repeat 50000
                                   do (funcall fn)))
       :start-up cl-utils::start-up-1))))
;; fn1 is two times quicker

(defun %SHOW-vector-zeroisation-boolean-v1 ()
  (let* ((size 100000)
         (vec (make-array size :element-type 'boolean :initial-element nil)))
    (declare (type fixnum size)
             (type (simple-array boolean) vec))
    (labels ((%fn1 ()
               (loop for i from 0 below size do (setf (aref vec i) nil))
               (loop repeat 3 do (setf (aref vec (random size)) t)))
             (%fn2 ()
               (setf vec (make-array size :element-type 'boolean :initial-element nil))
               (loop repeat 3 do (setf (aref vec (random size)) t))))
      (cl-utils::compare-durations
       (%fn1
        %fn2)
       :context (lambda (fn) (loop repeat 50000
                                   do (funcall fn)))
       :start-up cl-utils::start-up-1))))
;; fn1 is four times quicker

(defun %SHOW-vector-zeroisation-fixnum-v1 ()
  (let* ((size 100000)
         (vec (make-array size :element-type 'fixnum)))
    (declare (type fixnum size)
             (type (simple-array fixnum) vec))
    (labels ((%fn1 ()
               (loop for i from 0 below size do (setf (aref vec i) 0))
               (loop repeat 3 do (setf (aref vec (random size)) 1)))
             (%fn2 ()
               (setf vec (make-array size :element-type 'fixnum :initial-element 0))
               (loop repeat 3 do (setf (aref vec (random size)) 1))))
      (cl-utils::compare-durations
       (%fn1
        %fn2)
       :context (lambda (fn) (loop repeat 50000
                                   do (funcall fn)))
       :start-up cl-utils::start-up-1))))
;; fn1 is two times quicker

(defun %SHOW-vector-zeroisation-bit-v1 (&optional (nb-iter 500) (size 1000000)) ; 1M
  (declare (type fixnum size nb-iter))
  (let* ((vec (make-array size :element-type 'bit))
         (nb-of-ones (floor size 5))) ; 1 bit sur 5
    (declare (type fixnum nb-of-ones)
             (type (simple-array bit) vec))
    (labels ((%fn1 ()
               (loop for i from 0 below size do (setf (sbit vec i) 0))
               (loop repeat nb-of-ones do (setf (sbit vec (random size)) 1)))
             (%fn2 ()
               (setf vec (make-array size :element-type 'bit :initial-element 0))
               (loop repeat nb-of-ones do (setf (sbit vec (random size)) 1))))
      (cl-utils::compare-durations
       (%fn1
        %fn2)
       :context (lambda (fn) (loop repeat nb-iter
                                   do (funcall fn)))
       :start-up cl-utils::start-up-1))))
;; fn2 a little quicker

;;; === end
