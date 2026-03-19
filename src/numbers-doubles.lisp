(in-package :cl-utils)

;;; ===
;;; ==========================
;;; === ENCAPSULATED FLOAT ===
;;; ==========================

;;; ===
;;; === 1 value

(deftype type-df-box ()
  "Type for encapsulated double-floats."
  '(simple-array double-float (*)))

(defmacro make-box (x)
  "Create an encapsulated double-float containing X."
  `(make-array 1 :initial-contents (list ,x) :element-type 'double-float))

(defmacro value-of (one-cell-vector)
  "Returns value of encapsulated double-float ONE-CELL-VECTOR."
  `(aref ,one-cell-vector 0))

(defmacro put-into (one-cell-vector value1)
  "Puts VALUE1 into encapsulated double-float ONE-CELL-VECTOR."
  `(setf (aref ,one-cell-vector 0) ,value1))

;;; ===
;;; === 3 values

(deftype type-df-3box ()
  "Type for encapsulated double-floats (3 values)."
  '(simple-array double-float (*)))

(defmacro make-3box (x y z)
  "Create an encapsulated double-float containing X, Y and Z."
  `(make-array 3 :initial-contents (list ,x ,y ,z) :element-type 'double-float))

(defmacro value1-of3 (three-cell-vector)
  "Returns first value of encapsulated double-float THREE-CELL-VECTOR."
  `(aref ,three-cell-vector 0))

(defmacro value2-of3 (three-cell-vector)
  "Returns second value of encapsulated double-float THREE-CELL-VECTOR."
  `(aref ,three-cell-vector 1))

(defmacro value3-of3 (three-cell-vector)
  "Returns third value of encapsulated double-float THREE-CELL-VECTOR."
  `(aref ,three-cell-vector 2))

(defmacro put3-into (three-cell-vector value1 value2 value3)
  "Puts VALUE1, VALUE2 and VALUE3 into encapsulated double-float THREE-CELL-VECTOR."
  `(progn
     (setf (aref ,three-cell-vector 0) ,value1)
     (setf (aref ,three-cell-vector 1) ,value2)
     (setf (aref ,three-cell-vector 2) ,value3)))


;;; ===
;;; ================================
;;; === DOUBLE-FLOAT <--> STRING ===
;;; ================================
;;; ===

;;; === Double-floats: string to double-float ===

(declaim (inline %string-to-doublefloat-into))
(defun %string-to-doublefloat-into (box string decimal-delimiter)
  "Parse STRING into a double-float with DECIMAL-DELIMITER decimal delimiter (typically #\\. or #\\,). Store it into BOX. Faster than parse-float library."
  (declare (type type-df-box box)
           (type (simple-array character) string)
           (type character decimal-delimiter))
  (let* ((total-length (length string))
         (delimiter-position (position decimal-delimiter string))
         (res0 0.0d0))
    (declare (type double-float res0)
             (type fixnum total-length))

    (cond

      ;; case 1: no decimal delimiter... just an integer part
      ((null delimiter-position)
       (setf res0 (coerce (the fixnum (parse-integer string)) 'double-float)))

      ;; case 2: decimal delimiter is at the end ... just an integer part
      ((= total-length (the fixnum (+ 1 (the fixnum delimiter-position))))
       (setf res0 (coerce (the fixnum (parse-integer (subseq string 0 (the fixnum delimiter-position)))) 'double-float)))

      ;; now we are sure that there is a true decimal part
      (t (let* ((dp (the fixnum delimiter-position))
                (integer-part
                  (if (= 0 dp)
                      0
                      (parse-integer (subseq string 0 dp))))
                (decimal-part3
                  (loop with res of-type double-float = 0.0d0
                        for x across (subseq string (the fixnum (+ 1 dp)))
                        for mult of-type fixnum = 10 then (the fixnum (* mult 10))
                        do (setf res (+ res (/ (the fixnum (digit-char-p x)) (* 1.0d0 mult))))
                        finally (return res))))
           (declare (type fixnum integer-part))
           (setf res0
                 (+ (* 1.0d0 integer-part)
                    (the double-float decimal-part3)))))) ; end of cond
    (put-into box res0)
    nil))

(defmacro string-to-doublefloat-m (tmp-box string decimal-delimiter)
  "Parse STRING into a double-float with DECIMAL-DELIMITER decimal delimiter (typically #\\. or #\\,). Return result. Use temporary TMP-BOX. Faster than parse-float library."
  `(progn
     (%string-to-doublefloat-into ,tmp-box ,string ,decimal-delimiter)
     (value-of ,tmp-box)))


;;; ===
;;; ==============================
;;; === LISTS OF DOUBLE-FLOATS ===
;;; ==============================
;;; ===

(defun %elements-are-doublefloats (seq)
  "Return t when all elements of sequence SEQ are double-floats."
  (every #'(lambda (x) (typep x 'double-float)) seq))

(deftype type-list-of-doublefloats ()
  "Type for list of double-floats."
  '(and list (satisfies %elements-are-doublefloats)))


;;; ===
;;; ================================
;;; === VECTORS OF DOUBLE-FLOATS ===
;;; ================================
;;; ===

;;; ===
;;; ======================================
;;; === GENERATION OF SPECIFIC VECTORS ===
;;; ======================================
;;; ===

(declaim (inline random-dfvec))
(defun random-dfvec (length &key (limit 100.0d0))
  "Return a vector of length LENGTH containing double-float random numbers between 0 and LIMIT (default: 100.0d0).

Example: (random-dfvec 3 :limit 10.0d0)"
  (declare (type fixnum length)
           (type double-float limit))
  (let ((res (make-array length :element-type 'double-float)))
    (loop for i of-type fixnum from 0 below length do
      (setf (aref res i) (random limit)))
    res))

(defun vec-new-chirp (&key (flip nil) (nb-bars 1500) (short-T 40.0d0) (long-T 100.0d0))
  "Generate a double-float vector representing a chirp signal.
NB-BARS: number of bars (default: 1500)
FLIP: reverse the frequency sweep (default: nil)
SHORT-T: shortest period (default: 40.0d0)
LONG-T: longest period (default: 100.0d0)"
  (declare (type boolean flip)
           (type fixnum nb-bars)
           (type double-float short-T long-T))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((pi1 3.141592653589793d0)
           (x1500 (* nb-bars 1.0d0))
           (periods (loop for i of-type fixnum from 1 to nb-bars collect (+ short-T (* (- long-T short-T) (/ i x1500)))))
           (periods2 (if flip (reverse periods) periods))
           (sinewave1 (loop for i of-type fixnum from 1
                            for period of-type double-float in periods2
                            for inv-period of-type double-float = (/ 1.0d0 period)
                            collect (sin (the double-float (* 2.0d0 pi1 i inv-period)))))
           (sinewave2 (make-array nb-bars :element-type 'double-float :initial-contents sinewave1)))
      (declare (type double-float x1500 pi1))
      sinewave2)))

(defun vec-new-creneau (&key (mini 20.0d0) (maxi 100.0d0) (nb-bars 1500))
  "Generate a double-float vector representing a square wave ('creneau').
NB-BARS: number of bars (default: 1500)
MINI: low value (default: 20.0d0)
MAXI: high value (default: 100.0d0)"
  (declare (type fixnum nb-bars)
           (type double-float mini maxi))
  (let* ((m (the fixnum (floor nb-bars 3)))
         (values1 (loop for i of-type fixnum from 1 to nb-bars collect (cond ((<= i m) mini)
                                                                             ((<= i (the fixnum (* 2 m))) maxi)
                                                                             (t mini))))
         (values2 (make-array nb-bars :element-type 'double-float :initial-contents values1)))
    (declare (type fixnum m))
    values2))

;;; =============================
;;; === OPERATIONS ON VECTORS ===
;;; =============================

(declaim (inline %vec-mean-into))
(defun %vec-mean-into (res-box input-vec &key (first-index 0) (last-index -1))
  "Calculate the mean of INPUT-VEC from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector), and stores it into RES-BOX."
  (declare (type (simple-array double-float (*)) input-vec)
           (type type-df-box res-box)
           (type fixnum first-index last-index))
  (let ((len (length input-vec))
        (res 0.0d0))
    (declare (type fixnum len)
             (type double-float res))
    (when (< last-index 0) (setq last-index (the fixnum (- len 1))))
    (loop for j of-type fixnum from first-index to last-index
          do (setq res (+ res (aref input-vec j))))
    (setq res (/ res (the fixnum (+ (the fixnum (- last-index first-index)) 1))))
    (put-into res-box res)
    nil))

(defmacro vec-mean-m (tmp-box input-vec &key (first-index 0) (last-index -1))
  "Return the mean of INPUT-VEC from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector). Use temporary TMP-BOX."
  `(progn
     (%vec-mean-into ,tmp-box ,input-vec :first-index ,first-index :last-index ,last-index)
     (value-of ,tmp-box)))

(declaim (inline %vec-variance-into))
(defun %vec-variance-into (res-box input-vec &key (first-index 0) (last-index -1))
  "Calculate the variance of INPUT-VEC from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector), and stores it into RES-BOX."
  (declare (type (simple-array double-float (*)) input-vec)
           (type type-df-box res-box)
           (type fixnum first-index last-index))
  (let ((n 0)
        (len (length input-vec))
        (sum 0.0d0)
        (sumsq 0.0d0)
        (variance 0.0d0)
        (mean 0.0d0))
    (declare (type fixnum len n)
             (type double-float sum sumsq variance mean))
    (when (< last-index 0) (setq last-index (the fixnum (- len 1))))
    (setq n (the fixnum (+ (the fixnum (- last-index first-index)) 1)))
    (loop for j of-type fixnum from first-index to last-index
          do (incf sum (aref input-vec j))
             (incf sumsq (* (aref input-vec j) (aref input-vec j))))
    (setq mean (/ sum n)
          variance (- (/ sumsq n) (* mean mean)))
    (put-into res-box variance)
    nil))

(defmacro vec-variance-m (tmp-box input-vec &key (first-index 0) (last-index -1))
  "Return the variance of INPUT-VEC from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector). Use temporary TMP-BOX."
  `(progn
     (%vec-variance-into ,tmp-box ,input-vec :first-index ,first-index :last-index ,last-index)
     (value-of ,tmp-box)))

(declaim (inline %vec-highest-into))
(defun %vec-highest-into (res-box input-vec &key (first-index 0))
  "Calculate the highest element in double-float vector INPUT-VEC, starting at index FIRST-INDEX (default: 0), and store it into RES-BOX."
  (declare (type fixnum first-index)
           (type type-df-box res-box)
           (type (simple-array double-float (*)) input-vec))
  (let ((res (aref input-vec first-index)))
    (declare (type double-float res))
    (loop for j of-type fixnum from (the fixnum (+ first-index 1)) below (length input-vec)
          do (when (> (aref input-vec j) res) (setq res (aref input-vec j))))
    (put-into res-box res)
    nil))

(defmacro vec-highest-m (tmp-box input-vec &key (first-index 0))
  "Return the highest element in double-float vector INPUT-VEC, starting at index FIRST-INDEX (default: 0). Use temporary TMP-BOX."
  `(progn
     (%vec-highest-into ,tmp-box ,input-vec :first-index ,first-index)
     (value-of ,tmp-box)))

(declaim (inline %vec-lowest-into))
(defun %vec-lowest-into (res-box input-vec &key (first-index 0))
  "Calculate the lowest element in double-float vector INPUT-VEC, starting at index FIRST-INDEX (default: 0), and store it into RES-BOX."
  (declare (type fixnum first-index)
           (type type-df-box res-box)
           (type (simple-array double-float (*)) input-vec))
  (let ((res (aref input-vec first-index)))
    (declare (type double-float res))
    (loop for j of-type fixnum from (the fixnum (+ first-index 1)) below (length input-vec)
          do (when (< (aref input-vec j) res) (setq res (aref input-vec j))))
    (put-into res-box res)
    nil))

(defmacro vec-lowest-m (tmp-box input-vec &key (first-index 0))
  "Return the lowest element in double-float vector INPUT-VEC, starting at index FIRST-INDEX (default: 0). Use temporary TMP-BOX."
  `(progn
     (%vec-lowest-into ,tmp-box ,input-vec :first-index ,first-index)
     (value-of ,tmp-box)))

(declaim (inline %variance-of-log-into))
(defun %variance-of-log-into (res-box use-change icase length prices)
  "Calculate the variance of log-returns and store the result in RES-BOX.
PRICES: double-float vector of prices
LENGTH: window length
ICASE: current index (end of window)
USE-CHANGE: if t, use log(price_i/price_{i-1}); if nil, use log(price_i)

Source: Timothy Masters."
  (declare (type boolean use-change)
           (type fixnum icase length)
           (type (simple-array double-float (*)) prices)
           (type type-df-box res-box))
  (let* ((mean 0.0d0) (sum 0.0d0))
    (declare (type double-float mean sum))
    (if use-change
        (unless (>= icase length) (error "Should not happen"))
        (unless (>= icase (the fixnum (- length 1))) (error "Should not happen")))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from (the fixnum (+ (the fixnum (- icase length)) 1)) to icase
          with term of-type double-float = 0.0d0
          do (if use-change
                 (setq term (log (the (double-float 0.0d0) (/ (aref prices i) (aref prices (the fixnum (- i 1)))))))
                 (setq term (log (the (double-float 0.0d0) (aref prices i)))))
             (incf sum term))
    (setq mean (/ sum length))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from (the fixnum (+ (the fixnum (- icase length)) 1)) to icase
          with term of-type double-float = 0.0d0
          with log1 of-type double-float = 0.0d0
          do (if use-change
                 (setq log1 (log (the (double-float 0.0d0) (/ (aref prices i) (aref prices (the fixnum (- i 1))))))
                       term (- log1 mean))
                 (setq log1 (log (the (double-float 0.0d0) (aref prices i)))
                       term (- log1 mean)))
             (incf sum (* term term)))
    (put-into res-box (/ sum length))
    nil))

(defmacro variance-of-log-m (tmp-box use-change icase length prices)
  "Return the variance of log-returns. Use temporary TMP-BOX.
PRICES: double-float vector of prices
LENGTH: window length
ICASE: current index (end of window)
USE-CHANGE: if t, use log(price_i/price_{i-1}); if nil, use log(price_i)

Source: Timothy Masters."
  `(progn
     (%variance-of-log-into ,tmp-box ,use-change ,icase ,length ,prices)
     (value-of ,tmp-box)))

(declaim (inline %vec-SORTED-median-into))
(defun %vec-SORTED-median-into (res-box input-vec &key (first-index 0) (last-index -1) &aux (n (length input-vec)))
  "Calculate the median of a >>sorted<< double-float vector INPUT-VEC, from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector), and store it into RES-BOX."
  (declare (type (simple-array double-float (*)) input-vec)
           (type type-df-box res-box)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- n 1))))
  (let* ((nb-valid-bars (the fixnum (+ 1 (the fixnum (- last-index first-index)))))
         (k50 (the fixnum (floor (the fixnum (* nb-valid-bars 1)) 2)))
         (median (aref input-vec (the fixnum (+ first-index k50)))))
    (declare (type fixnum n nb-valid-bars k50)
             (type double-float median))
    (put-into res-box median))
  nil)

(defmacro vec-SORTED-median-m (tmp-box input-vec &key (first-index 0) (last-index -1))
  "Return the median of a >>sorted<< double-float vector INPUT-VEC, from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector). Use temporary TMP-BOX."
  `(progn
     (%vec-SORTED-median-into ,tmp-box ,input-vec :first-index ,first-index :last-index ,last-index)
     (value-of ,tmp-box)))

(declaim (inline %vec-SORTED-quartiles-into))
(defun %vec-SORTED-quartiles-into (res-box input-vec &key (first-index 0) (last-index -1) &aux (n (length input-vec)))
  "Calculate the quartiles 25, 50 and 75 of a >>sorted<< double-float vector INPUT-VEC, from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector), and store them into 3-cell RES-BOX."
  (declare (type (simple-array double-float (*)) input-vec)
           (type type-df-3box res-box)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- n 1))))
  (let* ((nb-valid-bars (the fixnum (+ 1 (the fixnum (- last-index first-index)))))
         (k25 (the fixnum (floor (the fixnum (+ nb-valid-bars 1)) 4)))
         (value25 (aref input-vec (the fixnum (+ first-index k25))))
         (k50 (the fixnum (floor (the fixnum (+ nb-valid-bars 1)) 2)))
         (value50 (aref input-vec (the fixnum (+ first-index k50))))
         (k75 (the fixnum (- nb-valid-bars 1 k25)))
         (value75 (aref input-vec (the fixnum (+ first-index k75)))))
    (declare (type fixnum n nb-valid-bars k25 k50 k75)
             (type double-float value25 value50 value75))
    (put3-into res-box value25 value50 value75))
  nil)

(defmacro vec-SORTED-quartiles-m (tmp-box input-vec &key (first-index 0) (last-index -1))
  "Return the quartiles 25, 50 and 75 of a >>sorted<< double-float vector INPUT-VEC as a 3-cell box. Use temporary TMP-BOX (type-df-3box)."
  `(progn
     (%vec-SORTED-quartiles-into ,tmp-box ,input-vec :first-index ,first-index :last-index ,last-index)
     ,tmp-box))

(declaim (inline %vec-SORTED-centile-into))
(defun %vec-SORTED-centile-into (res-box input-vec cent &key (first-index 0) (last-index -1) &aux (n (length input-vec)))
  "Calculate the centile CENT (0...100) of a >>sorted<< double-float vector INPUT-VEC, from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector), and store it into 1-cell RES-BOX."
  (declare (type (simple-array double-float (*)) input-vec)
           (type type-df-box res-box)
           (type fixnum cent n first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- n 1))))
  (let* ((nb-valid-bars (the fixnum (+ 1 (the fixnum (- last-index first-index)))))
         (k (the fixnum (floor (the fixnum (* cent (the fixnum (+ nb-valid-bars 1)))) 100)))
         (value1 (aref input-vec (the fixnum (+ first-index k)))))
    (declare (type fixnum nb-valid-bars k)
             (type double-float value1))
    (put-into res-box value1))
  nil)

(defmacro vec-SORTED-centile-m (tmp-box input-vec cent &key (first-index 0) (last-index -1))
  "Return the centile CENT (0...100) of a >>sorted<< double-float vector INPUT-VEC, from FIRST-INDEX (default: 0) to LAST-INDEX included (default: -1, which means the end of the vector). Use temporary TMP-BOX."
  `(progn
     (%vec-SORTED-centile-into ,tmp-box ,input-vec ,cent :first-index ,first-index :last-index ,last-index)
     (value-of ,tmp-box)))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-numbers-doubles ()
  "Demonstrate double-float utilities."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (format t "~%~%======~%=== NUMBERS-DOUBLES~%======~%")

  ;; Box operations
  (format t "~%--- Box operations ---~%")
  (let ((b (make-box 3.14d0)))
    (format t "make-box 3.14d0 => ~s~%" b)
    (format t "value-of => ~s~%" (value-of b))
    (put-into b 2.71d0)
    (format t "put-into 2.71d0, value-of => ~s~%" (value-of b)))

  (let ((b3 (make-3box 1.0d0 2.0d0 3.0d0)))
    (format t "make-3box 1 2 3 => ~s~%" b3)
    (format t "value1-of3 => ~s, value2-of3 => ~s, value3-of3 => ~s~%"
            (value1-of3 b3) (value2-of3 b3) (value3-of3 b3)))

  ;; String parsing
  (format t "~%--- String to double-float ---~%")
  (let ((tmp (make-box 0.0d0)))
    (format t "\"145,256\" with #\\, => ~s~%" (string-to-doublefloat-m tmp "145,256" #\,))
    (format t "\",256\" with #\\, => ~s~%" (string-to-doublefloat-m tmp ",256" #\,)))

  ;; Vector generation
  (format t "~%--- Vector generation ---~%")
  (format t "random-dfvec 5 => ~s~%" (random-dfvec 5))

  ;; Vector statistics
  (format t "~%--- Vector statistics ---~%")
  (let ((tmp (make-box 0.0d0))
        (v (make-array 4 :initial-contents (list 1.0d0 3.0d0 5.0d0 7.0d0) :element-type 'double-float)))
    (format t "vec = ~s~%" v)
    (format t "mean => ~s~%" (vec-mean-m tmp v))
    (format t "variance => ~s~%" (vec-variance-m tmp v))
    (format t "highest => ~s~%" (vec-highest-m tmp v))
    (format t "lowest => ~s~%" (vec-lowest-m tmp v)))

  (format t "~%"))

;;; === end
