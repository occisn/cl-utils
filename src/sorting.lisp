;;;; Utilities for sorting.

(in-package :cl-utils)

;;; ===============
;;; === SORTING ===
;;; ===============

(defun vec-qsortd-doublefloat (data &key (first-index 0) (last-index -1))
  "Sort double-float vector DATA in place, from FIRST-INDEX (default: 0) to LAST-INDEX included
(default: -1, which means the end of the vector).

Source: Timothy Masters' QSORTD.CPP"
  (declare (type (simple-array double-float (*)) data)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- (length data) 1))))
  (let ((lower1 0)
        (upper1 0)
        (ftemp 0.0d0)
        (split 0.0d0)
        (mid-index (the fixnum (floor (the fixnum (+ first-index last-index)) 2))))
    (declare (type fixnum lower1 upper1 mid-index)
             (type double-float ftemp split))
    (setq split (aref data mid-index)
          lower1 first-index
          upper1 last-index)
    (loop do
      (while1 (> split (aref data lower1)) (incf lower1))
      (while1 (< split (aref data upper1)) (decf upper1))
      (if (= lower1 upper1)
          (progn (incf lower1) (decf upper1))
          (when (< lower1 upper1)
            (setq ftemp (aref data lower1))
            (setf (aref data lower1) (aref data upper1))
            (incf lower1)
            (setf (aref data upper1) ftemp)
            (decf upper1)))
          while (<= lower1 upper1))
    (when (< first-index upper1) (vec-qsortd-doublefloat data :first-index first-index :last-index upper1))
    (when (< lower1 last-index) (vec-qsortd-doublefloat data :first-index lower1 :last-index last-index))))

(defun vec-qsortds-doublefloat-slave-doublefloat (data slave &key (first-index 0) (last-index -1))
  "Sort double-float vector DATA in place, from FIRST-INDEX (default: 0) to LAST-INDEX included
(default: -1, which means the end of the vector).
Double-float vector SLAVE is sorted accordingly.

Source: Timothy Masters' QSORTD.CPP"
  (declare (type (simple-array double-float (*)) data slave)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- (length data) 1))))
  (let ((lower1 0)
        (upper1 0)
        (ftemp 0.0d0)
        (split 0.0d0)
        (mid-index (the fixnum (floor (the fixnum (+ first-index last-index)) 2))))
    (declare (type fixnum lower1 upper1 mid-index)
             (type double-float split ftemp))
    (setq split (aref data mid-index)
          lower1 first-index
          upper1 last-index)
    (loop do
      (while1 (> split (aref data lower1)) (incf lower1))
      (while1 (< split (aref data upper1)) (decf upper1))
      (if (= lower1 upper1)
          (progn (incf lower1) (decf upper1))
          (when (< lower1 upper1)
            (setf ftemp (aref slave lower1)
                  (aref slave lower1) (aref slave upper1)
                  (aref slave upper1) ftemp
                  ftemp (aref data lower1)
                  (aref data lower1) (aref data upper1)
                  lower1 (+ lower1 1)
                  (aref data upper1) ftemp
                  upper1 (- upper1 1))))
          while (<= lower1 upper1))
    (when (< first-index upper1) (vec-qsortds-doublefloat-slave-doublefloat data slave :first-index first-index :last-index upper1))
    (when (< lower1 last-index) (vec-qsortds-doublefloat-slave-doublefloat data slave :first-index lower1 :last-index last-index))))

(defun vec-qsortdsi-doublefloat-slave-fixnum (data slave &key (first-index 0) (last-index -1))
  "Sort double-float vector DATA in place, from FIRST-INDEX (default: 0) to LAST-INDEX included
(default: -1, which means the end of the vector).
Fixnum vector SLAVE is sorted accordingly.

Source: Timothy Masters' QSORTD.CPP"
  (declare (type (simple-array double-float (*)) data)
           (type (simple-array fixnum (*)) slave)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- (length data) 1))))
  (let ((lower1 0)
        (upper1 0)
        (ftemp 0.0d0)
        (split 0.0d0)
        (itemp 0)
        (mid-index (the fixnum (floor (the fixnum (+ first-index last-index)) 2))))
    (declare (type fixnum lower1 upper1 mid-index itemp)
             (type double-float split ftemp))
    (setq split (aref data mid-index)
          lower1 first-index
          upper1 last-index)
    (loop do
      (while1 (> split (aref data lower1)) (incf lower1))
      (while1 (< split (aref data upper1)) (decf upper1))
      (if (= lower1 upper1)
          (progn (incf lower1) (decf upper1))
          (when (< lower1 upper1)
            (setf itemp (aref slave lower1)
                  (aref slave lower1) (aref slave upper1)
                  (aref slave upper1) itemp
                  ftemp (aref data lower1)
                  (aref data lower1) (aref data upper1)
                  lower1 (+ lower1 1)
                  (aref data upper1) ftemp
                  upper1 (- upper1 1))))
          while (<= lower1 upper1))
    (when (< first-index upper1) (vec-qsortdsi-doublefloat-slave-fixnum data slave :first-index first-index :last-index upper1))
    (when (< lower1 last-index) (vec-qsortdsi-doublefloat-slave-fixnum data slave :first-index lower1 :last-index last-index))))

(defun vec-qsortssi-singlefloat-slave-fixnum (data slave &key (first-index 0) (last-index -1))
  "Sort single-float vector DATA in place, from FIRST-INDEX (default: 0) to LAST-INDEX included
(default: -1, which means the end of the vector).
Fixnum vector SLAVE is sorted accordingly."
  (declare (type (simple-array single-float) data)
           (type (simple-array fixnum (*)) slave)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- (length data) 1))))
  (let ((lower1 0)
        (upper1 0)
        (ftemp 0.0)
        (split 0.0)
        (itemp 0)
        (mid-index (the fixnum (floor (the fixnum (+ first-index last-index)) 2))))
    (declare (type fixnum lower1 upper1 mid-index itemp)
             (type single-float split ftemp))
    (setq split (aref data mid-index)
          lower1 first-index
          upper1 last-index)
    (loop do
      (while1 (> split (aref data lower1)) (incf lower1))
      (while1 (< split (aref data upper1)) (decf upper1))
      (if (= lower1 upper1)
          (progn (incf lower1) (decf upper1))
          (when (< lower1 upper1)
            (setf itemp (aref slave lower1)
                  (aref slave lower1) (aref slave upper1)
                  (aref slave upper1) itemp
                  ftemp (aref data lower1)
                  (aref data lower1) (aref data upper1)
                  lower1 (+ lower1 1)
                  (aref data upper1) ftemp
                  upper1 (- upper1 1))))
          while (<= lower1 upper1))
    (when (< first-index upper1) (vec-qsortssi-singlefloat-slave-fixnum data slave :first-index first-index :last-index upper1))
    (when (< lower1 last-index) (vec-qsortssi-singlefloat-slave-fixnum data slave :first-index lower1 :last-index last-index))))

(defun vec-qsortisi-fixnum-slave-fixnum (data slave &key (first-index 0) (last-index -1))
  "Sort fixnum vector DATA in place, from FIRST-INDEX (default: 0) to LAST-INDEX included (default:
-1, which means the end of the vector).
Fixnum vector SLAVE is sorted accordingly."
  (declare (type (simple-array fixnum (*)) data slave)
           (type fixnum first-index last-index))
  (when (< last-index 0) (setq last-index (the fixnum (- (length data) 1))))
  (let ((lower1 0)
        (upper1 0)
        (ftemp 0)
        (split 0)
        (itemp 0)
        (mid-index (the fixnum (floor (the fixnum (+ first-index last-index)) 2))))
    (declare (type fixnum lower1 upper1 mid-index itemp split ftemp))
    (setq split (aref data mid-index)
          lower1 first-index
          upper1 last-index)
    (loop do
      (while1 (> split (aref data lower1)) (incf lower1))
      (while1 (< split (aref data upper1)) (decf upper1))
      (if (= lower1 upper1)
          (progn (incf lower1) (decf upper1))
          (when (< lower1 upper1)
            (setf itemp (aref slave lower1)
                  (aref slave lower1) (aref slave upper1)
                  (aref slave upper1) itemp
                  ftemp (aref data lower1)
                  (aref data lower1) (aref data upper1)
                  lower1 (+ lower1 1)
                  (aref data upper1) ftemp
                  upper1 (- upper1 1))))
          while (<= lower1 upper1))
    (when (< first-index upper1) (vec-qsortisi-fixnum-slave-fixnum data slave :first-index first-index :last-index upper1))
    (when (< lower1 last-index) (vec-qsortisi-fixnum-slave-fixnum data slave :first-index lower1 :last-index last-index))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-sorting ()
  "Demonstrate sorting utilities."
  (format t "~%~%======~%=== SORTING~%======~%")
  (format t "~%--- vec-qsortd-doublefloat ---~%")
  (let ((v (random-dfvec 10 :limit 100.0d0)))
    (format t "before: ~s~%" v)
    (vec-qsortd-doublefloat v)
    (format t "after:  ~s~%" v))
  (format t "~%"))

;;; === end
