(in-package :cl-utils)

;;; ============
;;; === LAZY ===
;;; ============

;;; *** make-ascending-sequence-with-explicit-formula ***

(defun make-ascending-sequence-with-explicit-formula (explicit-formula)
  "Returns a predicate that tests membership in an ascending sequence.
Given EXPLICIT-FORMULA (a strictly increasing function from positive integers
to fixnums), returns a closure that, when called with a fixnum NB, returns T
if NB appears in the sequence, NIL otherwise. Previously computed values are
memoized in a list for efficient repeated lookups."
  (declare (type function explicit-formula))
  (let ((last-n  0)
        (list-of-numbers '()))
    (declare (type fixnum last-n)
             (type list list-of-numbers))
    (lambda (nb)
      (declare (type fixnum nb))
      (if (and (not (null list-of-numbers)) (<= nb (the fixnum (car list-of-numbers))))
          (if (find nb list-of-numbers) t nil)
          (progn
            (loop for n of-type fixnum from (the fixnum (+ 1 last-n))
                  for new-nb of-type fixnum = (funcall explicit-formula n)
                  do (progn
                       (push new-nb list-of-numbers)
                       (incf last-n))
                  until (>= new-nb nb))
            (= nb (the fixnum (car list-of-numbers))))))))

(defun SHOW-make-ascending-sequence-with-explicit-formula ()
  "Example of usage of 'make-ascending-sequence-with-explicit-formula'."
  (labels ((triangular (n)
             (declare (type fixnum n))
             (format t "   Calculate 'triangular' for ~s~%" n)
	     (the fixnum (floor (the fixnum (* n (the fixnum (+ 1 n)))) 2))))

    (let ((triangularp (make-ascending-sequence-with-explicit-formula #'triangular)))
      (declare (type function triangularp))

      (format t "Is 3 triangular? ~s~%" (funcall triangularp 3))
      (format t "Is 15 triangular? ~s~%" (funcall triangularp 15)))))

;;; -->
#|
   Calculate 'triangular' for 1
   Calculate 'triangular' for 2
Is 3 triangular? T
   Calculate 'triangular' for 3
   Calculate 'triangular' for 4
   Calculate 'triangular' for 5
Is 15 triangular? T
|#

;;; *** make-ascending-sequence-with-explicit-formula-no-memo ***

(defun make-ascending-sequence-with-explicit-formula-no-memo (explicit-formula)
  "Like MAKE-ASCENDING-SEQUENCE-WITH-EXPLICIT-FORMULA, but without memoization.
Only the last computed value is remembered. This variant uses less memory but
only works correctly when the predicate is called with strictly increasing
values of NB."
  (declare (type function explicit-formula))
  (let ((last-n  0)
        (last-nb nil))
    (declare (type fixnum last-n)
             (type (or null fixnum) last-nb))
    (lambda (nb)
      (declare (type fixnum nb))
      (cond ((and (not (null last-nb)) (= nb last-nb)) t)
	    ((and (not (null last-nb)) (< nb last-nb)) nil)
	    (t (progn
		 (loop for n of-type fixnum from (the fixnum (+ 1 last-n))
                       for new-nb of-type fixnum = (funcall explicit-formula n)
                       do (progn
			    (setq last-nb new-nb)
			    (incf last-n))
                       until (>= new-nb nb))
		 (= nb last-nb)))))))

(defun SHOW-make-ascending-sequence-with-explicit-formula-no-memo ()
  "Example of usage of 'make-ascending-sequence-with-explicit-formula-no-memo'."
  (labels ((triangular (n)
             (declare (type fixnum n))
             (format t "   Calculate 'triangular' for ~s~%" n)
	     (the fixnum (floor (the fixnum (* n (the fixnum (+ 1 n)))) 2))))

    (let ((triangularp (make-ascending-sequence-with-explicit-formula-no-memo #'triangular)))
      (declare (type function triangularp))

      (format t "Is 3 triangular? ~s~%" (funcall triangularp 3))
      (format t "Is 15 triangular? ~s~%" (funcall triangularp 15)))))

;;; -->
#|
   Calculate 'triangular' for 1
   Calculate 'triangular' for 2
Is 3 triangular? T
   Calculate 'triangular' for 3
   Calculate 'triangular' for 4
   Calculate 'triangular' for 5
Is 15 triangular? T
|#

;;; *** SHOW-all ***

(defun SHOW-all-lazy ()
  (format t "~%=== LAZY ===~%~%")
  (SHOW-make-ascending-sequence-with-explicit-formula)
  (SHOW-make-ascending-sequence-with-explicit-formula-no-memo))

;;; end
