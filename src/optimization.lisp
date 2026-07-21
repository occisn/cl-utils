;;;; Utilities for optimization.
;;;;
;;;; How to comply with `(speed 3)` compilation notes. General considerations and methods, plus:

(in-package :cl-utils)

(defun SHOW-all-optimization (m n)
  "Example of methods to comply with 'speed 3' compilation notes."
  (declare (type fixnum m n))

    (format t "~%~%======~%=== OPTIMIZATION~%======~%")

  (let ((tmp 0))
    (declare (type fixnum tmp))

    ;; MUFFLING:
    ;; ---------
    ;; (locally
    ;;     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    
    ;; EXPT
    ;; ----
    (setq tmp (the fixnum (expt m (the (integer 0) (- n 2)))))
    ;; (integer 0) can be replaced by unsigned-byte

    ;; GCD and LCM
    ;; -----------
    ;; --> use muffle

    ;; CEILING
    ;; -------
    ;; (the fixnum (ceiling...
    ;; reason: The return type of ceiling in CL is always (values integer remainder) where integer is a general integer (not necessarily a fixnum)

    ;; FLOOR of double-float
    ;; ---------------------
    ;; --> see floor-of-positive-df in double-floats utils
    ;; if POSITIVE double-float:
    ;;   floor-of-positive-df below ≡ (truncate (the fixnum-range-double-float df))
    ;;   to be tested: (floor (the (double-float 0.0d0) log10n))
    ;;
    ;; BETTER: use truncate
    
    ;; ROUND of double-float
    ;; ---------------------
    ;; ROUND-OF-DF below ≡ (round (the fixnum-range-double-float param1))
    
    (format t "~%")
    (format t "tmp = ~a~%" tmp)))

(deftype fixnum-range-double-float ()
  "Type for double-float comprises into fixnum range."
  `(double-float
    ,(float most-negative-fixnum 1.0d0)
    ,(float most-positive-fixnum 1.0d0))) ; previously: (/ most-positive-fixnum 2)

(defmacro round-of-df (df)
  "Return round of positiv double-float DF."
  `(round (the fixnum-range-double-float ,df)))

(deftype positive-fixnum-range-double-float ()
  `(double-float
    0d0
    ,(float (1- most-positive-fixnum) 1.0d0)))

(defmacro floor-of-positive-df (df)
  "Return the floor of DF. DF must be a positive double-float within fixnum range.
Use of macro to avoid 'doing float to pointer coercion'.
(v1, available in occisn/cl-utils GitHub repository, 2025-12-18)"
  `(truncate (the positive-fixnum-range-double-float ,df)))

;;; end
