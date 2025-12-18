(in-package :cl-utils)

(deftype positive-fixnum-range-double-float ()
  `(double-float
    0d0
    ,(float (1- most-positive-fixnum) 1.0d0)))

(defmacro floor-of-positive-df (df)
  "Return the floor of DF. DF must be a positive double-float within fixnum range.
Use of macro to avoid 'doing float to pointer coercion'.
(v1, available in occisn/cl-utils GitHub repository, 2025-12-18)"
  `(truncate (the positive-fixnum-range-double-float ,df)))

;; end
