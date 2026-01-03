(in-package :cl-utils)

(defun SHOW-types-general ()
  ""

  ;; TYPEP

  ;; TYPE-OF

  ;; compound types:
  ;;    (or integer string)
  ;;    (and number (satisfies evenp))
  ;;    (vector integer 10) 
  ;;    etc

  ;; COMMON TYPES:
  ;; -------------
  ;;
  ;; **Number Types:**
  ;; - `integer` - whole numbers (includes `fixnum` and `bignum`)
  ;; - `ratio` - rational numbers like 2/3
  ;; - `float` - floating point (includes `short-float`, `single-float`, `double-float`, `long-float`)
  ;; - `complex` - complex numbers
  ;; - `real` - all real numbers (integers, ratios, floats)
  ;; - `number` - all numeric types

  ;; **Character and String Types:**
  ;; - `character` - single characters
  ;; - `string` - sequences of characters
  ;; - `base-string` - strings with base characters

  ;; **Sequence Types:**
  ;; - `list` - linked lists (includes `null` for empty list and `cons` for pairs)
  ;; - `vector` - one-dimensional arrays
  ;; - `bit-vector` - vectors of bits
  ;; - `array` - multi-dimensional arrays
  ;; - `sequence` - general sequence type (covers lists and vectors)

  ;; **Symbol and Package Types:**
  ;; - `symbol` - symbolic names
  ;; - `keyword` - symbols in the keyword package
  ;; - `package` - namespace containers

  ;; **Function and Stream Types:**
  ;; - `function` - callable functions
  ;; - `compiled-function` - compiled functions
  ;; - `stream` - I/O streams

  ;; **Other Common Types:**
  ;; - `t` - the universal supertype (all values are of type `t`)
  ;; - `nil` - the empty type (no values) and also the boolean false
  ;; - `boolean` - `t` or `nil`
  ;; - `hash-table` - hash tables
  ;; - `pathname` - file system paths
  ;; - `readtable` - reader syntax tables
  ;; - `random-state` - random number generator state
  )

(defun SHOW-all-types ()
  ""
  (format t "~%~%======~%=== TYPES~%======~%")
  (format t "~%")
  (SHOW-types-general))

;;; end
