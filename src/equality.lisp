;;;; Utilities for equality.
;;;;
;;;; Considerations on equality predicates (`eq`, `eql`, `equal`, `equalp`, `=`).
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-equality-predicates ()
  ""
  
  ;; Rule of thumb:
  ;; --------------
  ;;
  ;; compare numbers:            type-insensitive: =
  ;;                             type-sensitive:   EQL
  ;;
  ;; compare symbols, keywords:  default EQL (or EQ)
  ;;
  ;; compare characters:         case-sensitive:   CHAR= or EQL or EQUAL
  ;;                             case-insensitive: CHAR-EQUAL or EQUALP
  ;;
  ;; compare strings:            case-sensitive:   STRING= or EQUAL
  ;;                             case-insensitive: STRING-EQUAL or EQUALP
  ;;
  ;; compare lists:              EQUAL
  ;;
  ;; compare bit-vectors:        EQUAL
  ;;
  ;; compare other arrays:       by identity: EQ or EQL
  ;;                             by content:  EQUALP (recursive, may be slow) (case-insensitive for element)
  ;;                                          or custom
  ;;                                             for numeric vectors: (every #'= float-vector1 float-vector2)
  ;;
  ;; compare structs:            by identity: EQ or EQL (check if same instance)
  ;;                             by content:  define a custom equality function
  ;;                                               since EQUAL and EQUALP don't compare struct slots by default
  ;;                                          (EQUALP works in SBCL, for this is implementation-dependant)

  ;; =
  ;; -
  ;; equality of numbers, even if not of the same type
  ;; (= 3 3.0) is true
  ;; (= #c(4 5) #c(4 5)) also

  ;; EQ
  ;; --
  ;; variables are bound to the same object (same data in memory, same memory address)
  ;; so could work also in some implementations for numbers and symbols, since usually duplicated
  ;; so unreliable for numbers and characters

  ;; EQL
  ;; ---
  ;; default equality test
  ;; EQ or same number (type sensitive) or same character (case sensitive)
  ;; does not work for strings: (eql "hello" "hello") returns nil
  ;; does not work for arrays

  ;; EQUAL
  ;; -----
  ;; print the same, recursively

  ;; EQUALP
  ;; ------
  ;; same as equal but is type-insensitive for numbers ans case-insensitive for characters and strings
  ;; OK for arrays, hashes and structures

  ;; STRING=
  ;; -------

  ;; Som ressources:
  ;; ---------------
  ;; https://eli.thegreenplace.net/2004/08/08/equality-in-lisp
  )

(defun SHOW-all-equality ()
  ""
  (SHOW-equality-predicates))

;;; end
