(defpackage cl-utils
  (:use cl)
  (:export
   ;; arrays and vectors:
   vec-view-all-content
   vec-preview
   unliteral--fixnum-vector
   ;; bit-vectors:
   fixnum->bit-vector
   bit-vector->fixnum
   bit-vector-logcount
   ;; booleans:
   boolean-value
   ;; compare-durations:
   start-up-1
   compare-durations
   ;; dates and times:
   universal-time-to-YYYYMMDD-HHMMSS
   get-current-YYYYMMDD-HHMMSS
   convert-int-YYYYMMDD-and-HHMMSS-to-universal-time
   convert-int-YYYYMMDD-to-universal-time
   universal-time-to-YYYY-MM-DD
   pretty-print-universal-time-as-full-date-time
   pretty-print-universal-time-as-long-date
   pretty-print-universal-time-as-short-date
   pretty-print-time-difference
   ;; debug:
   debug-output
   ;; files:
   random-file-name
   reduce-for-each-line-of-file
   ;; lists:
   delete-nth
   replace-nth
   list-preview
   arg-min
   arg-max
   nb-of-occurrences-of-sublist-in-list
   make-circular-DO-NOT-PRINT--AND-NOT-LITERAL
   circular-list-length
   unliteral--fixnum-list
   ;; macros:
   with-gensyms
   while
   while1
   repeat-until
   do-while
   aprogn
   ablock
   setf-min
   setf-max
   setf-min--df
   setf-max--df
   collecting
   collect1 
   collecting--reversed-order
   ;; mails:
   send-mail-via-gmail
   ;; measure-duration
   measure-duration-example
   ;; numbers-doubles
   floor-of-positive-df
   ;; numbers-integers-french:
   en-toutes-lettres
   ;; numbers-integers-primes
   largest-prime-factor
   ;; numbers-rationals:
   convert-to-proper-fraction-m
   length-of-recurring-cycle
   ;; strings:
   string-split
   substring-after-last
   string-repeat-string
   string-add-space-at-left
   duplicate-strings-in-list
   palindrome-string-p
   unliteral--string
   ;; symbols:
   function-to-string
   function-to-string-no-package
   ;; triangles:
   triangle-to-2d-array
   ;; web:
   web-redirect-p))
