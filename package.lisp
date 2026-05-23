(defpackage cl-utils
  (:use cl)
  (:export
   ;; arrays and vectors:
   vec-view-all-content
   vec-preview
   unliteral--fixnum-vector
   ;; association lists:
   ;;   (void)
   ;; bit-vectors:
   fixnum->bit-vector
   bit-vector->fixnum
   bit-vector-logcount
   ;; booleans:
   boolean-value
   ;; compare-durations:
   start-up-1
   compare-durations
   plot-basic
   plot-cumulated
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
   ;; format:
   ;;    (void)
   ;; functions:
   ;;    (void)
   ;; hash tables:
   ;;    (void)
   ;; high-order-functions:
   reduce-recursive-sequence
   all-which
   first-which
   maximizing--fixnum
   minimizing--fixnum
   maximizing--bigint
   minimizing--bigint
   maximizing--rational
   minimizing--rational
   maximizing--df
   minimizing--df
   max1D
   min1D
   max2D
   min2D
   ;; lazy:
   make-ascending-sequence-with-explicit-formula
   make-ascending-sequence-with-explicit-formula-no-memo
   ;; images:
   ;;    (void)
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
   shuffle
   sublist-knowing-indexes-as-list
   new-random-fixnum-list
   fixnump
   list-of-fixnums-p
   list-of-fixnums
   double-float-p
   list-of-double-floats-p
   list-of-double-floats
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
   with-collector
   with-collector--reversed-order
   ;; math-functions:
   +epsilon8+
   +epsilon12+
   +epsilon30+
   +epsilon60+
   +epsilon98+
   +1-over-12+
   +1-over-360+
   +1-over-1260+
   +1-over-1680+
   +ln-2pi-over-2+
   +1-over-sqrt-2pi+
   legendre1
   legendre2
   legendre3
   %normal-cdf-into
   normal-cdf-m
   %lgamma-into
   lgamma-m
   %ibeta-into
   ibeta-m
   %igamma-into
   igamma-m
   %f-cdf-into
   f-cdf-m
   ;; mails:
   send-mail-via-gmail
   ;; memoization:
   labels-memo-ht
   labels-memo-array
   labels-memo-mix
   ;; measure-duration:
   SHOW-measure-duration
   SHOW-benchmark-5-times-A
   SHOW-benchmark-5-times-B
   with-timing
   ;; numbers-doubles:
   type-df-box
   make-box
   value-of
   put-into
   type-df-3box
   make-3box
   value1-of3
   value2-of3
   value3-of3
   put3-into
   %string-to-doublefloat-into
   string-to-doublefloat-m
   %elements-are-doublefloats
   type-list-of-doublefloats
   random-dfvec
   vec-new-chirp
   vec-new-creneau
   %vec-mean-into
   vec-mean-m
   %vec-variance-into
   vec-variance-m
   %vec-highest-into
   vec-highest-m
   %vec-lowest-into
   vec-lowest-m
   %variance-of-log-into
   variance-of-log-m
   %vec-SORTED-median-into
   vec-SORTED-median-m
   %vec-SORTED-quartiles-into
   vec-SORTED-quartiles-m
   %vec-SORTED-centile-into
   vec-SORTED-centile-m
   ;; numbers-integers:
   +square-root-of-5+
   +phi+
   +phi2+
   power
   power--bigint
   floor-to-power-of-10
   ceiling-to-power-of-10
   concatenate-numbers
   fibonacci-through-iteration
   fibonacci-through-explicit-formula
   non-decreasing-p
   group-factors
   iota
   make-list-within-range
   new-sequence-fixnum-vector
   new-random-fixnum-vector
   fact
   fact--bigint
   fact-0-9
   combin
   mod-incf
   mod-expt--fixnum
   char->digit
   last-digit
   first-digit
   for-successive-digits-in-reverse-order
   number->digits
   number->digits--bigint
   nth-digit
   number->vector-of-digits
   number->existing-vector-of-digits
   number->digits-set
   number->digits-set--bigint
   list-of-digits->number
   list-of-digits->number--bigint
   vector-of-digits->number
   number->sorted-digits
   first-digits
   first-digits--bigint
   nb-digits
   nb-digits--bigint
   sum-of-digits
   sum-of-digits--bigint
   product-of-digits
   contains-all-digits-at-least-once-p
   contains-all-digits-at-least-once-p--bigint
   replace-nth-digit
   replace-digits
   pandigitalp
   reverse-number--fixnum
   reverse-number--bigint
   sum-of-multiples-below
   sum-of-multiples-below--bigint
   gcd--2fixnum
   is-divisible-by-m
   divides-m
   coprimes-p
   coprimes-p-m
   totient
   totients-from-1-to-n
   totient-summatory
   perfect-square-p
   perfect-square-p-m
   perfect-square-p-specific
   perfect-cube-p-specific
   string-to-integer-list
   integer->english
   roman->integer
   integer->roman
   ;; numbers-integers-french:
   en-toutes-lettres
   ;; numbers-integers-primes:
   largest-prime-factor
   primep
   next-prime
   nth-prime
   primes-below-as-list
   primes-below-as-vector
   primep-vector-below
   prime-decomposition-grouped
   prime-divisors
   list-of-divisors
   nb-of-divisors
   list-of-proper-divisors
   sum-of-proper-divisors
   abundantp
   totient-below
   ;; numbers-rationals:
   convert-to-proper-fraction-m
   length-of-recurring-cycle
   ;; continued-fractions:
   rational-to-continued-fraction
   integer-sqrt-to-continued-fraction
   length-continued-fraction-isqrt
   with-successive-convergents
   solve-pell-equation
   ;; farey-sequences:
   with-successive-farey
   farey-immediately-on-left-of
   farey-length
   ;; sorting:
   vec-qsortd-doublefloat
   vec-qsortds-doublefloat-slave-doublefloat
   vec-qsortdsi-doublefloat-slave-fixnum
   vec-qsortssi-singlefloat-slave-fixnum
   vec-qsortisi-fixnum-slave-fixnum
   ;; permutations:
   permute-randomly-fixnum-array-in-place
   permute-randomly-doublefloat-array-in-place
   with-random-fixnum-permutations
   with-random-doublefloat-permutations
   rotate-randomly-doublefloat-array-in-place
   next-distinct-lexicographic-permutation--string
   next-distinct-lexicographic-permutation--fixnum-vector
   with-permutations--fixnum-list
   with-distinct-rotated-numbers
   list-of-distinct-rotated-numbers
   with-permutations-of-digits
   with-permutations-of-digits-no-leading-zero
   have-permutated-digits-p
   ;; combinations:
   with-combinations-of-index
   ;; search:
   binary-search--with-initial-bounds
   binary-search
   ;; gnuplot:
   gnuplot-plot-line-chart
   gnuplot-chart-one-bar-chart
   ;; console-charts:
   +default-console-width+
   +default-console-height+
   console-quick-bar-chart-from-lists
   console-quick-bar-chart-from-vectors
   console-quick-line-chart
   console-quick-scatter-plot-xs-ys
   console-quick-histogram
   ;; os-interaction-windows (Windows-specific):
   open-html-file-with-default-browser
   open-url-with-default-browser
   +field-delimiter-for-export-to-clipboard+
   convert-double-float-to-string-ready-for-clipboard-towards-Excel
   convert-single-float-to-string-ready-for-clipboard-towards-Excel
   copy-string-to-clipboard
   with-export-to-clipboard
   ;; optimization:
   fixnum-range-double-float
   round-of-df
   positive-fixnum-range-double-float
   floor-of-positive-df
   ;; property lists:
   ;;   (void)
   ;; sexp:
   sexp-replace-sexp-beginning-by
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
   ;; tco:
   ;;    (void)
   ;; trampoline:
   trampoline
   ;; triangles:
   triangle-to-2d-array
   ;; types:
   ;;    (void)
   ;; variables:
   ;;    (void)
   ;; ollama:
   call-ollama1
   call-ollama2
   ;; parallelism:
   nb-cores
   with-parallelism
   show-kernel-info
   ploop--based-on-pmap
   ploop--throwable-threads
   ploop--reusable-threads
   pfor-by-blocks-with-pmap
   p-first-which
   p-maximizing--based-on-pmap
   p-minimizing--based-on-pmap
   p-maximizing--throwable-threads
   p-minimizing--throwable-threads
   p-maximizing-by-blocks-with-pmap--fixnum
   p-minimizing-by-blocks-with-pmap--fixnum
   p-maximizing-by-blocks-with-pmap--rational
   p-minimizing-by-blocks-with-pmap--rational
   p-maximizing-by-blocks-with-pmap--df
   p-minimizing-by-blocks-with-pmap--df
   ;; web:
   web-redirect-p)

  ;; end
  )
