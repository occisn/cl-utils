# cl-utils

Personal utilities for Common Lisp.

Some functions are specific to SBCL.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

When relevant, functions and macros are illustrated with `SHOW-xxx`.

The function `(SHOW-all-cl-utils)` defined in `_show-all.lisp` file executes most of the `SHOW-xxx` functions one after the other.

A test suite is proposed. In addition to traditional tests, when several implementations of the same function are proposed, tests files may *(i)* check that the various implementations yield the same results and *(ii)* compare speed (`asd` files specify `(speed 3)`).

File **arrays-and-vectors**  
   - general considerations  
   - functions `vec-view-all-content` and `vec-preview`  
   - function `unliteral--fixnum-vector`  
   - in the tests: a benchmark showing that `svref` does not bring extra performance (at least with `(speed 3)`)  
   - bit-vectors: see specific file below
   
File **association-lists**  
   - useful standard functions

File **bit-vectors**  
   - functions `fixnum->bit-vector` and `bit-vector->fixnum`  
   - function `bit-vector-logcount`  
   - in the tests: a benchmark showing that, in order to reset a bit vector to zero, it is less costly to create a new one than to set each bit to zero individually.

File **booleans**  
   - macro `boolean-value`

File **compare-durations**
   - function `start-up-1`
   - macro `compare-durations`
   - functions `plot-basic` and `plot-cumulated` (gnuplot-based benchmark plotting)
   
File **dates-and-times**  
   - function `universal-time-to-YYYYMMDD-HHMMSS`  
   - function `get-current-YYYYMMDD-HHMMSS`  
   - functions `convert-int-YYYYMMDD-and-HHMMSS-to-universal-time` and `convert-int-YYYYMMDD-to-universal-time`  
   - function `universal-time-to-YYYY-MM-DD`  
   - functions `pretty-print-universal-time-as-full-date-time`, `pretty-print-universal-time-as-long-date` and `pretty-print-universal-time-as-short-date`  
   - function `pretty-print-time-difference`

File **debug**  
   - SLDB commands: see Emacs related hydra  
   - function `debug-output`
   
File **equality**  
   - considerations on equality predicates
   
File **ffi-with-cffi**  
   - function `SHOW-ffi-with-cffi`

File **ffi-with-sb-alien**  
   - function `SHOW-ffi-with-sb-alien`

File **files**  
   - function `random-file-name`  
   - function `reduce-for-each-line-of-file`
   
File **format**  
   - illustrations of various `format` directives

File **functions**  
   - generalities on functions  
   - are arguments passed by reference or by value?

File **hash-tables**
   - useful standard functions

File **high-order-functions**
   - function `reduce-recursive-sequence`
   - function `all-which`
   - macro `first-which`
   - macros `maximizing--fixnum`, `minimizing--fixnum`, `maximizing--bigint`, `minimizing--bigint`, `maximizing--rational`, `minimizing--rational`, `maximizing--df`, `minimizing--df`
   - functions `max1D`, `min1D`, `max2D`, `min2D`

File **images**
   - function `draw-pic-from-rgb-arrays` (uses [zpng library](https://github.com/xach/zpng))
   - function `draw-heatmap-from-values` (uses [zpng library](https://github.com/xach/zpng))

File **lazy**
   - function `make-ascending-sequence-with-explicit-formula` (memoizing membership predicate for ascending sequences)
   - function `make-ascending-sequence-with-explicit-formula-no-memo` (lightweight variant, requires increasing query order)

File **lists**
   - general considerations on lists
   - function `delete-nth` and `replace-nth`
   - function `list-preview`
   - functions `arg-min` and `arg-max`
   - function `nb-of-occurrences-of-sublist-in-list`
   - function `shuffle` (Fisher-Yates in-place shuffle)
   - function `sublist-knowing-indexes-as-list`
   - function `new-random-fixnum-list`
   - type predicates: `fixnump`, `list-of-fixnums-p`, `double-float-p`, `list-of-double-floats-p`
   - types: `list-of-fixnums`, `list-of-double-floats`
   - functions `make-circular-DO-NOT-PRINT--AND-NOT-LITERAL` and `circular-list-length`
   - function `unliteral--fixnum-list`
      
File **macros**  
   - macro `with-gensyms`  
   - macros `while`and `while1`  
   - macros `repeat-until`and `do-while`  
   - macros `aprogn` and `ablock`  
   - macros `setf-min`, `setf-max`, `setf-min--df`, `setf-max--df`  
   - macros `with-collector` and `with-collector--reversed-order`  
   - macro `let+`
   
File **mail-via-gmail**
   - function `send-mail-via-gmail`

File **math-functions**
   - constants: `+epsilon8+`, `+epsilon12+`, `+epsilon30+`, `+epsilon60+`, `+epsilon98+`
   - constants: `+1-over-12+`, `+1-over-360+`, `+1-over-1260+`, `+1-over-1680+`
   - constants: `+ln-2pi-over-2+`, `+1-over-sqrt-2pi+`
   - functions `legendre1`, `legendre2`, `legendre3` (discrete Legendre polynomial coefficients, source: Timothy Masters)
   - macro `normal-cdf-m` (normal CDF, source: Timothy Masters / Abramovitz & Stegun)
   - macro `lgamma-m` (log-Gamma function, source: Timothy Masters)
   - macro `ibeta-m` (incomplete beta function ratio, source: Timothy Masters)
   - macro `igamma-m` (complement to 1 of incomplete gamma function ratio, source: Timothy Masters)
   - macro `f-cdf-m` (CDF of F-ratio distribution, source: Timothy Masters)

File **memoization**
   - macro `labels-memo-ht` (memoization by hash table)
   - macro `labels-memo-array` (memoization by array, 1D and 2D)
   - macro `labels-memo-mix` (memoization by array below pivot, hash table above)

File **measure-duration**
   - function `SHOW-measure-duration`
   - function `SHOW-benchmark-5-times-A` and `SHOW-benchmark-5-times-B`
   - macro `with-timing`
   
File **numbers-doubles**
   - types `type-df-box` and `type-df-3box` (encapsulated double-floats to avoid float-to-pointer coercion)
   - macros `make-box`, `value-of`, `put-into` (1-value box)
   - macros `make-3box`, `value1-of3`, `value2-of3`, `value3-of3`, `put3-into` (3-value box)
   - function `%string-to-doublefloat-into` and macro `string-to-doublefloat-m` (fast string-to-double-float parsing)
   - function `%elements-are-doublefloats` and type `type-list-of-doublefloats`
   - function `random-dfvec` (random double-float vector generation)
   - functions `vec-new-chirp` and `vec-new-creneau` (signal generation)
   - macros `vec-mean-m`, `vec-variance-m`, `vec-highest-m`, `vec-lowest-m` (vector statistics)
   - macro `variance-of-log-m` (variance of log-returns, from Timothy Masters)
   - macros `vec-SORTED-median-m`, `vec-SORTED-quartiles-m`, `vec-SORTED-centile-m` (sorted vector quantiles)
   
File **numbers-integers**
   - constants: `+square-root-of-5+`, `+phi+`, `+phi2+`
   - functions `power`, `power--bigint`
   - functions `floor-to-power-of-10`, `ceiling-to-power-of-10`
   - function `concatenate-numbers`
   - functions `fibonacci-through-iteration`, `fibonacci-through-explicit-formula`
   - functions `non-decreasing-p`, `group-factors`, `iota`, `make-list-within-range`
   - functions `new-sequence-fixnum-vector`, `new-random-fixnum-vector`
   - functions `fact`, `fact--bigint`, `fact-0-9`, `combin`
   - macro `mod-incf`, function `mod-expt--fixnum`
   - functions `char->digit`, `last-digit`, `first-digit`
   - macro `for-successive-digits-in-reverse-order`
   - functions `number->digits`, `number->digits--bigint`, `nth-digit`
   - functions `number->vector-of-digits`, `number->existing-vector-of-digits`
   - functions `number->digits-set`, `number->digits-set--bigint`
   - functions `list-of-digits->number`, `list-of-digits->number--bigint`, `vector-of-digits->number`
   - functions `number->sorted-digits`, `first-digits`, `first-digits--bigint`
   - functions `nb-digits`, `nb-digits--bigint`
   - functions `sum-of-digits`, `sum-of-digits--bigint`, `product-of-digits`
   - functions `contains-all-digits-at-least-once-p`, `contains-all-digits-at-least-once-p--bigint`, `replace-nth-digit`, `replace-digits`
   - function `pandigitalp`
   - functions `reverse-number--fixnum`, `reverse-number--bigint`
   - functions `sum-of-multiples-below`, `sum-of-multiples-below--bigint`
   - function `gcd--2fixnum`, macros `is-divisible-by-m`, `divides-m`
   - function `coprimes-p`, macro `coprimes-p-m`
   - functions `totient`, `totients-from-1-to-n`, `totient-summatory`
   - functions `perfect-square-p`, `perfect-square-p-m`, `perfect-square-p-specific`, `perfect-cube-p-specific`
   - function `string-to-integer-list`
   - functions `integer->english`, `roman->integer`, `integer->roman`

File **numbers-integers-french**  
   - function `en-toutes-lettres`

File **numbers-integers-primes**
   - function `largest-prime-factor`
   - function `primep`
   - function `next-prime`
   - function `nth-prime`
   - functions `primes-below-as-list` (sieve of Eratosthenes), `primes-below-as-vector`, `primep-vector-below`
   - functions `prime-decomposition-grouped` (trial division), `prime-divisors`
   - functions `list-of-divisors`, `nb-of-divisors`, `list-of-proper-divisors`, `sum-of-proper-divisors`
   - function `abundantp`
   - function `totient-below`
   
File **continued-fractions**
   - function `rational-to-continued-fraction`
   - function `integer-sqrt-to-continued-fraction`
   - function `length-continued-fraction-isqrt`
   - macro `with-successive-convergents`
   - function `solve-pell-equation`

File **farey-sequences**
   - macro `with-successive-farey`
   - function `farey-immediately-on-left-of`
   - function `farey-length`

File **sorting**
   - function `vec-qsortd-doublefloat` (quicksort for double-float vectors)
   - function `vec-qsortds-doublefloat-slave-doublefloat` (quicksort with double-float slave)
   - function `vec-qsortdsi-doublefloat-slave-fixnum` (quicksort with fixnum slave)
   - function `vec-qsortssi-singlefloat-slave-fixnum` (quicksort for single-float with fixnum slave)
   - function `vec-qsortisi-fixnum-slave-fixnum` (quicksort for fixnum with fixnum slave)

File **permutations**
   - functions `permute-randomly-fixnum-array-in-place`, `permute-randomly-doublefloat-array-in-place`
   - macros `with-random-fixnum-permutations`, `with-random-doublefloat-permutations`
   - function `rotate-randomly-doublefloat-array-in-place`
   - functions `next-distinct-lexicographic-permutation--string`, `next-distinct-lexicographic-permutation--fixnum-vector`
   - macro `with-permutations--fixnum-list` (Heap's algorithm)
   - macro `with-distinct-rotated-numbers`, function `list-of-distinct-rotated-numbers`
   - macros `with-permutations-of-digits`, `with-permutations-of-digits-no-leading-zero`
   - function `have-permutated-digits-p`

File **combinations**
   - macro `with-combinations-of-index`

File **search**
   - function `binary-search` (binary search on sorted fixnum vectors)
   - function `binary-search--with-initial-bounds`

File **gnuplot**
   - function `gnuplot-plot-line-chart` (multi-panel line chart with Gnuplot)
   - function `gnuplot-chart-one-bar-chart` (bar chart with Gnuplot)

File **console-charts**
   - constants `+default-console-width+`, `+default-console-height+`
   - functions `console-quick-bar-chart-from-lists`, `console-quick-bar-chart-from-vectors` (horizontal bar charts with sorting options)
   - function `console-quick-line-chart` (multi-series line chart with legends)
   - function `console-quick-scatter-plot-xs-ys` (scatter plot with density indicators)
   - function `console-quick-histogram` (histogram with configurable bars and value labels)

File **os-interaction-windows** (Windows-specific)
   - functions `open-html-file-with-default-browser`, `open-url-with-default-browser`
   - function `copy-string-to-clipboard` (Win32 API via CFFI)
   - macro `with-export-to-clipboard` (structured clipboard export for Excel)
   - functions `convert-double-float-to-string-ready-for-clipboard-towards-Excel`, `convert-single-float-to-string-ready-for-clipboard-towards-Excel`

File **numbers-rationals**
   - macro `convert-to-proper-fraction-m`  
   - function `length-of-recurring-cycle`
   
File **ollama**  
   - function `call-ollama1` (requires jonathan, dexador)  
   - function `call-ollama2` (requires jonathan, dexador, bordeaux-threads)

File **optimization** (how comply with 'speed 3' compilation notes?)  
   - general considerations and methods  
   - type `fixnum-range-double-float`  
   - macro `round-of-df`  
   - type `positive-fixnum-range-double-float`  
   - macro `floor-of-positive-df`  
\+ a [note](notes/float-to-pointer-coercion.md) about "doing float to pointer coercion (cost 13) to \<return value\>" compilation notes. 

File **parallelism**
   - function `nb-cores`, macro `with-parallelism`, function `show-kernel-info`
   - loop variants: `ploop--based-on-pmap`, `ploop--throwable-threads`, `ploop--reusable-threads`, `pfor-by-blocks-with-pmap`
   - search: `p-first-which`
   - extremum variants: `p-maximizing--*` / `p-minimizing--*` (based-on-pmap, throwable-threads, by-blocks for fixnum/rational/df)
   - Leibniz series benchmarks (8 variants) comparing sequential vs parallel approaches

File **sexp**
   - function `sexp-replace-sexp-beginning-by`

File **property-lists**
   - useful standard functions
   
File **strings**  
   - useful standard functions  
   - function `string-split`  
   - function `substring-after-last`  
   - function `string-repeat-string`  
   - function `string-add-space-at-left`  
   - function `duplicate-strings-in-list`  
   - function `palindrome-string-p`  
   - function `unliteral--string`

File **symbols**  
   - general considerations on symbols, including keywords, at the end of the file  
   - function `function-to-string` and `function-to-string-no-package`
   
File **tco**  
   - illustration that (speed 3) encourages tail-call optimization
   
File **trampoline**  
   - function `trampoline` and example
   
File **triangles**  
   - function `triangle-to-2d-array`
   
File **types**  
   - general considerations on types
   
File **variables**  
   - general considerations on variables  
   - explanations about bindings  
   - lexical vs dynamic scoping
   
File **web**  
   - function `web-redirect-p`

Any comment? Open an [issue](https://github.com/occisn/cl-utils/issues), or start a discussion [here](https://github.com/occisn/cl-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).
   
(end of README)
