# cl-utils

Personal utilities for Common Lisp.

Some functions are specific to SBCL.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

When relevant, functions and macros are illustrated with `SHOW-xxx`.

The function `(SHOW-all-cl-utils)` defined in `_show-all.lisp` file executes most of the `SHOW-xxx` functions one after the other.

A test suite is proposed. In addition to traditional tests, when several implementations of the same function are proposed, tests files may *(i)* check that the various implementations yield the same results and *(ii)* compare speed (`asd` files specify `(speed 3)`).

In the per-file listing below, each user-facing function is annotated:

- **[macro]** — defined with `defmacro` rather than `defun`
- **[SHOW]** — a dedicated `SHOW-xxx` demonstration function exists for it (beyond the file-level `SHOW-all-xxx`)
- **[test]** — a unit test (parachute or variants-equality) covers it

Files whose heading is marked **\*(reference notes)\*** contain illustrations, considerations, or demonstrations rather than exported functions.

Each function is followed by a one-line usage example.

## Table of contents

- [Quick start](#quick-start)
- [Building executables](#building-executables)
- [Files](#files-1) — [arrays-and-vectors](#arrays-and-vectors) · [association-lists *(ref. notes)*](#association-lists-reference-notes) · [bit-vectors](#bit-vectors) · [booleans](#booleans) · [combinations](#combinations) · [compare-durations](#compare-durations) · [console-charts](#console-charts) · [continued-fractions](#continued-fractions) · [dates-and-times](#dates-and-times) · [debug](#debug) · [equality *(ref. notes)*](#equality-reference-notes) · [farey-sequences](#farey-sequences) · [ffi-with-cffi *(ref. notes)*](#ffi-with-cffi-reference-notes) · [ffi-with-sb-alien *(ref. notes)*](#ffi-with-sb-alien-reference-notes) · [files](#files) · [format *(ref. notes)*](#format-reference-notes) · [functions *(ref. notes)*](#functions-reference-notes) · [gnuplot](#gnuplot) · [hash-tables *(ref. notes)*](#hash-tables-reference-notes) · [high-order-functions](#high-order-functions) · [images *(ref. notes)*](#images-reference-notes) · [lazy](#lazy) · [lists](#lists) · [macros](#macros) · [mail-via-gmail](#mail-via-gmail-requires-cl-smtp) · [math-functions](#math-functions) · [measure-duration](#measure-duration) · [memoization](#memoization) · [numbers-doubles](#numbers-doubles) · [numbers-integers](#numbers-integers) · [numbers-integers-french](#numbers-integers-french) · [numbers-integers-primes](#numbers-integers-primes) · [numbers-rationals](#numbers-rationals) · [ollama](#ollama-requires-jonathan-and-dexador) · [optimization](#optimization-sbcl-specific) · [os-interaction-windows](#os-interaction-windows-windows-specific-requires-cffi) · [parallelism](#parallelism-requires-lparallel-bordeaux-threads-sb-concurrency) · [permutations](#permutations) · [property-lists *(ref. notes)*](#property-lists-reference-notes) · [search](#search) · [sexp](#sexp) · [sorting](#sorting) · [strings](#strings) · [symbols](#symbols) · [tco *(ref. notes)*](#tco-reference-notes) · [trampoline](#trampoline) · [triangles](#triangles) · [types *(ref. notes)*](#types-reference-notes) · [variables *(ref. notes)*](#variables-reference-notes) · [web](#web-requires-drakma)

## Quick start

``` lisp
;; 1. Load the system
(asdf:load-system "cl-utils")

;; 2. Try a function
(cl-utils:primep 9973) ; => T
(cl-utils:fibonacci-through-iteration 10) ; => 55

;; 3. Run all the demonstrations
(cl-utils:SHOW-all-cl-utils)

;; 4. Run the test suite
(asdf:test-system "cl-utils-tests")
```

cl-utils is optimised for SBCL. A few sections (bit-vectors, parallelism, FFI, optimization, OS-interaction) depend on SBCL internals or platform/library prerequisites — these are flagged inline.

## Building executables

To produce an executable in Windows, execute following command in Powershell within project directory;
``` powershell
sbcl --load build_windows.lisp
```
Then launch `./my_program.exe`

To produce an executable in WSL, execute following command within project directory;
``` powershell
sbcl --load build_linux.lisp
```
Then launch `./my_program`

---

## Files

### arrays-and-vectors

   - `vec-view-all-content` [SHOW]  
     `(vec-view-all-content #(1 2 3 4 5 6 7 8)) ; prints length and full content`
   - `vec-preview` [SHOW]  
     `(vec-preview #(1 2 3 4 5 6 7 8)) ; prints length and first/last 3 elements`
   - `unliteral--fixnum-vector` [SHOW] [test]  
     `(unliteral--fixnum-vector #(1 2 3 4 5 6)) ; => fresh (simple-array fixnum) copy`
   - In the tests: a benchmark showing that `svref` does not bring extra performance (at least with `(speed 3)`).

### association-lists *(reference notes)*

Useful standard functions for association lists.

### bit-vectors (SBCL-specific)

   - `fixnum->bit-vector` [test]  
     `(fixnum->bit-vector 10) ; => #*0101 (left-endian)`
   - `bit-vector->fixnum` [test]  
     `(bit-vector->fixnum #*0101) ; => 10`
   - `bit-vector-logcount` [test]  
     `(bit-vector-logcount #*010101) ; => 3 (number of 1-bits)`
   - In the tests: a benchmark showing that, to reset a bit vector to zero, it is less costly to create a new one than to set each bit to zero individually.

### booleans

   - `boolean-value` [macro] [test]  
     `(boolean-value 2) ; => T (coerces any truthy value to T)`

### combinations

   - `with-combinations-of-index` [macro] [test]  
     `(with-combinations-of-index (c :of 5 3) (print c)) ; prints all 10 combinations C(5,3) as #(i j k)`

### compare-durations

   - `start-up-1`  
     `(start-up-1) ; => large fixnum sum (CPU warm-up before benchmarks)`
   - `compare-durations` [macro] [SHOW]  
     `(compare-durations (fn1 fn2) :args (10000) :repeat 10) ; prints real/run times, sorted`  
     (Demonstrations `SHOW-1-compare-durations` through `SHOW-5-compare-durations` illustrate five usage patterns.)
   - `plot-basic` (requires external `gnuplot`)  
     `(plot-basic 100 1000 100 (list #'fn1 #'fn2) "/usr/bin/gnuplot" "/tmp/") ; displays Gnuplot line chart of execution times`
   - `plot-cumulated` (requires external `gnuplot`)  
     `(plot-cumulated 100 1000 100 (list #'fn1 #'fn2) "/usr/bin/gnuplot" "/tmp/") ; displays cumulated-times chart with ranking`

### console-charts

   - Constants `+default-console-width+`, `+default-console-height+`  
     `+default-console-width+ ; => 94`
   - `console-quick-bar-chart-from-lists` [SHOW]  
     `(console-quick-bar-chart-from-lists '(1 2 30 40 150) '(75.1 96.567 151.1 80 88.12) :y-format "~,2f") ; prints ASCII bar chart`
   - `console-quick-bar-chart-from-vectors` [SHOW]  
     `(console-quick-bar-chart-from-vectors #(1 2 3) #(10.0 20.0 30.0) :sort-by 'y-desc) ; prints sorted ASCII bar chart`
   - `console-quick-line-chart` [SHOW]  
     `(console-quick-line-chart xs ys :y-format "~,3f") ; prints multi-series ASCII line chart`
   - `console-quick-scatter-plot-xs-ys` [SHOW]  
     `(console-quick-scatter-plot-xs-ys xs ys :y-format "~,3f") ; prints ASCII scatter plot with density characters`
   - `console-quick-histogram` [SHOW]  
     `(console-quick-histogram xs :nb-bars 20) ; prints ASCII histogram`

### continued-fractions

   - `rational-to-continued-fraction`  
     `(rational-to-continued-fraction 3 7) ; => (0 (2 3))`
   - `integer-sqrt-to-continued-fraction` [test]  
     `(integer-sqrt-to-continued-fraction 23) ; => (4 (1 3 1 8))`
   - `length-continued-fraction-isqrt` [test]  
     `(length-continued-fraction-isqrt 23) ; => 4`
   - `with-successive-convergents` [macro] [test]  
     `(with-successive-convergents (i n d :of (integer-sqrt-to-continued-fraction 2)) (format t "~s/~s " n d)) ; prints 1/1 3/2 7/5 17/12 ...`
   - `solve-pell-equation` [test]  
     `(solve-pell-equation 3) ; => 2, 1 (since 2² − 3·1² = 1)`

### dates-and-times

   - `universal-time-to-YYYYMMDD-HHMMSS` [test]  
     `(universal-time-to-YYYYMMDD-HHMMSS 0) ; => "19000101-000000"`
   - `get-current-YYYYMMDD-HHMMSS` [SHOW]  
     `(get-current-YYYYMMDD-HHMMSS) ; => e.g. "20260523-145223"`
   - `convert-int-YYYYMMDD-and-HHMMSS-to-universal-time` [test]  
     `(convert-int-YYYYMMDD-and-HHMMSS-to-universal-time 19000101 100) ; => 60`
   - `convert-int-YYYYMMDD-to-universal-time` [test]  
     `(convert-int-YYYYMMDD-to-universal-time 19000102) ; => 86400`
   - `universal-time-to-YYYY-MM-DD` [test]  
     `(universal-time-to-YYYY-MM-DD 0) ; => "1900-01-01"`
   - `pretty-print-universal-time-as-full-date-time` [SHOW] [test]  
     `(pretty-print-universal-time-as-full-date-time (get-universal-time)) ; => "Saturday 23/05/2026 14:59:26 (GMT+1, no DST)"`
   - `pretty-print-universal-time-as-long-date` [SHOW] [test]  
     `(pretty-print-universal-time-as-long-date (get-universal-time)) ; => "May 23rd, 2026"`
   - `pretty-print-universal-time-as-short-date` [SHOW] [test]  
     `(pretty-print-universal-time-as-short-date (get-universal-time)) ; => "May 23rd, 2026"`
   - `pretty-print-time-difference` [test]  
     `(pretty-print-time-difference 0 (* 3 24 3600)) ; => "4 days" (or "3.5 months", "1.3 years" depending on delta)`

### debug

   - SLDB commands: see Emacs related hydra
   - `debug-output` [macro] [SHOW]  
     `(let ((r 0)) (debug-output (incf r 5))) ; prints "(INCF R 5) --> 5"`

### equality *(reference notes)*

Considerations on equality predicates (`eq`, `eql`, `equal`, `equalp`, `=`).

### farey-sequences

   - `with-successive-farey` [macro]  
     `(with-successive-farey (a b :order 5) (format t "~s/~s " a b)) ; prints 0/1 1/5 1/4 1/3 2/5 1/2 ...`
   - `farey-immediately-on-left-of` [test]  
     `(farey-immediately-on-left-of 8 3 7) ; => (2 5) (i.e. 2/5)`
   - `farey-length` [test]  
     `(farey-length 8) ; => 23`

### ffi-with-cffi *(reference notes)*

Demonstration of foreign-function interface via [CFFI](https://cffi.common-lisp.dev/) (`SHOW-ffi-with-cffi`).

### ffi-with-sb-alien *(reference notes)*

Demonstration of foreign-function interface via SBCL's `sb-alien` (`SHOW-ffi-with-sb-alien`).

### files

   - `random-file-name`  
     `(random-file-name "c:" "graph1" "gp") ; => "c:/graph1-r264491-20220403-145457.gp"`
   - `reduce-for-each-line-of-file` [test]  
     `(reduce-for-each-line-of-file #'+ "numbers.txt" :key #'parse-integer) ; => sum of integers, one per line`

### format *(reference notes)*

Illustrations of various `format` directives.

### functions *(reference notes)*

Generalities on functions; pass-by-reference vs pass-by-value notes.

### gnuplot (requires external `gnuplot`)

   - `gnuplot-plot-line-chart`  
     `(gnuplot-plot-line-chart x-vec (list (list :top y-vec :legend "data")) "/usr/bin/gnuplot" "/tmp/" :title "Demo") ; launches Gnuplot to show line chart`
   - `gnuplot-chart-one-bar-chart`  
     `(gnuplot-chart-one-bar-chart x-vec y-vec "/usr/bin/gnuplot" "/tmp/" :title "Bars" :color "blue") ; launches Gnuplot to show bar chart`

### hash-tables *(reference notes)*

Useful standard functions for hash tables.

### high-order-functions

   - `reduce-recursive-sequence` [SHOW]  
     `(reduce-recursive-sequence :from 0 :rec-fn #'1+ :while0 (lambda (n) (<= n 100)) :fn #'identity :aggregate-fn #'+) ; => 5050`
   - `all-which` [SHOW] [test]  
     `(all-which :from -3 :fn (lambda (n) (* n n)) :target-reached-fn (lambda (m) (= m 4)) :while-arg-fn (lambda (n) (<= n 3))) ; => ((-2 4) (2 4))`
   - `first-which` [macro] [SHOW] [test]  
     `(first-which :generator (loop for i from 1 do (submit i)) :fn (lambda (i) (* i i)) :target-reached-fn (lambda (m) (> m 100))) ; => (11 121)`
   - `maximizing--fixnum` [macro] [SHOW] [test]  
     `(maximizing--fixnum (loop for n from -3 to 3 do (maximize (- 5 (* n n)) n))) ; => (5 (0))`
   - `minimizing--fixnum` [macro] [SHOW] [test]  
     `(minimizing--fixnum (loop for n from -3 to 3 do (minimize (+ 5 (* n n)) n))) ; => (5 (0))`
   - `maximizing--bigint` [macro]  
     `(maximizing--bigint (loop for n from 1 to 5 do (maximize (expt 10 n) n))) ; => (100000 (5))`
   - `minimizing--bigint` [macro]  
     `(minimizing--bigint (loop for n from 1 to 5 do (minimize (expt 10 n) n))) ; => (10 (1))`
   - `maximizing--rational` [macro]  
     `(maximizing--rational (loop for n from 1 to 3 do (maximize (/ 1 n) n))) ; => (1 (1))`
   - `minimizing--rational` [macro]  
     `(minimizing--rational (loop for n from 1 to 3 do (minimize (/ 1 n) n))) ; => (1/3 (3))`
   - `maximizing--df` [macro]  
     `(maximizing--df (loop for n from -3 to 3 do (maximize (- 5.0d0 (* n n 1.0d0)) n))) ; => (#(5.0d0) (0))`
   - `minimizing--df` [macro]  
     `(minimizing--df (loop for n from -3 to 3 do (minimize (+ 5.0d0 (* n n 1.0d0)) n))) ; => (#(5.0d0) (0))`
   - `max1D` [SHOW] [test]  
     `(max1D (lambda (n) (- 25 (* n n))) -5 5) ; => (25 (0))`
   - `min1D` [SHOW] [test]  
     `(min1D (lambda (n) (* n n)) -5 5) ; => (0 (0))`
   - `max2D` [SHOW] [test]  
     `(max2D (lambda (x y) (- 50 (* x x) (* y y))) -5 5 -5 5) ; => (50 ((0 0)))`
   - `min2D` [SHOW] [test]  
     `(min2D (lambda (x y) (+ (* x x) (* y y))) -5 5 -5 5) ; => (0 ((0 0)))`

### images *(reference notes)*

Internal-only helpers for PNG generation (uses [zpng library](https://github.com/xach/zpng)). Not exported; copy-paste from source if needed.

### lazy

   - `make-ascending-sequence-with-explicit-formula` [SHOW] (memoizing membership predicate for ascending sequences)  
     `(funcall (make-ascending-sequence-with-explicit-formula (lambda (n) (/ (* n (1+ n)) 2))) 15) ; => T (15 is triangular)`
   - `make-ascending-sequence-with-explicit-formula-no-memo` [SHOW] (lightweight variant, requires monotonically increasing queries)  
     `(funcall (make-ascending-sequence-with-explicit-formula-no-memo (lambda (n) (/ (* n (1+ n)) 2))) 15) ; => T`

### lists

   - `delete-nth` [test]  
     `(delete-nth 2 '(1 2 3 4 5)) ; => (1 2 4 5)`
   - `replace-nth` [test]  
     `(replace-nth 2 99 '(1 2 3 4 5)) ; => (1 2 99 4 5)`
   - `list-preview`  
     `(list-preview '(1 2 3 4 5 6 7 8 9)) ; prints "[9] 1 2 3 ... 7 8 9"`
   - `arg-min` [test]  
     `(arg-min '(1 3 2 0 5) #'<) ; => 3 (index of minimum)`
   - `arg-max` [test]  
     `(arg-max '(1 3 2 0 5) #'<) ; => 4 (index of maximum)`
   - `nb-of-occurrences-of-sublist-in-list` [test]  
     `(nb-of-occurrences-of-sublist-in-list '(4 5) '(1 4 5 6 4 5 7 4 5)) ; => 3`
   - `shuffle` (Fisher-Yates in-place shuffle) [test]  
     `(shuffle (list 1 2 3 4 5)) ; => e.g. (3 1 5 2 4) (destructive)`
   - `sublist-knowing-indexes-as-list` [test]  
     `(sublist-knowing-indexes-as-list '(a b c d e f) '(2 3 5)) ; => (C D F)`
   - `new-random-fixnum-list`  
     `(new-random-fixnum-list 5) ; => e.g. (42 7 88 13 5)`
   - Type predicates:
     - `fixnump` [test]  
       `(fixnump 42) ; => T`
     - `list-of-fixnums-p` [test]  
       `(list-of-fixnums-p '(1 2 3)) ; => T`
     - `double-float-p` [test]  
       `(double-float-p 3.14d0) ; => T`
     - `list-of-double-floats-p` [test]  
       `(list-of-double-floats-p '(1.0d0 2.0d0)) ; => T`
   - Types:
     - `list-of-fixnums`  
       `(typep '(1 2 3) 'list-of-fixnums) ; => T`
     - `list-of-double-floats`  
       `(typep '(1.0d0 2.0d0) 'list-of-double-floats) ; => T`
   - `make-circular-DO-NOT-PRINT--AND-NOT-LITERAL` [test]  
     `(make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (list 1 2 3)) ; => circular list 1->2->3->1->...`
   - `circular-list-length` [test]  
     `(circular-list-length (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL (list 1 2 3))) ; => 3`
   - `unliteral--fixnum-list`  
     `(unliteral--fixnum-list '(1 2 3)) ; => (1 2 3) (fresh, non-literal copy)`

### macros

   - `with-gensyms` [macro]  
     `` (defmacro swap (a b) (with-gensyms (tmp) `(let ((,tmp ,a)) (setf ,a ,b ,b ,tmp)))) ``
   - `while` [macro] [SHOW] [test]  
     `(let ((a 0)) (while (< a 5) (incf a)) a) ; => 5`
   - `while1` [macro] [SHOW] [test] (usable inside `loop`)  
     `(loop for n from 1 to 3 do (let ((a 0)) (while1 (< a n) (incf a))))`
   - `repeat-until` [macro] [SHOW] [test]  
     `(let ((a 0)) (repeat-until (incf a) :until (> (* a a) 10)) a) ; => 4`
   - `do-while` [macro] [SHOW] [test]  
     `(let ((a 0)) (do-while (incf a) :while (<= (* a a) 10)) a) ; => 4`
   - `aprogn` [macro] [SHOW]  
     `(aprogn 5 (+ 1 it) (* 3 it)) ; => 18 (each form sees previous value as IT)`
   - `ablock` [macro] [SHOW]  
     `(ablock it (dotimes (i 10) (when (= i 5) (return-from it i)))) ; => 5`
   - `setf-min` [macro]  
     `(let ((x 5)) (setf-min x 3) x) ; => 3`
   - `setf-max` [macro]  
     `(let ((x 5)) (setf-max x 7) x) ; => 7`
   - `setf-min--df` [macro]  
     `(let ((x 5.0d0)) (setf-min--df x 3.0d0) x) ; => 3.0d0`
   - `setf-max--df` [macro]  
     `(let ((x 5.0d0)) (setf-max--df x 7.0d0) x) ; => 7.0d0`
   - `with-collector` [macro] [SHOW] [test]  
     `(with-collector (collect) (loop for i from -5 to 5 when (> (* i i) 10) do (collect i))) ; => (-5 -4 4 5)`
   - `with-collector--reversed-order` [macro] [SHOW]  
     `(with-collector--reversed-order (collect) (dolist (i '(1 2 3)) (collect i))) ; => (3 2 1)`
   - `let+` [macro] [SHOW]  
     `(let+ ((x 10) (square (lambda (n) (* n n)))) (square x)) ; => 100 (bindings can be values or functions)`

### mail-via-gmail (requires `cl-smtp`)

   - `send-mail-via-gmail`  
     `(send-mail-via-gmail :from "a@gmail.com" :to "b@gmail.com" :subject "test" :body "hi" :password "app-pw") ; sends mail via Gmail SMTP+STARTTLS`

### math-functions

   - Constants `+epsilon8+`, `+epsilon12+`, `+epsilon30+`, `+epsilon60+`, `+epsilon98+`  
     `+epsilon8+ ; => 1.0d-8`
   - Constants `+1-over-12+`, `+1-over-360+`, `+1-over-1260+`, `+1-over-1680+`  
     `+1-over-12+ ; => 0.08333333333333333d0`
   - Constants `+ln-2pi-over-2+`, `+1-over-sqrt-2pi+`  
     `+1-over-sqrt-2pi+ ; => 0.3989422804014327d0`
   - `legendre1`, `legendre2` [test], `legendre3` [test] (discrete Legendre polynomial coefficients, source: Timothy Masters)  
     `(legendre3 5 work1 work2 work3) ; fills WORK1/2/3 with normalized Legendre coefficients of orders 1/2/3 for N=5`
   - `normal-cdf-m` [macro] (normal CDF, source: Timothy Masters / Abramovitz & Stegun)  
     `(normal-cdf-m tmp-box 0.88d0) ; => ~0.8105d0`
   - `lgamma-m` [macro] (log-Gamma function, source: Timothy Masters)  
     `(lgamma-m tmp-box 0.65d0) ; => ~0.3796d0`
   - `ibeta-m` [macro] (incomplete beta function ratio, source: Timothy Masters)  
     `(ibeta-m tmp-box 0.6d0 0.4d0 0.8d0) ; => I(0.8; 0.6, 0.4)`
   - `igamma-m` [macro] (complement to 1 of incomplete gamma function ratio, source: Timothy Masters)  
     `(igamma-m tmp-box 0.4d0 0.6d0) ; => 1 − P(0.4, 0.6)`
   - `f-cdf-m` [macro] (CDF of F-ratio distribution, source: Timothy Masters)  
     `(f-cdf-m tmp-box 6 4 2.0d0) ; => P(F ≤ 2.0) with (6,4) degrees of freedom`

### measure-duration

   - `SHOW-measure-duration`  
     `(SHOW-measure-duration 100000000) ; prints Leibniz-pi approximation and elapsed seconds`
   - `SHOW-benchmark-5-times-A`  
     `(SHOW-benchmark-5-times-A 100000000) ; runs Leibniz formula five times; reports quickest & slowest`
   - `SHOW-benchmark-5-times-B`  
     `(SHOW-benchmark-5-times-B 100000000) ; same idea, but the benched function returns its own duration`
   - `with-timing` [macro] [SHOW]  
     `(let (d) (with-timing (d) (dotimes (i 1000000) (sin 1.0d0))) d) ; => elapsed seconds bound to D`

### memoization

   - `labels-memo-ht` [macro] [SHOW] (memoization by hash table)  
     `(labels-memo-ht nil ((fibo (m) (if (or (= m 1) (= m 2)) 1 (+ (fibo (- m 2)) (fibo (- m 1)))))) (fibo 20)) ; => 6765`
   - `labels-memo-array` [macro] [SHOW] (memoization by array, 1D and 2D)  
     `(labels-memo-array 21 ((fibo (m) (if (or (= m 1) (= m 2)) 1 (+ (fibo (- m 2)) (fibo (- m 1)))))) (fibo 20)) ; => 6765`
   - `labels-memo-mix` [macro] [SHOW] (memoization by array below pivot, hash table above)  
     `(labels-memo-mix equal 10 ((fibo (m) (if (or (= m 1) (= m 2)) 1 (+ (fibo (- m 2)) (fibo (- m 1)))))) (fibo 20)) ; => 6765`

### numbers-doubles

   - Types `type-df-box` and `type-df-3box` (encapsulated double-floats to avoid float-to-pointer coercion)  
     `(typep #(3.14d0) 'type-df-box) ; => T`
   - `make-box`, `value-of`, `put-into` [macro × 3] (1-value box)  
     `(let ((b (make-box 3.14d0))) (put-into b 2.71d0) (value-of b)) ; => 2.71d0`
   - `make-3box`, `value1-of3`, `value2-of3`, `value3-of3`, `put3-into` [macro × 5] (3-value box)  
     `(let ((b (make-3box 1d0 2d0 3d0))) (put3-into b 10d0 20d0 30d0) (value2-of3 b)) ; => 20.0d0`
   - `%string-to-doublefloat-into` and `string-to-doublefloat-m` [macro] [test] (fast string-to-double-float parsing)  
     `(string-to-doublefloat-m tmp-box "145,256" #\,) ; => 145.256d0`
   - `%elements-are-doublefloats` and type `type-list-of-doublefloats`  
     `(%elements-are-doublefloats '(1.0d0 2.0d0)) ; => T`
   - `random-dfvec` (random double-float vector generation)  
     `(random-dfvec 3 :limit 10.0d0) ; => e.g. #(3.7d0 8.2d0 1.4d0)`
   - `vec-new-chirp`, `vec-new-creneau` (signal generation)  
     `(vec-new-chirp :nb-bars 1500 :short-T 40.0d0 :long-T 100.0d0) ; => chirp signal as df-vector`
   - `vec-mean-m` [macro], `vec-variance-m` [macro] [test], `vec-highest-m` [macro] [test], `vec-lowest-m` [macro] [test] (vector statistics)  
     `(vec-mean-m tmp-box #(1.0d0 3.0d0 5.0d0 7.0d0)) ; => 4.0d0`
   - `variance-of-log-m` [macro] (variance of log-returns, from Timothy Masters)  
     `(variance-of-log-m tmp-box t 99 20 prices) ; => variance of log(p_i / p_{i-1}) over a 20-bar window`
   - `vec-SORTED-median-m`, `vec-SORTED-quartiles-m`, `vec-SORTED-centile-m` [macro × 3] (sorted vector quantiles)  
     `(vec-SORTED-median-m tmp-box #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)) ; => 3.0d0`

### numbers-integers

**Constants and basic arithmetic**

   - Constants `+square-root-of-5+`, `+phi+`, `+phi2+`  
     `+phi+ ; => 1.618033988749895d0`
   - `power` [test], `power--bigint`  
     `(power 2 10) ; => 1024`  
     `(power--bigint 2 100) ; => 1267650600228229401496703205376`
   - `floor-to-power-of-10` [test], `ceiling-to-power-of-10` [test]  
     `(floor-to-power-of-10 123) ; => 100`  
     `(ceiling-to-power-of-10 123) ; => 1000`
   - `concatenate-numbers` [test]  
     `(concatenate-numbers 41 5) ; => 415`
   - `fibonacci-through-iteration` [test], `fibonacci-through-explicit-formula`  
     `(fibonacci-through-iteration 10) ; => 55`  
     `(fibonacci-through-explicit-formula 10) ; => 55`
   - `non-decreasing-p` [test]  
     `(non-decreasing-p '(1 1 2 2 3)) ; => T`
   - `group-factors` [test]  
     `(group-factors '(2 2 2 3)) ; => ((2 . 3) (3 . 1))`
   - `iota` [test], `make-list-within-range` [test]  
     `(iota 5) ; => (1 2 3 4 5)`  
     `(make-list-within-range 1 3) ; => (1 2 3)`
   - `new-sequence-fixnum-vector`, `new-random-fixnum-vector`  
     `(new-sequence-fixnum-vector 5) ; => #(0 1 2 3 4)`  
     `(new-random-fixnum-vector 3 :mini 0 :maxi 10) ; => e.g. #(7 2 5)`
   - `string-to-integer-list` [test]  
     `(string-to-integer-list "1 2 3") ; => (1 2 3)`

**Combinatorics and modular arithmetic**

   - `fact` [test], `fact--bigint`, `fact-0-9` [test], `combin` [test]  
     `(fact 5) ; => 120`  
     `(fact--bigint 25) ; => 15511210043330985984000000`  
     `(fact-0-9 5) ; => 120 (specialised for 0..9)`  
     `(combin 10 3) ; => 120`
   - `mod-incf` [macro], `mod-expt--fixnum` [test]  
     `(let ((a 5)) (mod-incf a 6 10) a) ; => 1`  
     `(mod-expt--fixnum 2 10 1000) ; => 24`

**Digit operations**

   - `char->digit` [test], `last-digit` [test], `first-digit` [test]  
     `(char->digit #\5) ; => 5`  
     `(last-digit 123) ; => 3`  
     `(first-digit 29) ; => 2`
   - `for-successive-digits-in-reverse-order` [macro]  
     `(for-successive-digits-in-reverse-order (d :in 123) (print d)) ; prints 3, 2, 1`
   - `number->digits` [test], `number->digits--bigint`, `nth-digit` [test]  
     `(number->digits 123) ; => (1 2 3)`  
     `(nth-digit 56789 3) ; => 7`
   - `number->vector-of-digits` [test for round-trip], `number->existing-vector-of-digits`  
     `(number->vector-of-digits 123) ; => #(1 2 3)`  
     `(let ((w (make-array 8 :element-type 'fixnum))) (number->existing-vector-of-digits 1234 w) w) ; => #(1 2 3 4 0 0 0 0)`
   - `number->digits-set`, `number->digits-set--bigint`  
     `(number->digits-set 1223) ; => (1 2 3) (no duplicates)`
   - `list-of-digits->number` [test], `list-of-digits->number--bigint`, `vector-of-digits->number`  
     `(list-of-digits->number '(1 2 3)) ; => 123`  
     `(vector-of-digits->number #(1 2 3)) ; => 123`
   - `number->sorted-digits` [test], `first-digits`, `first-digits--bigint`  
     `(number->sorted-digits 5643) ; => (3 4 5 6)`  
     `(first-digits 12345 2) ; => 12`
   - `nb-digits` [test], `nb-digits--bigint`  
     `(nb-digits 999) ; => 3`
   - `sum-of-digits` [test], `sum-of-digits--bigint`, `product-of-digits` [test]  
     `(sum-of-digits 123) ; => 6`  
     `(product-of-digits 1234) ; => 24`
   - `contains-all-digits-at-least-once-p` [test], `contains-all-digits-at-least-once-p--bigint`, `replace-nth-digit` [test], `replace-digits`  
     `(contains-all-digits-at-least-once-p 1234567890) ; => T`  
     `(replace-nth-digit 123 2 9) ; => 193`  
     `(replace-digits 12345 #(1 3) 9) ; => 92945`
   - `pandigitalp` [test]  
     `(pandigitalp 987654321) ; => T`
   - `reverse-number--fixnum` [test], `reverse-number--bigint` [test]  
     `(reverse-number--fixnum 123) ; => 321`

**Divisibility and number theory**

   - `sum-of-multiples-below` [test], `sum-of-multiples-below--bigint`  
     `(sum-of-multiples-below 3 10) ; => 18 (3+6+9)`
   - `gcd--2fixnum` [test], `is-divisible-by-m` [macro], `divides-m` [macro]  
     `(gcd--2fixnum 12 18) ; => 6`  
     `(is-divisible-by-m 12 4) ; => T`  
     `(divides-m 4 12) ; => T`
   - `coprimes-p` [test], `coprimes-p-m` [macro]  
     `(coprimes-p 3 7) ; => T`
   - `totient` [test], `totients-from-1-to-n` [test], `totient-summatory`  
     `(totient 12) ; => 4`  
     `(totients-from-1-to-n 10) ; => #(0 1 1 2 2 4 2 6 4 6 4)`  
     `(totient-summatory 10) ; => 32`

**Perfect powers**

   - `perfect-square-p` [test], `perfect-square-p-m` [macro], `perfect-square-p-specific` [test], `perfect-cube-p-specific` [test]  
     `(perfect-square-p 16) ; => T`  
     `(perfect-square-p-specific 16) ; => 4`  
     `(perfect-cube-p-specific 27) ; => 3`

**Integer ↔ words**

   - `integer->english` [test]  
     `(integer->english 123) ; => "one hundred and twenty-three"`
   - `roman->integer` [test], `integer->roman` [test]  
     `(roman->integer "MCMXCIX") ; => 1999`  
     `(integer->roman 1999) ; => "MCMXCIX"`

### numbers-integers-french

   - `en-toutes-lettres` [test]  
     `(en-toutes-lettres 101) ; => "cent un"`

### numbers-integers-primes

   - `largest-prime-factor` [test]  
     `(largest-prime-factor 13195) ; => 29`
   - `primep` [test]  
     `(primep 9973) ; => T`
   - `next-prime` [test]  
     `(next-prime 7) ; => 11`
   - `nth-prime` [test]  
     `(nth-prime 6) ; => 13`
   - `primes-below-as-list` (sieve of Eratosthenes), `primes-below-as-vector`, `primep-vector-below`  
     `(primes-below-as-list 30) ; => (2 3 5 7 11 13 17 19 23 29)`  
     `(primes-below-as-vector 30) ; => #(2 3 5 7 11 13 17 19 23 29)`  
     `(primep-vector-below 10) ; => #*0011010100 (bit-vector; bit I = 1 iff I is prime)`
   - `prime-decomposition-grouped` (trial division), `prime-divisors`  
     `(prime-decomposition-grouped 100) ; => ((2 . 2) (5 . 2))`  
     `(prime-divisors 100) ; => (2 5)`
   - `list-of-divisors`, `nb-of-divisors`, `list-of-proper-divisors`, `sum-of-proper-divisors`  
     `(list-of-divisors 12) ; => (1 2 3 4 6 12)`  
     `(nb-of-divisors 12) ; => 6`  
     `(list-of-proper-divisors 6) ; => (1 2 3)`  
     `(sum-of-proper-divisors 9) ; => 4`
   - `abundantp`  
     `(abundantp 12) ; => T (12 < 1+2+3+4+6 = 16)`
   - `totient-below`  
     `(totient-below 9 10) ; => 6 (count of k in [1,10) with gcd(9,k)=1)`

### numbers-rationals

   - `convert-to-proper-fraction-m` [macro] [SHOW] [test]  
     `(let ((a 50) (b 15)) (convert-to-proper-fraction-m a b) (list a b)) ; => (10 3)`
   - `length-of-recurring-cycle` [test]  
     `(length-of-recurring-cycle 7) ; => 6`

### ollama (requires `jonathan` and `dexador`)

   - `call-ollama1`  
     `(call-ollama1 "Define recursion in three sentences" :model "llama3") ; => "Recursion is …"`
   - `call-ollama2` (also requires `bordeaux-threads`)  
     `(call-ollama2 "Describe recursion in one sentence") ; prints progress dots + stats, returns response string`

### optimization (SBCL-specific)

How to comply with `(speed 3)` compilation notes. General considerations and methods, plus:

   - Type `fixnum-range-double-float`  
     `(typep 3.7d0 'fixnum-range-double-float) ; => T (a double-float that fits in fixnum range)`
   - `round-of-df` [macro]  
     `(round-of-df 3.7d0) ; => 4 (fast round for fixnum-range double-floats)`
   - Type `positive-fixnum-range-double-float`  
     `(typep 3.7d0 'positive-fixnum-range-double-float) ; => T`
   - `floor-of-positive-df` [macro] [test]  
     `(floor-of-positive-df 3.7d0) ; => 3`

\+ a [note](notes/float-to-pointer-coercion.md) about "doing float to pointer coercion (cost 13) to \<return value\>" compilation notes.

### os-interaction-windows (Windows-specific, requires `cffi`)

   - `open-html-file-with-default-browser`, `open-url-with-default-browser`  
     `(open-html-file-with-default-browser "C:/tmp/page.html") ; launches Windows START on the file`  
     `(open-url-with-default-browser "https://example.com") ; opens URL in default browser`
   - `copy-string-to-clipboard` (Win32 API via CFFI)  
     `(copy-string-to-clipboard "hello") ; puts "hello" on the Windows clipboard`
   - `with-export-to-clipboard` [macro] (structured clipboard export for Excel)  
     `(with-export-to-clipboard t (export1 (:string "name") :nc (:double-float 1.5d0) :nl)) ; writes TAB-separated, CRLF-terminated row to clipboard`
   - `convert-double-float-to-string-ready-for-clipboard-towards-Excel`, `convert-single-float-to-string-ready-for-clipboard-towards-Excel`  
     `(convert-double-float-to-string-ready-for-clipboard-towards-Excel 3.14d0) ; => "3,14"`  
     `(convert-single-float-to-string-ready-for-clipboard-towards-Excel 3.14f0) ; => "3,14"`

### parallelism (requires `lparallel`, `bordeaux-threads`, `sb-concurrency`)

   - `nb-cores`, `with-parallelism` [macro], `show-kernel-info`  
     `(nb-cores) ; => e.g. 8 (caches into *nb-cores*)`  
     `(with-parallelism (lparallel:pmap 'list #'1+ '(1 2 3))) ; => (2 3 4); creates/destroys lparallel kernel`  
     `(show-kernel-info) ; prints worker count, context, and bindings of current kernel`
   - Loop variants: `ploop--based-on-pmap` [macro] [SHOW], `ploop--throwable-threads` [macro] [SHOW], `ploop--reusable-threads` [macro] [SHOW], `pfor-by-blocks-with-pmap` [macro] [SHOW]  
     `(ploop--based-on-pmap :generator (loop for i from 1 to 40 do (submit i)) :thread-fn (lambda (n) (* n 10)) :aggregate-fn (lambda (m) (incf sum m))) ; pmaps THREAD-FN over submitted values`  
     `(pfor-by-blocks-with-pmap :from 1 :below 101 :nb-parts 16 :thread-fn (lambda (a b) (loop for i from a below b sum i)) :aggregate-fn (lambda (v) (reduce #'+ v))) ; => 5050`
   - Search: `p-first-which` [macro]  
     `(p-first-which :from 0 :fn #'identity :target-reached-fn (lambda (x) (> x 1000)) :block-size 30 :nb-threads 4) ; => (1001 1001)`
   - Extremum variants (all [macro]): `p-maximizing--*`, `p-minimizing--*` (based-on-pmap, throwable-threads, by-blocks for fixnum/rational/df)  
     `(p-maximizing-by-blocks-with-pmap--fixnum :from 1 :below 1001 :nb-parts 16 :thread-fn (lambda (a b) (maximizing--fixnum (loop for i from a below b do (maximize (- 5 (* i i)) i))))) ; => (max (xs)) processed in parallel blocks`
   - Leibniz series benchmarks (8 variants) comparing sequential vs parallel approaches  
     `(SHOW-leibniz-3) ; => approximates pi using sb-thread + mutex; returns elapsed seconds`

### permutations

   - `permute-randomly-fixnum-array-in-place`, `permute-randomly-doublefloat-array-in-place`  
     `(let ((v (make-array 6 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6)))) (permute-randomly-fixnum-array-in-place v) v) ; => e.g. #(3 6 1 5 4 2)`  
     `(permute-randomly-doublefloat-array-in-place v 10) ; permutes V in place over first 10 elements`
   - `with-random-fixnum-permutations` [macro], `with-random-doublefloat-permutations` [macro]  
     `(with-random-fixnum-permutations (p :from arr :times 1000) (process p)) ; runs BODY 1000 times with P = random permutation of ARR`
   - `rotate-randomly-doublefloat-array-in-place`  
     `(rotate-randomly-doublefloat-array-in-place v 10 work) ; random cyclic rotation of V using WORK buffer`
   - `next-distinct-lexicographic-permutation--string` [test]  
     `(next-distinct-lexicographic-permutation--string (copy-seq "123") #'char<) ; => "132"`
   - `next-distinct-lexicographic-permutation--fixnum-vector` [test]  
     `(next-distinct-lexicographic-permutation--fixnum-vector (make-array 3 :element-type 'fixnum :initial-contents '(1 2 3)) #'<) ; => #(1 3 2)`
   - `with-permutations--fixnum-list` [macro] (Heap's algorithm)  
     `(with-permutations--fixnum-list (v :from '(1 2 3)) (print v)) ; prints all 6 permutations of (1 2 3)`
   - `with-distinct-rotated-numbers` [macro], `list-of-distinct-rotated-numbers` [test]  
     `(with-distinct-rotated-numbers (m :from 123) (print m)) ; prints 123 312 231`  
     `(list-of-distinct-rotated-numbers 12345) ; => (51234 45123 34512 23451 12345)`
   - `with-permutations-of-digits` [macro], `with-permutations-of-digits-no-leading-zero` [macro]  
     `(with-permutations-of-digits (m :of 123) (print m)) ; prints 123 132 213 231 312 321`  
     `(with-permutations-of-digits-no-leading-zero (m :of 102) (print m)) ; iterates digit-permutations of 102 that don't start with 0`
   - `have-permutated-digits-p` [test]  
     `(have-permutated-digits-p 123 321) ; => T`

### property-lists *(reference notes)*

Useful standard functions for property lists.

### search

   - `binary-search` (binary search on sorted fixnum vectors) [test]  
     `(binary-search 4 (make-array 7 :element-type 'fixnum :initial-contents '(1 2 3 4 5 6 7))) ; => 3`
   - `binary-search--with-initial-bounds` [test]  
     `(binary-search--with-initial-bounds 4 #(1 2 3 4 5 6 7) 2 5) ; => 3`

### sexp

   - `sexp-replace-sexp-beginning-by` [SHOW] [test]  
     `(sexp-replace-sexp-beginning-by '(a (d (submit g h)) c) 'submit (lambda (s) (cons 'submit2 (cdr s)))) ; => (A (D (SUBMIT2 G H)) C)`

### sorting

   - `vec-qsortd-doublefloat` (quicksort for double-float vectors) [test]  
     `(let ((v (random-dfvec 10 :limit 100.0d0))) (vec-qsortd-doublefloat v) v) ; => sorted in place`
   - `vec-qsortds-doublefloat-slave-doublefloat` (quicksort with double-float slave) [test]  
     `(vec-qsortds-doublefloat-slave-doublefloat data slave) ; sorts DATA in place; SLAVE moves in lockstep`
   - `vec-qsortdsi-doublefloat-slave-fixnum` (quicksort with fixnum slave) [test]  
     `(vec-qsortdsi-doublefloat-slave-fixnum data slave-fix) ; df-data sorted; fixnum-slave follows`
   - `vec-qsortssi-singlefloat-slave-fixnum` (quicksort for single-float with fixnum slave) [test]  
     `(vec-qsortssi-singlefloat-slave-fixnum data slave-fix) ; single-float DATA sorted; fixnum SLAVE follows`
   - `vec-qsortisi-fixnum-slave-fixnum` (quicksort for fixnum with fixnum slave) [test]  
     `(vec-qsortisi-fixnum-slave-fixnum data slave) ; fixnum DATA sorted; fixnum SLAVE follows`

### strings

   - `string-split` [test]  
     `(string-split "aa,b b,c c c" #\,) ; => ("aa" "b b" "c c c")`
   - `substring-after-last` [test]  
     `(substring-after-last "abc:def:::ghi" #\:) ; => "ghi"`
   - `string-repeat-string` [test]  
     `(string-repeat-string 3 "abc") ; => "abcabcabc"`
   - `string-add-space-at-left` [test]  
     `(string-add-space-at-left "abc" :total-length 6) ; => "   abc"`
   - `duplicate-strings-in-list` [test]  
     `(duplicate-strings-in-list '("abc" "def" "abc")) ; => ("abc")`
   - `palindrome-string-p` [test]  
     `(palindrome-string-p "aba") ; => T`
   - `unliteral--string`  
     `(unliteral--string "abcde") ; => "abcde" (fresh, non-literal copy)`

### symbols

   - `function-to-string` [test]  
     `(function-to-string #'sin) ; => "SIN"`
   - `function-to-string-no-package`  
     `(function-to-string-no-package #'cl-utils::primep) ; => "PRIMEP" (strips package prefix)`

### tco *(reference notes)*

Illustration that `(speed 3)` encourages tail-call optimization.

### trampoline

   - `trampoline`  
     `(trampoline (lambda () (labels ((sub (n acc) (if (zerop n) acc (lambda () (sub (1- n) (+ acc n)))))) (sub 1000000 0)))) ; => 500000500000 (no stack blow-up without TCO)`

### triangles

   - `triangle-to-2d-array` [test]  
     `(triangle-to-2d-array '((1) (2 3) (4 5 6))) ; => #2A((1 0 0) (2 3 0) (4 5 6))`

### types *(reference notes)*

General considerations on types.

### variables *(reference notes)*

General considerations on variables; bindings; lexical vs dynamic scoping.

### web (requires `drakma`)

   - `web-redirect-p`  
     `(web-redirect-p "http://www.google.fr" :print-info t) ; => T (URL was redirected)`

---

Any comment? Open an [issue](https://github.com/occisn/cl-utils/issues), or start a discussion [here](https://github.com/occisn/cl-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

(end of README)
