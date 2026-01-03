# cl-utils

Personal utilities for Common Lisp.

Some functions are specific to SBCL.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

When relevant, functions and macros are illustrated with `SHOW-xxx`.

The function `(SHOW-all-cl-utils)` defined in `_show-all.lisp` file executes most of the `SHOW-xxx` functions one after the other.

A test suite is proposed. In addition to traditional tests, when several implementations of the same function are proposed, tests files may *(i)* check that the various implementations yield the same results and *(ii)* compare speed (`asd` files specify `(speed 3)`).

File **arrays-and-vectors**  
   - functions `vec-view-all-content` and `vec-preview`  
   - function `unliteral--fixnum-vector`
   
File **bit-vectors**  
   - functions `fixnum->bit-vector` and `bit-vector->fixnum`  
   - function `bit-vector-logcount`

File **booleans**  
   - macro `boolean-value`

File **compare-durations**  
   - function `start-up-1`  
   - macro `compare-durations` 
   
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

File **files**  
   - function `random-file-name`  
   - function `reduce-for-each-line-of-file`
   
File **format**  
   - illustrations of various `format` directives

File **functions**  
   - generalities on functions  
   - are arguments passed by reference of by value?
   
File **lists**  
   - function `delete-nth` and `replace-nth`  
   - function `list-preview`  
   - functions `arg-min` and `arg-max`  
   - function `nb-of-occurrences-of-sublist-in-list`  
   - functions `make-circular-DO-NOT-PRINT--AND-NOT-LITERAL` and `circular-list-length`  
   - function `unliteral--fixnum-list`
      
File **macros**  
   - macro `with-gensyms`  
   - macros `while`and `while1`  
   - macros `repeat-until`and `do-while`  
   - macros `aprogn` and `ablock`  
   - macros `setf-min`, `setf-max`, `setf-min--df`, `setf-max--df`  
   - macros `collecting` and `collecting--reversed-order`  
   - macro `let+`
   
File **mail-vis-gmail**  
   - function `send-mail-via-gmail`

File **measure-duration**  
   - function `SHOW-measure-duration`  
   - function `SHOW-benchmark-5-times-A` and `SHOW-benchmark-5-times-B`
   
File **numbers-doubles**  
   - macro `floor-of-positive-df`
   
File **numbers-integers**  
   - function `reverse-number--fixnum`

File **numbers-integers-french**  
   - function `en-toutes-lettres`

File **numbers-integers-primes**  
   - function `largest-prime-factor`  
   - function `primep`  
   - function `next-prime`  
   - function `nth-prime`
   
File **numbers-rationals**  
   - macro `convert-to-proper-fraction-m`  
   - function `length-of-recurring-cycle`
   
File **optimization**  
   - function `optimization-examples`
   
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
   
File **variables**  
   - general considerations on variables  
   - explanations about bindings  
   - lexical vs dynamic scoping
   
File **web**  
   - function `web-redirect-p`

Any comment? Open an [issue](https://github.com/occisn/cl-utils/issues), or start a discussion [here](https://github.com/occisn/cl-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).
   
(end of README)
