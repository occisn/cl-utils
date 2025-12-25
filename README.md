# cl-utils

Personal utilities for Common Lisp.  

Some functions are specific to SBCL.

This project is a kind of shelf providing many functions. With a few clearly indicated exceptions, these functions are self-supporting. They do not require any dependency, or to be built/integrated in any specific way. Because "the truly reusable code is the one that you can simply copy-paste".

When relevant, functions and macros are illustrated with `SHOW-xxx`.

A test suite is proposed. In addition to traditional tests, when several implementations of the same function are proposed, tests file may *(i)* check that they yield the same returns and *(ii)* compare speed (do not forget to set: `speed = 3` in `asd` files).

Minimal ASDF example : see [dedicated repository](https://github.com/occisn/cl-minimal-asdf-example)

File **arrays-and-vectors.lisp**  
   - functions `vec-view-all-content` and `vec-preview`  
   - function `unliteral--fixnum-vector`
   
File **bit-vectors.lisp**  
   - functions `fixnum->bit-vector` and `bit-vector->fixnum`  
   - function `bit-vector-logcount`

File **booleans.lisp**  
   - macro `boolean-value`

File **compare-durations.lisp**  
   - function `start-up-1`  
   - macro `compare-durations` 
   
File **dates-and-times.lisp**  
   - function `universal-time-to-YYYYMMDD-HHMMSS`  
   - function `get-current-YYYYMMDD-HHMMSS`  
   - functions `convert-int-YYYYMMDD-and-HHMMSS-to-universal-time` and `convert-int-YYYYMMDD-to-universal-time`  
   - function `universal-time-to-YYYY-MM-DD`  
   - functions `pretty-print-universal-time-as-full-date-time`, `pretty-print-universal-time-as-long-date` and `pretty-print-universal-time-as-short-date`  
   - function `pretty-print-time-difference`

File **debug.lisp**  
   - function `debug-output`

File **files.lisp**  
   - function `random-file-name`  
   - function `reduce-for-each-line-of-file`
   
File **lists.lisp**  
   - function `delete-nth` and `replace-nth`  
   - function `list-preview`  
   - functions `arg-min` and `arg-max`  
   - function `nb-of-occurrences-of-sublist-in-list`  
   - functions `make-circular-DO-NOT-PRINT--AND-NOT-LITERAL` and `circular-list-length`  
   - function `unliteral--fixnum-list`
      
File **macros.lisp**  
   - macro `with-gensyms`  
   - macros `while`and `while1`  
   - macros `repeat-until`and `do-while`  
   - macros `aprogn` and `ablock`  
   - macros `setf-min`, `setf-max`, `setf-min--df`, `setf-max--df`  
   - macros `collecting` and `collecting--reversed-order`
   
File **mail-vis-gmail.lisp**  
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
   
File **strings.lisp**  
   - function `string-split`  
   - function `substring-after-last`  
   - function `string-repeat-string`  
   - function `string-add-space-at-left`  
   - function `duplicate-strings-in-list`  
   - function `palindrome-string-p`  
   - function `unliteral--string`

File **symbols.lisp**  
   - function `function-to-string` and `function-to-string-no-package`
   
File **triangles.lisp**  
   - function `triangle-to-2d-array`
   
File **web.lisp**  
   - function `web-redirect-p`

Any comment? Open an [issue](https://github.com/occisn/cl-utils/issues), or start a discussion [here](https://github.com/occisn/cl-utils/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).
   
(end of README)
