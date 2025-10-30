# cl-utils

Personal utilities for Common Lisp.

When relevant, functions and macros are illustrated or checked with `SHOW-xxx` or `TEST-xxx` functions.

File **arrays-and-vectors.lisp**  
   - functions `vec-view-all-content` and `vec-preview`  
   - function `unliteral--fixnum-vector`
   
File **bit-vectors.lisp**  
   - functions `fixnum->bit-vector` and `bit-vector->fixnum`  
   - function `bit-vector-logcount`

File **booleans.lisp**  
   - macro `boolean-value`
   
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
   
File **french-numbers**  
   - function `en-toutes-lettres`
   
File **macros.lisp**  
   - macro `with-gensyms`  
   - macros `while`and `while1`  
   - macros `repeat-until`and `do-while`  
   - macros `aprogn` and `ablock`  
   - macros `setf-min`, `setf-max`, `setf-min--df`, `setf-max--df`  
   - macros `collecting` and `collecting--reversed-order`
   
File **mail-vis-gmail.lisp**  
   - function `send-mail-via-gmail`
   
File **numbers-rationals**  
   - macro `convert-to-proper-fraction-m`  
   - function `length-of-recurring-cycle`
   
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
