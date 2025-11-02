# cl-utils

Personal utilities for Common Lisp.  
\+ considerations on development workflow

Some functions are specific to SBCL.

This repository is not supposed to be forked or included in another project. It is more a kind of shelf with many functions, where each function is self-supporting: it does not require another function of the project (in case it requires another function of the project, the said function is included with `flet` or `labels`); in case of dependency to specific packages (`drakma`...), it is clearly specified.

When relevant, functions and macros are illustrated or checked with `SHOW-xxx`.

A test suite is proposed. In addition to traditional tests, when several implementations of the same function are ptoposed, tests file may *(i)* check that they yield the same returns and *(ii)* compare speed (do not forget to set: `speed = 3` in `asd` files).

Table of contents:  
- [list of functions and macros](#list-of-functions-and-macros)  
- [personal notes](#personal-notes)  
- [minimal ASDF example](#minimal-ASDF-example) --> see [dedicated repository](https://github.com/occisn/cl-minimal-asdf-example)  
- [interactive development workflow in Emacs](#interactive-development-workflow-in-emacs)

## List of functions and macros

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
   
File **numbers-french**  
   - function `en-toutes-lettres`

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

## Minimal ASDF example

See [dedicated repository](https://github.com/occisn/cl-minimal-asdf-example)

## Interactive development workflow in Emacs

This section is not specifically related to functions proposed by `cl-utils`.

My actual Emacs configuration file is available in a [dedicated repository](https://github.com/occisn/emacs-config)

When working :  
- compile function/macro: `C-c C-c`  
- compile buffer: `C-c C-k` (probably to be avoided with ASDF project, since it creates a `fasl` file next to `lisp` file, with a risk of confusion)  
- send function to REPL, ready to be executed: `C-c C-y`  
- send function to REPL, ready to be executed with time measurement: `C-c C-x` (custom function defined in my [Emacs configuration file](https://github.com/occisn/emacs-config))  
- expand macro: `C-c C-m`; fully expand macro: `C-c M-m`

**The rest of this section is specific to ASDF projects.**

To **load** system from Common Lisp REPL, use one of the following instructions:
```
,load-system [cl-my-project]
(asdf:load-system :cl-my-project)
(ql:quickload :cl-my-project)
```

At the beginning of the work session, it could be smarter to load tests system, which also force loading of main system:
```
,load-system [cl-my-project-tests]
(asdf:load-system :cl-my-project-tests)
(ql:quickload :cl-my-project-tests)
``` 

To **test** system from Common Lisp REPL, use one of the following instructions:
```
,test-system [cl-my-project]
(asdf:test-system :cl-my-project)
(parachute:test 'cl-my-project-tests)
```

For a specfic test:
```
(parachute:test 'cl-my-project-tests::test-triple)
```

To **reload** the whole system:
```
(asdf:load-system :cl-my-project :force t)
,force-load-system [cl-my-project]
```

Switch between source and test file: `C-c d s` (custom function defined in my [Emacs configuration file](https://github.com/occisn/emacs-config), accessible via hydra)

Jump to `asd` file: `C-c d a` (custom function defined in my [Emacs configuration file](https://github.com/occisn/emacs-config), accessible via hydra)

Launch tests only related to the current function: `C-c SPC` (custom function defined in my [Emacs configuration file](https://github.com/occisn/emacs-config)) will create relevant instructions in REPL, ready to be executed.

## Personal notes

In case a new function is added in `cl-utils`, it has to be included *(i)* in the list of exported symbols within `package.lisp`, and *(ii)* in this README file.
   
(end of README)
