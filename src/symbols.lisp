(in-package :cl-utils)

;; General explanations at the end of the file.

(defun function-to-string (fn)
  "Return a string corresponding to function FN.
For instance #'SIN --> 'SIN'
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type function fn))
  (format nil "~a" (nth-value 2 (function-lambda-expression fn))))

(defun function-to-string-no-package (fn)
  "Return a string corresponding to function FN without the possible initial part corresponding to the package. This initial part is identified through ':'.
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type function fn))

  (flet ((substring-after-last (str chr)
           "Return the last substring of STR after character CHAR.
For instance: 'abc::def' #\: --> 'def'
(v1, available in occisn/cl-utils GitHub repository)"
           (declare (type character chr)
                    ;; (type (simple-array character) str)
                    
                    )
           (if
            (= 0 (length str))
            str
            (let ((idx
                   (loop with res of-type fixnum = 0
                         for c of-type character across str
                         for i of-type fixnum from 0
                         when (char= c chr)
                         do (setq res i)
                         finally (return res))))
              (if (= idx 0)
                  str
                  (subseq str (+ idx 1) )))))) ; end of labels definitions
    
    (substring-after-last
     (format nil "~a" (nth-value 2 (function-lambda-expression fn)))
     #\:)))

(defun SHOW-all-symbols ()
  ""
  (format t "~%~%======~%=== SYMBOLS~%======~%")
  (format t "~%")
  (format t "#'sin --> ~a~%" (function-to-string #'sin))
  (format t "~%")
  (format t "#'sin --> ~a~%" (function-to-string #'sin)))


;; A symbol is a name that can refer to values, functions, properties,
;; and more — all at the same time.
;; It is also a first-class object that we can manipulate.

;; It has 5 components:
;; Name: A string that identifies the symbol (like FOO or MY-VARIABLE)
;; Value cell: Can hold a value when the symbol is used as a variable
;; Function cell: Can hold a function definition when the symbol names a function
;; Property list: A list of key-value pairs for storing additional attributes
;; Package: The package to which the symbol belongs

;; Symbols are "interned" in packages, meaning that within a given package,
;; there's only one symbol with a particular name. When you type FOO twice,
;; you get the same symbol object both times. 

(locally

    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (defvar *foo8* 5)
  ;; IMPORTANT: we use a DYNAMIC variable
  ;; since LEXICAL variables (let bindings) are resolved
  ;; at compile time and don't use the symbol's value cell

  (defun *foo8* (n) (* 2 n))

  ;; is a symbol?
  ;; ------------
  (symbolp '*foo8*)                      ; => T

  ;; name:
  ;; -----
  (symbol-name '*foo8*)                  ; ==> "*FOO8*"

  ;; package:
  ;; --------
  (symbol-package '*foo8*)          ; ==> #<PACKAGE "COMMON-LISP-USER">

  ;; value:
  ;; ------
  *foo8*                                 ; ==> 5
  (symbol-value '*foo8*)                 ; ==> 5
  (boundp '*foo8*)                       ; => T (if *foo8* has a value)
  (set '*foo8* 10)
  *foo8*                                 ; ==> 10
  (setf *foo8* 20)
  *foo8*                                 ; ==> 20
  (setf (symbol-value '*foo8*) 30)
  *foo8*                                 ; ==> 30

  ;; function:
  ;; ---------
  (fboundp '*foo8*)            ; => T (if *foo8* has a function binding)
  (symbol-function '*foo8*)    ; ==> #<FUNCTION *FOO8*>
  (setf (symbol-function '*foo8*) (lambda (n) (* 3 n)))
  (*foo8* 5)                             ; ==> 15

  ;; mix of value and function:
  ;; --------------------------
  (*foo8* *foo8*)                         ; ==> 90 (30 x 3)
  (funcall #'*foo8* *foo8*)               ; ==> 90
  (apply #'*foo8* (list *foo8*))          ; ==> 90

  ;; property list:
  ;; --------------
  (symbol-plist '*foo8*)                 ; ==> NIL
  (setf (get '*foo8* 'color) 'red)
  (get '*foo8* 'color)                   ; ==> RED
  (symbol-plist '*foo8*)                 ; ==> (COLOR RED)

  ;; know full content:
  ;; ------------------

  ;; (1/3)
  ;; (describe '*foo8*)
  ;; ==>
  ;; COMMON-LISP-USER::*FOO8*
  ;;   [symbol]
  ;;
  ;; *FOO8* names a special variable:
  ;;   Value: 30
  ;;
  ;; *FOO8* names a compiled function:
  ;;   Lambda-list: (N)
  ;;   Derived type: (FUNCTION (T) (VALUES NUMBER &OPTIONAL))
  ;;   Source file: c:/Users/noccis/Downloads/tmp.lisp
  ;;
  ;; Symbol-plist:
  ;;   COLOR -> RED

  ;; (2/3)
  ;; (inspect '*foo8*) ; then q
  ;; ==>
  ;; The object is a SYMBOL.
  ;; 0. Name: "*FOO8*"
  ;; 1. Package: #<PACKAGE "COMMON-LISP-USER">
  ;; 2. Value: 30
  ;; 3. Function: #<FUNCTION (LAMBDA (N) :IN "c:/Users/noccis/Downloads/tmp.lisp") {2264ED3B}>
  ;; 4. Plist: (COLOR RED)

  ;; (3/3)
  ;; C-c I (do not forget the quote)
  ;; ==>
  ;; #<SYMBOL {20322BAF}>
  ;; --------------------
  ;; Its name is: "*FOO8*"
  ;; It is a global variable bound to: 30 [unbind]
  ;; It is a function: #<FUNCTION (LAMBDA (N) :IN "c:/Users/noccis/Downloads/tmp.lisp") {2264ED3B}> [unbind]
  ;; It is internal to the package: COMMON-LISP-USER [export] [unintern]
  ;; Property list: (COLOR RED)
  
  ;; interned or uninterned symbols:
  ;; -------------------------------
  (make-symbol "TEMP9")      ; => #:TEMP (uninterned)
  (gensym)                  ; => #:G1234 (unique, uninterned)
  (gensym "VAR") ; => #:VAR1235
  (intern "FOO9")           ; => FOO, :INTERNAL
  (unintern 'my-symbol4)

  ;; keywords:
  ;; ---------
  ;; In Common Lisp, keywords are a specific kind of symbol.
  ;; They belong to the KEYWORD package
  ;; They are self-evaluating
  ;; They are usually written with a leading colon: :foo
  ;; They are globally accessible
  ;; They are typically used to name function arguments, as labels in lists, or as symbolic constants.
  ;;
  ;; (keywordp :key)  ; => T
  ;; (intern "BAR" :keyword)  ; => :BAR, :EXTERNAL

  )

;;; end

