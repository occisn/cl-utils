;;;; Utilities for variables.
;;;;
;;;; General considerations on variables; bindings; lexical vs dynamic scoping.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-variable-general ()
  ""

  ;; Variables are UNTYPED containers
  ;; Values are TYPED objects.
  ;; Type declarations are optional optimization hints,
  ;; not type constraints (unless you explicitly check them).

  )

(defun SHOW-variables-bindings ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  
  (let ((x nil)
        (y nil))
    ;; Variables are bound to objects (similar to references/pointers).
    ;; In Lisp, variables hold references to objects, not the objects themselves.
    (setq x 2)
    (setq y x)
    (eq x y)                            ; --> T
    ;; Both X and Y refer to the same object in memory (the number 2).
    ;; EQ tests object identity (whether two references point to the same object).
    (setq x 3)
    y     ; --> 2
    ;; X has been REBOUND to a different object (3),
    ;; whereas Y still refers to 2.
    ;; Note: Numbers are immutable, so we can't modify 2 itself.
    
    (setq x '(a b))
    (setq y x)
    (eq x y)                            ; --> T
    (setf (car x) 'new)
    x  ; --> (NEW B)
    y  ; --> (NEW B)
    ;; The list object that both X and Y refer to has been MUTATED.
    ;; Both variables see the change because they reference the same object.
    
    (setq x '(different))
    x       ; --> (DIFFERENT)
    y       ; --> (NEW B)
    ;; X has been REBOUND to a new list object,
    ;; whereas Y still refers to the original (modified) list.
    ))

(defparameter *x* 10)

(defun SHOW-variables-lexical-vs-dynamic ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  ;; Lexical variables:
  ;; ------------------
  ;; Scope: Determined by textual location in code
  ;; Binding: Variable reference resolved at compile-time
  ;;    (and value accessed at run-time)
  ;; Visible: Only in the code where they're defined
  ;; Declaration: Regular let, let*, function parameters

  ;; Dynamic variables (special variables):
  ;; --------------------------------------
  ;; Scope: Determined by call stack at runtime
  ;; Binding: Resolved at runtime
  ;; Visible: To all functions called in the dynamic extent
  ;; Declaration: defvar, defparameter, or declare special
  ;; Convention: Named with *earmuffs*
  ;;     (which prevents from inadvertenly over-shadow it
  ;;      within a function)
  ;;
  ;; They are mainly for configuration settings, thread-local storage,
  ;; runtime-configurable context that flows down the call stack.

  ;; LEXICAL EXAMPLE

  (let ((x 10))
    (defun get-x () x)
    (defun call-get-x () (get-x)))

  (let ((x 20))                  ; new lexical x
    ;; warning: x defined and not used
    (declare (ignorable x))
    (call-get-x))                ; => 10 (uses x from definition site)

  ;; It illustrates closure capture:
  ;; GET-X closes over the lexical x from its definition environment,
  ;; not the call site.

  ;; DYNAMIC EXAMPLE

  ;; (defparameter *x* 10)
  ;; the previous line has been moved before the defun

  (defun get-*x* () *x*)
  (defun call-get-*x* () (get-*x*))

  (let ((*x* 20))         ; temporarily rebind *x* ; dynamic rebinding
    (call-get-*x*))       ; => 20 (uses x from call site!)

  )

(defun SHOW-all-variables ()
  ""
  (format t "~%~%======~%=== VARIABLES AND BINDINGS~%======~%")
  (format t "~%")
  (SHOW-variable-general)
  (format t "~%")
  (SHOW-variables-bindings)
  (format t "~%")
  (SHOW-variables-lexical-vs-dynamic))

;;; end

