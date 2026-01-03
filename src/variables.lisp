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

(defun SHOW-all-variables ()
  ""
  (format t "~%~%======~%=== VARIABLES AND BINDINGS~%======~%")
  (format t "~%")
  (SHOW-variable-general)
  (format t "~%")
  (SHOW-variables-bindings))

;;; end

