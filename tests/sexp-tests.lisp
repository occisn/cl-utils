;;;; Tests for the sexp utilities.

(in-package :cl-utils-tests)

;;; === sexp-replace-sexp-beginning-by

(parachute:define-test sexp-replace-sexp-beginning-by-test

  ;; Basic replacement (the docstring example)
  (parachute:is equal
                '(a (d (submit2 g h)) c)
                (sexp-replace-sexp-beginning-by
                 '(a (d (submit g h)) c)
                 'submit
                 (lambda (sexp) (cons 'submit2 (cdr sexp)))))

  ;; No match: nothing changes
  (parachute:is equal
                '(a (b c) d)
                (sexp-replace-sexp-beginning-by
                 '(a (b c) d)
                 'z
                 (lambda (sexp) (cons 'replaced (cdr sexp)))))

  ;; Match at top level
  (parachute:is equal
                '(new b c)
                (sexp-replace-sexp-beginning-by
                 '(old b c)
                 'old
                 (lambda (sexp) (cons 'new (cdr sexp)))))

  ;; Multiple matches: replacement stops recursion into matched sexp
  (parachute:is equal
                '((y 1) (y (x 2)))
                (sexp-replace-sexp-beginning-by
                 '((x 1) (x (x 2)))
                 'x
                 (lambda (sexp) (cons 'y (cdr sexp)))))

  ;; Atom input: returned as-is
  (parachute:is eq
                42
                (sexp-replace-sexp-beginning-by
                 42
                 'x
                 (lambda (sexp) (declare (ignore sexp)) 'replaced)))

  ;; nil input
  (parachute:is eq
                nil
                (sexp-replace-sexp-beginning-by
                 nil
                 'x
                 (lambda (sexp) (declare (ignore sexp)) 'replaced))))

;;; end
