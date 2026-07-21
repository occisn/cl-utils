;;;; Utilities for sexp.

(in-package :cl-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sexp-replace-sexp-beginning-by (lst prefix replace-builder-fn)
    "Transform sexp LST in the following way: each time a sexp, called sexp1, is found within LST of
format sexp1 = (PREFIX...), it is replaced by (REPLACE-BUILDER-FN sexp1).

Used by macros.

Example:
(sexp-replace-sexp-beginning-by
   '(a (d (submit g h)) c)
   'submit
   (lambda (sexp) (cons 'submit2 (cdr sexp)))))
;;; --> (A (D (SUBMIT2 G H)) C)"
    (declare (type (or atom list) lst)
             (type function replace-builder-fn))
    (cond ((atom lst) lst)
          ((list lst)
           (if (eq prefix (car lst))
               (funcall replace-builder-fn lst)
               (loop for elt in lst collect (sexp-replace-sexp-beginning-by elt prefix replace-builder-fn)))))))
;;; eval-when sinon "It is defined earlier in the file but is not available at compile-time"

(defun SHOW-sexp-replace-sexp-beginning-by ()
  "Example of usage of sexp-replace-sexp-beginning-by."
  (sexp-replace-sexp-beginning-by
   '(a (d (submit g h)) c)
   'submit
   (lambda (sexp) (cons 'submit2 (cdr sexp)))))
;;; --> (A (D (SUBMIT2 G H)) C)

(defun SHOW-all-sexp ()
  ""
  (format t "~%~%======~%=== SEXP~%======~%")
  (format t "~%")
  (format t "~a~%" (SHOW-sexp-replace-sexp-beginning-by)))

;;; end
