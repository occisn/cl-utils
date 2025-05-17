(defpackage cl-utils
  (:use :cl ))

(in-package :cl-utils)

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
(v1 as of 2025-05-17, from cl-utils)"
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))
;;; end
