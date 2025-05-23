;; (defpackage cl-utils
;;   (:use :cl ))

(in-package :cl-utils)

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
(v1 as of 2025-05-17)"
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))

(defun replace-nth (n new-value lst)
  "Return a list which is the original LST where N-th element is replaced by NEW-VALUE. Not destructive.
(v1 as of 2025-05-18)"
  (loop for elt in lst
        for i from 0
        when (= i n) collect new-value
        unless (= i n) collect elt))

;;; end
