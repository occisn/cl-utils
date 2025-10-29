(defpackage cl-utils--lists
  (:use :cl ))

(in-package :cl-utils--lists)

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
(v1 as of 2025-05-17, available in occisn/cl-utils GitHub repository)"
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))

(defun replace-nth (n new-value lst)
  "Return a list which is the original LST where N-th element is replaced by NEW-VALUE. Not destructive.
(v1 as of 2025-05-18, available in occisn/cl-utils GitHub repository)"
  (loop for elt in lst
        for i from 0
        when (= i n) collect new-value
        unless (= i n) collect elt))

;;; end
