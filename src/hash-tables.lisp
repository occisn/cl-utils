;;;; Utilities for hash tables.
;;;;
;;;; Useful standard functions for hash tables.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-hash-tables-general ()
  ""

  (let* (
         ;; creation:
         (ht0 (make-hash-table))

         ;; caution if keys are strings:
         (ht (make-hash-table :test #'equal)))

    (declare (ignorable ht0))

    ;; pretty-print :
    (maphash (lambda (key value)
               (format t "~A: ~A~%" key value))
             ht)

    ;; add or replace
    (setf (gethash "a" ht) 2)
    
    ;; get
    (gethash "a" ht)
    
    ;; gethash returns a second value answering to "found?"
    (multiple-value-bind (value found-p)
        (gethash "a" ht)
      ;; do something
      (declare (ignorable value found-p)))
    
    ;; traverse:
    (maphash (lambda (key value)
               ;; do something
               (declare (ignorable key value)))
             ht)

    ;; hash-table-count

    ;; remhash to delete a key

    ;; clrhash to delete everything
    ))

(defun SHOW-all-hash-tables ()
  ""
  (format t "~%~%======~%=== HASH TABLES~%======~%")
  (format t "~%")
  (SHOW-hash-tables-general))

;;; end
