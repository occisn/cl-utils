
(eval-when (:compile-toplevel)
  (ql:quickload :drakma))

(defpackage cl-utils--web
  (:use :cl :drakma))

(in-package :cl-utils--web)

(defun web-redirect-p (url)
  "Return t is URL is actually redirected, and nil otherwise.
Requires drakma.
(v1 as of 2025-10-30, available in occisn/cl-utils GitHub repository)"
  (multiple-value-bind (content status-code headers final-uri)
      (drakma:http-request url)
    (declare (ignore content status-code headers))
    (not (string= url (format nil "~a" final-uri)))))

;;; end
