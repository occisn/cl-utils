(in-package :cl-utils)

(defun web-redirect-p (url)
  "Return t is URL is actually redirected, and nil otherwise.
Requires drakma.
(v1 as of 2025-10-30, available in occisn/cl-utils GitHub repository)"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (multiple-value-bind (content status-code headers final-uri)
        (drakma:http-request url)
      (declare (ignore content status-code headers))
      ;; (format t "Final URI: ~a~%" final-uri)
      (not (string= url (format nil "~a" final-uri))))))

;;; end
