(in-package :cl-utils)

;;; ==========================================
;;; === OS INTERACTION (WINDOWS-SPECIFIC)  ===
;;; ==========================================
;;;
;;; These utilities are Windows-specific and require a Windows environment.
;;; Browser functions use Windows 'start' command.
;;; Clipboard functions use Win32 API via CFFI.
;;;

;;; ===
;;; === BROWSER ===
;;; ===

(defun open-html-file-with-default-browser (filename)
  "Open FILENAME with the default browser.
Windows-specific: uses the 'start' command."
  (uiop:run-program (format nil "start ~a" filename)))

(defun open-url-with-default-browser (url)
  "Open URL with the default browser (Firefox).
Windows-specific: uses the 'start firefox' command."
  (uiop:run-program (format nil "start firefox ~s" url)))

;;; ===
;;; === CLIPBOARD ===
;;; ===

(declaim (type character +field-delimiter-for-export-to-clipboard+))
(defparameter +field-delimiter-for-export-to-clipboard+ #\TAB)

(declaim (inline convert-double-float-to-string-ready-for-clipboard-towards-Excel))
(defun convert-double-float-to-string-ready-for-clipboard-towards-Excel (x)
  "Convert double-float X into a string ready to be pasted to clipboard for Excel,
i.e. with decimal dot replaced by comma.
Windows-specific."
  (declare (type double-float x))
  (let ((df1 (format nil "~f" x)))
    (substitute #\, #\. df1)))

(declaim (inline convert-single-float-to-string-ready-for-clipboard-towards-Excel))
(defun convert-single-float-to-string-ready-for-clipboard-towards-Excel (x)
  "Convert single-float X into a string ready to be pasted to clipboard for Excel,
i.e. with decimal dot replaced by comma.
Windows-specific."
  (declare (type single-float x))
  (let ((df1 (format nil "~f" x)))
    (substitute #\, #\. df1)))

(defconstant +win32-gmem-moveable+ 2)
(defconstant +win32-gmem-ddeshare+ 8192)
(defconstant +win32-cf-unicodetext+ 13)

(cffi:load-foreign-library '(:default "User32"))

(defun copy-string-to-clipboard (string)
  "Send STRING to the Windows clipboard via Win32 API (CFFI).
Windows-specific."
  (declare (type (simple-array character) string))
  (let ((cffi:*default-foreign-encoding* :utf-16le))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cffi:with-foreign-string (cstring string)
        (let ((hmem
                (cffi:foreign-funcall "GlobalAlloc"
                                      :uint #.(logior +win32-gmem-ddeshare+
                                                      +win32-gmem-moveable+)
                                      :uint (* 2 (1+ (cffi:foreign-funcall "wcslen"
                                                                           :pointer cstring
                                                                           :uint)))
                                      :pointer)))
          (when (cffi:null-pointer-p hmem)
            (error "GlobalAlloc failed."))
          (handler-bind
              ((error (lambda (c)
                        (declare (ignore c))
                        (cffi:foreign-funcall "GlobalFree" :pointer hmem :pointer))))
            (unwind-protect
                 (cffi:foreign-funcall "wcscpy"
                                       :pointer (cffi:foreign-funcall "GlobalLock"
                                                                      :pointer hmem
                                                                      :pointer)
                                       :pointer cstring
                                       :pointer)
              (cffi:foreign-funcall "GlobalUnlock" :pointer hmem))
            (unwind-protect
                 (progn
                   (or (cffi:foreign-funcall "OpenClipboard"
                                             :pointer (cffi:null-pointer)
                                             :boolean)
                       (error "OpenClipboard failed."))
                   (or (cffi:foreign-funcall "EmptyClipboard" :boolean)
                       (error "EmptyClipboard failed."))
                   (or (not (cffi:null-pointer-p
                             (cffi:foreign-funcall "SetClipboardData"
                                                   :uint +win32-cf-unicodetext+
                                                   :pointer hmem
                                                   :pointer)))
                       (error "SetClipboardData failed.")))
              (cffi:foreign-funcall "CloseClipboard" :boolean))))))))

(defmacro with-export-to-clipboard (flag &body body)
  "Macro creating an environment for export to the Windows clipboard.
FLAG is a boolean stating if we actually want to copy to clipboard.
Within BODY, macro 'export1' is available with the syntax:
  (export1 (:string \"text\") :nc (:fixnum 4) :nl (:double-float 5.21d0))
where :nc stands for new-column (TAB) and :nl stands for new-line.
Windows-specific."
  (let ((stream1 (gensym "stream")))
    `(let ((,stream1 (make-string-output-stream)))
       ,(sexp-replace-sexp-beginning-by
         `(progn ,@body)
         'export1
         (lambda (sexp)
           `(when ,flag
              ,@(loop for elt in (cdr sexp)
                      collect (cond ((eq :nc elt)
                                     `(format ,stream1 "~c" +field-delimiter-for-export-to-clipboard+))
                                    ((eq :nl elt)
                                     `(format ,stream1 "~%"))
                                    ((and (consp elt) (= 2 (length elt)))
                                     (let ((type (car elt))
                                           (x (cadr elt)))
                                       (cond
                                         ((eql type :string) `(format ,stream1 "~a" ,x))
                                         ((eql type :fixnum) `(format ,stream1 "~a" ,x))
                                         ((eql type :single-float) `(format ,stream1 "~a" (convert-single-float-to-string-ready-for-clipboard-towards-Excel ,x)))
                                         ((eql type :double-float) `(format ,stream1 "~a" (convert-double-float-to-string-ready-for-clipboard-towards-Excel ,x)))
                                         (t (error "Unrecognized element type: ~a" elt)))))
                                    (t (error "Malformed element: ~a" elt)))))))
       (when ,flag (copy-string-to-clipboard (get-output-stream-string ,stream1)))
       nil)))

;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-os-interaction-windows ()
  ""
  (format t "~%~%======~%=== OS-INTERACTION-WINDOWS~%======~%")
  (format t "~%")
  (format t "open-html-file-with-default-browser: available (Windows-specific)~%")
  (format t "open-url-with-default-browser: available (Windows-specific)~%")
  (format t "copy-string-to-clipboard: available (Windows-specific)~%")
  (format t "with-export-to-clipboard: available (Windows-specific)~%"))

;;; end
