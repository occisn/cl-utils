(defpackage cl-utils--symbols
  (:use :cl))

(in-package :cl-utils--symbols)

(defun function-to-string (fn)
  "Return a string corresponding to function FN.
For instance #'SIN --> 'SIN'
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type function fn))
  (format nil "~a" (nth-value 2 (function-lambda-expression fn))))

(defun function-to-string-no-package (fn)
  "Return a string corresponding to function FN without the possible initial part corresponding to the package. This initial part is identified through ':'.
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type function fn))

  (labels ((substring-after-last (str chr)
             "Return the last substring of STR after character CHAR.
For instance: 'abc::def' #\: --> 'def'
(v1, available in occisn/cl-utils GitHub repository)"
             (declare (type character chr)
                      (type (simple-array character) str))
             (if
              (= 0 (length str))
              str
              (let ((idx
                      (loop with res of-type fixnum = 0
                            for c of-type character across str
                            for i of-type fixnum from 0
                            when (char= c chr)
                              do (setq res i)
                            finally (return res))))
                (if (= idx 0)
                    str
                    (subseq str (+ idx 1) )))))) ; end of labels definitions
    
    (substring-after-last (function-to-string fn) #\:)))

;;; end
