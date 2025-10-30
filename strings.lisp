(defpackage cl-utils--strings
  (:use :cl ))

(in-package :cl-utils--strings)

(defun string-split (str delimiter)
  "Split STR according to DELIMITER and return a list.
For instance: 'abc def' #\space --> ('abc' 'def')
(v1, available in occisn/cl-utils GitHub repository)"
  (if (null str)
      nil
      (locally
          (declare (type (simple-array character) str))
        (loop for start = 0 then (1+ finish)
              for finish = (position delimiter str :start start)
              collecting (subseq str start finish)
              until (null finish)))))

(defun substring-after-last (str chr)
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
         (subseq str (+ idx 1) )))))

(defun string-repeat-string (n str)
  "Return a string consisting of the string STR repeated N times.
For instance: 3 'abc' --> 'abcabcabc'
(v1, available in occisn/cl-utils GitHub repository)"
  (format nil "~v@{~A~:*~}" n str))

(defun string-add-space-at-left (str &key total-length)
  "Add spaces at the left of the string STR so that the length becomes TOTAL-LENGTH. Return the completed string.
For instance: 'abc' :total-length 6 --> '   abc'
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum total-length)
           (type string str))
  (let* ((len (length str))
         (nb-spaces (- total-length len)))
    (declare (type fixnum len nb-spaces))
    (when (< total-length len) (error "Could not add space at left of ~s since its length is ~s whereas total-length is ~s" str len total-length))
    (let ((leftspaces (string-repeat-string nb-spaces " ")))
      (format nil "~a~a" leftspaces str))))

(defun duplicate-strings-in-list (lst)
  "Return the list of duplicate strings in the list of strings LST.
For instance: ('abc' 'def' 'abc') --> ('abc')
(v1, available in occisn/cl-utils GitHub repository)"
  (cond ((null lst) '())
        ((member (car lst) (cdr lst) :test #'string=) (cons (car lst) (duplicate-strings-in-list (cdr lst))))
        (t (duplicate-strings-in-list (cdr lst)))))
;; do not inline, since recursive
;; https://stackoverflow.com/questions/27757670/finding-duplicate-elemntes-in-a-list-with-lisp-language

(defun palindrome-string-p (str)
  "Return true if and only if the argument STR is a palindrome string.
For instance: 'aba' --> t.
The argument is supposed to be a string with at least one character.
(v1, available in occisn/cl-utils GitHub repository)"
  (declare (type (simple-array character) str))
  (loop
    for i of-type fixnum from 0
    for a across str
    for b across (reverse str)
    always (char= a b)
    until (> i (ash (length str) -1))))
;; https://gist.github.com/Denommus/8530515

(declaim (ftype (function ((simple-array character)) (simple-array character)) unliteral--string))
(defun unliteral--string (str)
  "Return an unliteral version of string STR.
(v1, available in occisn/cl-utils GitHub repository)"
    (declare (type (simple-array character) str))
    (make-array (length str) :element-type 'character :initial-contents str))

;;; end
