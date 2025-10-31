(in-package :cl-utils-tests)

;;; === string-split

(parachute:define-test test-string-split
  (parachute:is
   equal (list "aa" "b b" "c c c") (cl-utils::string-split "aa,b b,c c c" #\,))
  (parachute:is
   equal (list "aa" "bb" "cc") (cl-utils::string-split "aa bb cc" #\SPACE)))

;;; === substring-after-last

(parachute:define-test test-substring-after-last
  (parachute:is string= "" (cl-utils::substring-after-last "" #\:))
  (parachute:is string= "abc" (cl-utils::substring-after-last "abc" #\:))
  (parachute:is string= "ghi" (cl-utils::substring-after-last "abc:def:::ghi" #\:)) 
  (parachute:is string= "" (cl-utils::substring-after-last "abcdef:" #\:)))

;;; === string-repeat-string

(parachute:define-test test-string-repeat-string
  (parachute:is string= "abc-abc-abc-" (cl-utils::string-repeat-string 3 "abc-"))
  (parachute:is string= "" (cl-utils::string-repeat-string 0 "abc-"))
  (parachute:is string= "" (cl-utils::string-repeat-string 10 "")))

;;; === string-add-space-at-left

(parachute:define-test test-string-add-space-at-left
  (parachute:is string= "   abc" (cl-utils::string-add-space-at-left "abc" :total-length 6)))

;;; === palindrome-string-p

(parachute:define-test test-palindrome-string-p
  (parachute:true (cl-utils::palindrome-string-p "aba"))
  (parachute:true (not (cl-utils::palindrome-string-p "abab"))))

;;; end
