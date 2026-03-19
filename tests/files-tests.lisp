(in-package :cl-utils-tests)

;;; === reduce-for-each-line-of-file

(defparameter +tests-input-files-directory+
  (namestring
   (asdf:system-relative-pathname "cl-utils-tests" "tests/tests-input-files/")))

(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parachute:define-test reduce-for-each-line-of-file-test
    (parachute:is = 23 (reduce-for-each-line-of-file
                        #'+
                        (concatenate 'string +tests-input-files-directory+ "test-reduce-for-each-line-of-file.txt")
                        :key (lambda (s)
                               (let ((i (parse-integer s)))
                                 (if (or (= 0 (mod i 3)) (= 0 (mod i 5))) i 0)))))))

;;; === end
