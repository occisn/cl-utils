(asdf:defsystem "cl-utils"
  :name "cl-utils"
  ;; :version "1"
  :author "Nicolas Occis"
  :licence "MIT"
  :description "Personal utilities for Common Lisp"
  ;; :long-description "A long description"
  :depends-on (:parachute ; for tests
               :cl-smtp
               :drakma
               )
  :serial t ; load files in order
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 0) 
                                (safety 0)
                                (speed 3)))
                    (funcall next))
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "arrays-and-vectors")
                 (:file "bit-vectors")
                 (:file "booleans")
                 (:file "compare-durations")
                 (:file "dates-and-times")
                 (:file "debug")
                 (:file "files")
                 (:file "lists")
                 (:file "macros")
                 (:file "mail-via-gmail")
                 (:file "numbers-french")
                 (:file "numbers-rationals")
                 (:file "strings")
                 (:file "symbols")
                 (:file "triangles")
                 (:file "web"))
                ))
  :perform (load-op :after (op c)
                    (format t "~%Welcome in cl-utils! (~a exported symbols)~%"
                            (let ((package-name :cl-utils)
                                  (count 0))
                              (do-external-symbols (sym package-name count)
                                (when (eq (symbol-package sym) (find-package package-name))
                                  (incf count))))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-utils-tests))))

