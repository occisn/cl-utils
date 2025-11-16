(asdf:defsystem "cl-utils-tests"
  :depends-on ("cl-utils" "parachute")
  :serial t
  :components ((:module "tests"
                :around-compile (lambda (next)
                                  (proclaim '(optimize (debug 0) 
                                              (safety 0)
                                              (speed 3)))
                                  (funcall next))
                :components ((:file "_package-tests")
                             (:file "_macros-for-tests")
                             ;;
                             (:file "bit-vectors-tests")
                             (:file "lists-tests")
                             (:file "macros-tests")
                             (:file "numbers-french-tests")
                             (:file "numbers-primes-tests")
                             (:file "strings-tests")
                             (:file "symbols-tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test  :cl-utils-tests)))
