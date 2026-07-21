(asdf:defsystem "cl-utils-tests"
  :version "1.0.0"
  :author "Nicolas Occis"
  :license "MIT"
  :description "Test suite for cl-utils."
  :depends-on ("cl-utils" "parachute")
  :serial t
  :components ((:module "tests"
                ;; Compilation policy for every file of the system, set in one place.
                ;;
                ;; Beware: PROCLAIM is global and permanent.  It is not undone when this
                ;; system has finished compiling, so the setting below stays in force for
                ;; the rest of the session and applies to every other system compiled
                ;; afterwards in the same image.
                :around-compile (lambda (next)
                                  (proclaim '(optimize (debug 0) 
                                              (safety 0)
                                              (speed 3)))
                                  (funcall next))
                :components ((:file "_package-tests")
                             (:file "_macros-for-tests")
                             ;;
                             (:file "arrays-and-vectors-tests")
                             (:file "bit-vectors-tests")
                             (:file "booleans-tests")
                             (:file "dates-and-times-tests")
                             (:file "lists-tests")
                             (:file "macros-tests")
                             (:file "numbers-doubles-tests")
                             (:file "numbers-integers-tests")
                             (:file "numbers-integers-french-tests")
                             (:file "numbers-integers-primes-tests")
                             (:file "numbers-rationals-tests")
                             (:file "continued-fractions-tests")
                             (:file "farey-sequences-tests")
                             (:file "sorting-tests")
                             (:file "permutations-tests")
                             (:file "combinations-tests")
                             (:file "high-order-functions-tests")
                             (:file "math-functions-tests")
                             (:file "files-tests")
                             (:file "strings-tests")
                             (:file "symbols-tests")
                             (:file "triangles-tests")
                             (:file "search-tests")
                             (:file "sexp-tests"))))
  ;; PARACHUTE:TEST returns a result object that is true whether the suite
  ;; passed or not, so its status has to be inspected explicitly.  Without
  ;; this, (asdf:test-system "cl-utils") reports success on a failing suite --
  ;; including on tests that error out before running, which Parachute counts
  ;; as neither passed nor failed.
  :perform (asdf:test-op (op c)
                         (declare (ignore op c))
                         (let ((result (uiop:symbol-call :parachute :test
                                                         :cl-utils-tests)))
                           (unless (eq (uiop:symbol-call :parachute :status result)
                                       :passed)
                             (error "Test suite cl-utils-tests failed.")))))
