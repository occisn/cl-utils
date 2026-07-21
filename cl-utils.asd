(asdf:defsystem "cl-utils"
  :version "1.0.0"
  :author "Nicolas Occis"
  :maintainer "Nicolas Occis"
  :license "MIT"
  :homepage "https://github.com/occisn/cl-utils"
  :description "Personal utilities for Common Lisp"
  :depends-on (#:bordeaux-threads
               #:cffi
               #:cl-smtp
               #:dexador
               #:drakma
               #:jonathan
               #:cl-cpus
               #:lparallel
               #:sb-concurrency
               #:zpng
               )
  :serial t ; load files in order
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
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "arrays-and-vectors")
                 (:file "association-lists")
                 (:file "bit-vectors")
                 (:file "booleans")
                 (:file "compare-durations")
                 (:file "dates-and-times")
                 (:file "debug")
                 (:file "equality")
                 (:file "ffi-with-cffi")
                 (:file "ffi-with-sb-alien")
                 (:file "files")
                 (:file "format")
                 (:file "functions")
                 (:file "hash-tables")
                 (:file "macros")
                 (:file "sexp")
                 (:file "high-order-functions")
                 (:file "images")
                 (:file "lazy")
                 (:file "lists")
                 (:file "mail-via-gmail")
                 (:file "numbers-doubles")
                 (:file "optimization")
                 (:file "math-functions")
                 (:file "memoization")
                 (:file "measure-duration")
                 (:file "numbers-integers")
                 (:file "numbers-integers-french")
                 (:file "numbers-integers-primes")
                 (:file "numbers-rationals")
                 (:file "continued-fractions")
                 (:file "farey-sequences")
                 (:file "sorting")
                 (:file "permutations")
                 (:file "combinations")
                 (:file "search")
                 (:file "gnuplot")
                 (:file "console-charts")
                 (:file "os-interaction-windows")
                 (:file "ollama")
                 (:file "parallelism")
                 (:file "property-lists")
                 (:file "strings")
                 (:file "symbols")
                 (:file "tco")
                 (:file "trampoline")
                 (:file "triangles")
                 (:file "types")
                 (:file "variables")
                 (:file "web")
                 (:file "_show-all"))))
  :perform (load-op :after (op c)
                    (format t "~%Welcome in cl-utils! (~a exported symbols)~%~%Execute all demonstrations with (cl-utils::SHOW-all-cl-utils).~%A complete test suite is also available.~%~%"
                            (let ((package-name :cl-utils)
                                  (count 0))
                              (do-external-symbols (sym package-name count)
                                (when (eq (symbol-package sym) (find-package package-name))
                                  (incf count))))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-utils-tests))))

