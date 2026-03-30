(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:clear-output-translations)
(asdf:load-system "cl-utils" :force t)
(sb-ext:save-lisp-and-die "my-program.exe"
                          :toplevel #'cl-utils::main
                          :executable t)
