;;;; Utilities for ffi with cffi.
;;;;
;;;; Demonstration of a foreign-function interface via CFFI,
;;;; https://cffi.common-lisp.dev/ -- see SHOW-ffi-with-cffi.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

;; Define shared library (path computed relative to this source file)
(cffi:define-foreign-library lib-ffi-with-cffi
  (:windows
   #.(namestring
      (merge-pathnames "ffi-c-library/ffi.dll"
                       (make-pathname :directory (pathname-directory *compile-file-pathname*)))))
  (:unix
   #.(namestring
      (merge-pathnames "ffi-c-library/ffi.so"
                       (make-pathname :directory (pathname-directory *compile-file-pathname*))))))

;; Load the shared library
(cffi:use-foreign-library lib-ffi-with-cffi)

;; Define alien functions
(cffi:defcfun ("add" add-1) :int
  (a :int)
  (b :int))

(cffi:defcfun ("multiply_floats" multiply-floats-1) :float
  (a :float)
  (b :float))

(cffi:defcfun ("print_string" print-string-1) :void
  (str :string))

;; Define Point struct
(cffi:defcstruct point-1
  (x :int)
  (y :int))

;; Define move_point function
(cffi:defcfun ("move_point" move-point-1) :void
  (p (:pointer (:struct point-1)))
  (dx :int)
  (dy :int))

;; Define double_array function
(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (cffi:defcfun ("double_array" double-array-1) (:pointer :int)
      (arr (:pointer :int))
      (length :int)))

;; Define free_double_array function
(cffi:defcfun ("free_double_array" free-double-array-1) :void
  (arr (:pointer :int)))

;; Example of usage
(defun SHOW-ffi-with-cffi ()
  "Demonstrate FFI-WITH-CFFI."

  (format t "~%~%======~%=== FFI WITH CFFI~%======~%")
  (format t "~%")
  
  ;; Test add
  (format t "2 + 3 = ~a~%" (add-1 2 3))

  ;; Test multiply_floats
  (format t "2.5 * 3.5 = ~a~%" (multiply-floats-1 2.5 3.5))

  ;; Test print_string
  (print-string-1 "Hello from CFFI!")

  ;; Test move_point
  (cffi:with-foreign-object (p '(:struct point-1))
    (setf (cffi:foreign-slot-value p '(:struct point-1) 'x) 10)
    (setf (cffi:foreign-slot-value p '(:struct point-1) 'y) 20)
    (format t "Before: x=~a, y=~a~%"
            (cffi:foreign-slot-value p '(:struct point-1) 'x)
            (cffi:foreign-slot-value p '(:struct point-1) 'y))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (move-point-1 p 5 10))
    (format t "After: x=~a, y=~a~%"
            (cffi:foreign-slot-value p '(:struct point-1) 'x)
            (cffi:foreign-slot-value p '(:struct point-1) 'y)))

  ;; Test double_array
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cffi:with-foreign-object (arr :int 5)
        ;; Initialize array
        (dotimes (i 5)
          (setf (cffi:mem-aref arr :int i) (1+ i)))
        (format t "Original array: ~a~%"
                (loop for i from 0 below 5 collect (cffi:mem-aref arr :int i)))

        ;; Call double_array
        (let ((result (double-array-1 arr 5)))
          ;; Print returned array
          (format t "Doubled array: ~a~%"
                  (loop for i from 0 below 5 collect (cffi:mem-aref result :int i)))
          ;; free memory allocated to arrayby double_array
          (free-double-array-1 result)))))

;;; end
