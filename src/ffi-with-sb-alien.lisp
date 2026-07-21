;;;; Utilities for ffi with sb alien.
;;;;
;;;; Demonstration of foreign-function interface via SBCL's `sb-alien` (`SHOW-ffi-with-sb-alien`).
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

;; Load the shared library (path computed relative to this source file)
(sb-alien:load-shared-object
 #.(namestring
    (merge-pathnames #+windows "ffi-c-library/ffi.dll"
                     #-windows "ffi-c-library/ffi.so"
                     (make-pathname :directory (pathname-directory *compile-file-pathname*)))))

;; Define the alien types and functions
(sb-alien:define-alien-routine ("add" add-2) sb-alien:int
  (a sb-alien:int) (b sb-alien:int))

(sb-alien:define-alien-routine ("multiply_floats" multiply-floats-2) sb-alien:float
  (a sb-alien:float) (b sb-alien:float))

(sb-alien:define-alien-routine ("print_string" print-string-2) sb-alien:void
  (str sb-alien:c-string))

;; Define the Point struct and its function
(sb-alien:define-alien-type nil
  (sb-alien:struct point-2
    (x sb-alien:int)
    (y sb-alien:int)))

(sb-alien:define-alien-routine ("move_point" move-point-2) sb-alien:void
  (p (* (sb-alien:struct point-2)))
  (dx sb-alien:int)
  (dy sb-alien:int))

;; Define the array doubling function
(locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine ("double_array" double-array-2) (* sb-alien:int)
      (arr (* sb-alien:int))
      (length sb-alien:int)))

(sb-alien:define-alien-routine ("free_double_array" free-double-array-2) sb-alien:void
  (arr (* sb-alien:int)))

;; Example of usage
(defun SHOW-ffi-with-sb-alien ()
  "Demonstrate FFI-WITH-SB-ALIEN."

  (format t "~%~%======~%=== FFI WITH SB-ALIEN~%======~%")
  (format t "~%")
  
  ;; Test add
  (format t "2 + 3 = ~a~%" (add-2 2 3))

  ;; Test multiply_floats
  (format t "2.5 * 3.5 = ~a~%" (multiply-floats-2 2.5 3.5))

  ;; Test print_string
  (print-string-2 "Hello from SBCL and sb-alien!")

  ;; Test move_point
  (let ((p (sb-alien:make-alien (sb-alien:struct point-2))))
    (setf (sb-alien:slot p 'x) 10)
    (setf (sb-alien:slot p 'y) 20)
    (format t "Before: x=~a, y=~a~%" (sb-alien:slot p 'x) (sb-alien:slot p 'y))
    (move-point-2 p 5 10) 
    (format t "After: x=~a, y=~a~%" (sb-alien:slot p 'x) (sb-alien:slot p 'y)))

  ;; Test double_array
  (let* ((arr (sb-alien:make-alien sb-alien:int 5)))
    ;; Initialize array
    (dotimes (i 5)
      (setf (sb-alien:deref arr i) (1+ i)))
    (format t "Original array: ~a~%"
            (loop for i from 0 below 5 collect (sb-alien:deref arr i)))

    ;; Initialize array and apply foreign function:
    (let ((result (double-array-2 (sb-alien:alien-sap arr) 5)))
      ;; Access to content of returned table:
      (format t "Doubled array: ~a~%"
              (loop for i from 0 below 5 collect
                   (sb-alien:deref result i)))
      ;; free memory allocated to arrayby double_array
      (free-double-array-2 result))))

;;; end
