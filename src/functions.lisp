(in-package :cl-utils)

(defun SHOW-functions-general ()
  ""

  ;; LABELS allows recursion but FLET does not

  )

(defun SHOW-function-arguments-passed-by-reference-or-by-value ()
  ""
  
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  ;; Common Lisp uses "pass by sharing" (also called "pass reference by value")
  ;; 
  ;; What this means:
  ;; - The REFERENCE (pointer) to an object is copied when passed
  ;; - You CAN modify the object the reference points to
  ;; - You CANNOT change which object the caller's variable refers to
  ;;
  ;; This is NOT "pass by value" (which would copy the entire object)
  ;; This is NOT "pass by reference" (which would allow rebinding the caller's variable)

  (defun modify-list-1 (lst)
    (setf (car lst) 'changed) ; Modifies the shared object - visible to caller
    (setf lst '(new list))) ; Only rebinds local parameter - NOT visible to caller

  (let ((my-list '(a b c)))
    (modify-list-1 my-list)
    my-list)                            ; => (CHANGED B C)

  (defun modify-list-2 (lst)
    (rplaca lst 'changed) ; Modifies the shared object - visible to caller
    (push 'NEW lst)) ; Rebinds local parameter - NOT visible to caller

  (let ((my-list '(a b c)))
    (modify-list-2 my-list)
    my-list)                            ; => (CHANGED B C)
  )

(defun SHOW-all-functions ()
  ""
  (format t "~%~%======~%=== FUNCTIONS~%======~%")
  (format t "~%")
  (SHOW-functions-general)
  (format t "~%")
  (SHOW-function-arguments-passed-by-reference-or-by-value))

;;; end
