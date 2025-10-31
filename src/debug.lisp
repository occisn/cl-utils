(in-package :cl-utils)

(defmacro debug-output (form)
  "Execute FORM and prints both FORM and the result of its evaluation.
(v1, available in occisn/cl-utils GitHub repository)"
  (let ((res (gensym)))
    `(let ((,res (progn ,form)))
       (format t "~:w --> ~s~%" ',form ,res)
       ,res)))

(defun SHOW-debug-output ()
  "Exemple of usage of 'debug-output'."
  (let ((res 0))
    (declare (type fixnum res))
    (loop for i of-type fixnum from 1 to 5
          do (debug-output (incf res i)))
    res))
;; Output:
;; -------
;; (INCF CL-UTILS--DEBUG::RES CL-UTILS--DEBUG::I) --> 1
;; (INCF CL-UTILS--DEBUG::RES CL-UTILS--DEBUG::I) --> 3
;; (INCF CL-UTILS--DEBUG::RES CL-UTILS--DEBUG::I) --> 6
;; (INCF CL-UTILS--DEBUG::RES CL-UTILS--DEBUG::I) --> 10
;; (INCF CL-UTILS--DEBUG::RES CL-UTILS--DEBUG::I) --> 15
;; 15

;;; end
