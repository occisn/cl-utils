(in-package :cl-utils)

(declaim (ftype (function (list) simple-array) triangle-to-2d-array))
(defun triangle-to-2d-array (triangle)
  "Convert a triangle (represented as list of lists) in 2D array.
For instance: '((1) (2 3) (4 5 6)) --> #2A((1 0 0) (2 3 0) (4 5 6)).
The argument is supposed to be a list.
All numbers are fixnum.
(v1, available in occisn/cl-utils GitHub repository)"
  
  (declare (type list triangle))

  (let* ((last-line (car (last triangle)))
         (width (length last-line)))
    
    (declare (type list last-line) 
             (type fixnum width))
    
    (labels ((fill-with-zeros (initial-list)
               (declare (type list initial-list))
	       (append initial-list
		       (loop for i from (length initial-list) below width collect 0))))

      (make-array (list (length triangle) width)
                  :element-type 'fixnum
		  :initial-contents (mapcar #'fill-with-zeros triangle)))))

;;; end
