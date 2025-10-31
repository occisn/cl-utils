(in-package :cl-utils)

(defun random-file-name (dir base suffix)
  "Generate a random file name within DIR directory, with base BASE and suffix SUFFIX, inserting current datetime and random number. Directory is +tmp-directory+.
For instance: c:/ graph1 gp --> c:/graph1-r264491-20220403-145457.gp
In this exemple, 264491 is a random number
(v1 available in occisn/cl-utils GitHub repository)"

  (labels ((universal-time-to-YYYYMMDD-HHMMSS (universal-time)
             "Return universal time under YYYYMMDD-HHMMSS format.
For instance: 20220403-145223
(v1 available in occisn/cl-utils GitHub repository)"
             (multiple-value-bind
                   (second minute hour day month year _day-of-week _dst-p _tz)
                 (decode-universal-time universal-time)
               (declare (ignorable _day-of-week _dst-p _tz))
               (format nil "~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
	               year
                       month
                       day
                       hour
	               minute
	               second)))

           (get-current-YYYYMMDD-HHMMSS ()
  "Return current time under YYYYMMDD-HHMMSS format.
For instance: 20220403-145223
(v1 available in occisn/cl-utils GitHub repository)"
  (universal-time-to-YYYYMMDD-HHMMSS (get-universal-time)))) ; end of labels definition

      (format nil "~a/~a-r~s-~a.~a"
       dir
       base
       (random 1000000)
       (get-current-YYYYMMDD-HHMMSS) suffix)))

(defun reduce-for-each-line-of-file (fn file-name &key (key #'identity))
  "Reduce function FN for each line of file FILE-NAME processed through KEY."
  (declare (type function fn key))
  (with-open-file (stream file-name)
    (let* ((first-line (read-line stream nil))
	   (current-value (funcall key first-line)))
      (loop for line = (read-line stream nil)
	    while line do
	      (setf current-value
		    (funcall fn current-value (funcall key line))))
      current-value)))

;;; end
