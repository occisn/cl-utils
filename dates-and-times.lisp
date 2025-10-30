(defpackage cl-utils--dates-and-times
  (:use :cl ))

(in-package :cl-utils--dates-and-times)


;;; Universal time = number of seconds since January 1, 1900
;;;    (get-universal-time) ; --> 3832210384

;;; Unix time = number of seconds since January 1, 1970

;;; "unix-epoch-difference" = beginning of Unix epoch
;;;     = (encode-universal-time 0 0 0 1 1 1970)


;;; ===
;;; =========================================
;;; === Date & time: universal --> string ===
;;; =========================================
;;; ===

(defun universal-time-to-YYYYMMDD-HHMMSS (universal-time)
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

;;; ===
;;; ============================
;;; === Date & time: current ===
;;; ============================
;;; ===

(defun get-current-YYYYMMDD-HHMMSS ()
  "Return current time under YYYYMMDD-HHMMSS format.
For instance: 20220403-145223
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
	               second)))) ; end of labels definition
    
    (universal-time-to-YYYYMMDD-HHMMSS (get-universal-time))))

;;; ===
;;; ===========================
;;; === Date & time: basics ===
;;; ===========================
;;; ===

(defun convert-int-YYYYMMDD-and-HHMMSS-to-universal-time (YYYYMMDD-as-int HHMMSS-as-int &key (timezone 0))
  "Convert YYYYMMDD + HHMMSS to universal time.
For instance: 19000101 100 --> 60
Used to create U~ series
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum YYYYMMDD-as-int HHMMSS-as-int))
  (let* ((tmp (the fixnum (mod YYYYMMDD-as-int 100)))
         (res (encode-universal-time
               (mod HHMMSS-as-int 100) ; second 
               (floor (mod HHMMSS-as-int 10000) 100) ; minute
               (floor HHMMSS-as-int 10000) ; hour 
               tmp ; day
               (the fixnum (floor (mod YYYYMMDD-as-int 10000) 100))   ; month
               (the fixnum (floor YYYYMMDD-as-int 10000))  ; year
               timezone                                  ; time zone
               )))
    (declare (type fixnum tmp)                                
             (type fixnum res))
    res))

(defun convert-int-YYYYMMDD-to-universal-time (YYYYMMDD-as-int)
  "Convert YYYYMMDD to universal time.
For instance: 19000102 --> 24*3600 = 86400
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum YYYYMMDD-as-int))
  (let* ((tmp (the fixnum (mod YYYYMMDD-as-int 100)))
         (res (encode-universal-time
               0 ; second 
               0 ; minute
               0 ; hour 
               tmp ; day
               (the fixnum (floor (mod YYYYMMDD-as-int 10000) 100))   ; month
               (the fixnum (floor YYYYMMDD-as-int 10000))  ; year
               0                                  ; time zone
               )))
    (declare (type fixnum tmp)
             (type fixnum res))
    res))

;;; ===
;;; =========================================
;;; === Date & time: universal --> string ===
;;; =========================================
;;; ===

(defun universal-time-to-YYYY-MM-DD (universal-time)
  "Convert an universal time into YYYY-MM-DD string.
For instance: 0 --> 1900-01-01
(v1 available in occisn/cl-utils GitHub repository)"
  (multiple-value-bind
        (_second _minute _hour day month year _day-of-week _dst-p _tz)
      (decode-universal-time universal-time)
    (declare (ignorable _second _minute _hour _day-of-week _dst-p _tz))
    (format nil "~2,'0d-~2,'0d-~2,'0d"
	    year
            month
            day)))

(defun pretty-print-universal-time-as-full-date-time (universal-time)
  "Return a pretty string corresponding to UNIVERSAL-TIME.
For instance: Sunday 03/04/2022 15:59:26 (GMT+1, DST)
(v1 available in occisn/cl-utils GitHub repository)"
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (declare (type fixnum tz))
    (format nil "~a ~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d:~2,'0d (GMT~@d, ~a)"
	    (nth day-of-week '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
            date month year
            hour minute  second
	    (- tz) (if dst-p "DST" "no DST"))))

(defun SHOW-pretty-print-universal-time-as-full-date-time ()
  (pretty-print-universal-time-as-full-date-time (get-universal-time)))

(defun pretty-print-universal-time-as-long-date (universal-time)
  "Return a pretty string corresponding to UNIVERSAL-TIME.
For instance: --> 'January 1st, 2022'
(v1 available in occisn/cl-utils GitHub repository)"
  (multiple-value-bind
	(second minute hour day-number month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (declare (type fixnum month)
             (ignorable second minute hour day-of-week dst-p tz))
    (format nil "~a ~a, ~a"
            (nth month '("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
            (cond ((= 1 day-number) "1st")
                  ((= 2 day-number) "2nd")
                  ((= 3 day-number) "3rd")
                  (t (format nil "~sth" day-number)))
            year)))

(defun SHOW-pretty-print-universal-time-as-long-date ()
  (pretty-print-universal-time-as-long-date (get-universal-time)))

(defun pretty-print-universal-time-as-short-date (universal-time)
  "Return a pretty string corresponding to UNIVERSAL-TIME.
For instance: --> 'Jan. 1st, 2022'
(v1 available in occisn/cl-utils GitHub repository)"
  (multiple-value-bind
	(second minute hour day-number month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (declare (ignorable second minute hour day-of-week dst-p tz))
    (format nil "~a ~a, ~a"
            (nth month '("" "Jan." "Feb." "Mar." "Apr." "May" "June" "July" "Aug." "Sept." "Oct." "Nov." "Dec."))
            (cond ((= 1 day-number) "1st")
                  ((= 2 day-number) "2nd")
                  ((= 3 day-number) "3rd")
                  (t (format nil "~sth" day-number)))
            year)))

(defun SHOW-pretty-print-universal-time-as-short-date ()
  (pretty-print-universal-time-as-short-date (get-universal-time)))

(defun pretty-print-time-difference (universal-time-1 universal-time-2)
  "Return a pretty string corresponding to the time difference between the two universal times UNIVERSAL-TIME-1 and UNIVERSAL-TIME-2.
For instance: --> '3 days', '3.5 months', '1.3 years'
(v1 available in occisn/cl-utils GitHub repository)"
  (declare (type fixnum universal-time-1 universal-time-2))
  (let ((delta-in-days (+ 1.0 (* (the fixnum (- universal-time-2 universal-time-1)) (/ 1.0 (* 24 3600))))))
    (declare (type single-float delta-in-days))
    (cond ((<= delta-in-days 31.0)
           (format nil "~s days" (the fixnum (round delta-in-days))))
          ((<= delta-in-days 365.0)
           (format nil "~,1f months" (* 12.0 (* delta-in-days (/ 1.0 365.0)))))
          (t (format nil "~,1f years" (* delta-in-days (/ 1.0 365.0)))))))

;;; end
