(in-package :cl-utils-tests)

(parachute:define-test date-time-utils-tests
  
  ;; Test universal-time-to-YYYYMMDD-HHMMSS
  (parachute:define-test universal-time-to-yyyymmdd-hhmmss
    ;; Test with universal time 0 (1900-01-01 00:00:00)
    (parachute:is string= "19000101-000000" 
        (universal-time-to-YYYYMMDD-HHMMSS 0))
    
    ;; Test with a specific known time (2022-04-03 14:52:23)
    ;; Using encode-universal-time to create a known timestamp
    (let ((test-time (encode-universal-time 23 52 14 3 4 2022 0)))
      (parachute:is string= "20220403-145223" 
          (universal-time-to-YYYYMMDD-HHMMSS test-time)))
    
    ;; Test with single-digit values (should be zero-padded)
    (let ((test-time (encode-universal-time 5 6 7 8 9 2020 0)))
      (parachute:is string= "20200908-070605" 
          (universal-time-to-YYYYMMDD-HHMMSS test-time))))
  
  ;; Test convert-int-YYYYMMDD-and-HHMMSS-to-universal-time
  (parachute:define-test convert-int-yyyymmdd-and-hhmmss-to-universal-time
    ;; Test basic conversion
    (let ((result (convert-int-YYYYMMDD-and-HHMMSS-to-universal-time 
                   19000101 0 :timezone 0)))
      (parachute:is = 0 result))
    
    ;; Test with specific time (2022-04-03 14:52:23)
    (let ((result (convert-int-YYYYMMDD-and-HHMMSS-to-universal-time 
                   20220403 145223 :timezone 0))
          (expected (encode-universal-time 23 52 14 3 4 2022 0)))
      (parachute:is = expected result))
    
    ;; Test round-trip conversion
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (let* ((original-time (encode-universal-time 30 45 12 15 6 2021 0))
               (formatted (universal-time-to-YYYYMMDD-HHMMSS original-time))
               (yyyymmdd (parse-integer (subseq formatted 0 8)))
               (hhmmss (parse-integer (subseq formatted 9 15)))
               (converted (convert-int-YYYYMMDD-and-HHMMSS-to-universal-time 
                           yyyymmdd hhmmss :timezone 0)))
          (parachute:is = original-time converted))))
  
  ;; Test convert-int-YYYYMMDD-to-universal-time
  (parachute:define-test convert-int-yyyymmdd-to-universal-time
    ;; Test 1900-01-01 (should be 0)
    (parachute:is = 0 (convert-int-YYYYMMDD-to-universal-time 19000101))
    
    ;; Test 1900-01-02 (should be 86400 = 24*3600)
    (parachute:is = 86400 (convert-int-YYYYMMDD-to-universal-time 19000102))
    
    ;; Test that time is set to midnight (00:00:00)
    (let ((result (convert-int-YYYYMMDD-to-universal-time 20220403)))
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time result 0)
        (parachute:is = 0 sec)
        (parachute:is = 0 min)
        (parachute:is = 0 hour)
        (parachute:is = 3 day)
        (parachute:is = 4 month)
        (parachute:is = 2022 year))))
  
  ;; Test universal-time-to-YYYY-MM-DD
  (parachute:define-test universal-time-to-yyyy-mm-dd
    ;; Test with universal time 0
    (parachute:is string= "1900-01-01" (universal-time-to-YYYY-MM-DD 0))
    
    ;; Test with known date
    (let ((test-time (encode-universal-time 0 0 0 25 12 2023 0)))
      (parachute:is string= "2023-12-25" (universal-time-to-YYYY-MM-DD test-time)))
    
    ;; Test zero-padding for single-digit months and days
    (let ((test-time (encode-universal-time 0 0 0 5 3 2020 0)))
      (parachute:is string= "2020-03-05" (universal-time-to-YYYY-MM-DD test-time))))
  
  ;; Test pretty-print-universal-time-as-full-date-time
  (parachute:define-test pretty-print-universal-time-as-full-date-time
    ;; Test format and structure
    (let* ((test-time (encode-universal-time 26 59 15 3 4 2022 -1))
           (result (pretty-print-universal-time-as-full-date-time test-time)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      ;; Check that result contains expected components
      (parachute:true (search "03/04/2022" result))
      (parachute:true (search "15:59:26" result))
      (parachute:true (search "GMT" result)))
    
    ;; Test day of week (1900-01-01 was a Monday)
    (let ((result (pretty-print-universal-time-as-full-date-time 0)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:true (search "Monday" result))))
  
  ;; Test pretty-print-universal-time-as-long-date
  (parachute:define-test pretty-print-universal-time-as-long-date
    ;; Test 1st, 2nd, 3rd suffixes
    (let ((jan-1st (encode-universal-time 0 0 0 1 1 2022 0)))
      (parachute:is string= "January 1st, 2022" 
          (pretty-print-universal-time-as-long-date jan-1st)))
    
    (let ((jan-2nd (encode-universal-time 0 0 0 2 1 2022 0)))
      (parachute:is string= "January 2nd, 2022" 
          (pretty-print-universal-time-as-long-date jan-2nd)))
    
    (let ((jan-3rd (encode-universal-time 0 0 0 3 1 2022 0)))
      (parachute:is string= "January 3rd, 2022" 
          (pretty-print-universal-time-as-long-date jan-3rd)))
    
    ;; Test "th" suffix for other days
    (let ((jan-4th (encode-universal-time 0 0 0 4 1 2022 0)))
      (parachute:is string= "January 4th, 2022" 
          (pretty-print-universal-time-as-long-date jan-4th)))
    
    ;; Test different month
    (let ((dec-25th (encode-universal-time 0 0 0 25 12 2023 0)))
      (parachute:is string= "December 25th, 2023" 
          (pretty-print-universal-time-as-long-date dec-25th))))
  
  ;; Test pretty-print-universal-time-as-short-date
  (parachute:define-test pretty-print-universal-time-as-short-date
    ;; Test abbreviated month names
    (let ((jan-1st (encode-universal-time 0 0 0 1 1 2022 0)))
      (parachute:is string= "Jan. 1st, 2022" 
          (pretty-print-universal-time-as-short-date jan-1st)))
    
    (let ((feb-2nd (encode-universal-time 0 0 0 2 2 2022 0)))
      (parachute:is string= "Feb. 2nd, 2022" 
          (pretty-print-universal-time-as-short-date feb-2nd)))
    
    ;; Test months without abbreviation (May, June, July)
    (let ((may-15th (encode-universal-time 0 0 0 15 5 2022 0)))
      (parachute:is string= "May 15th, 2022" 
          (pretty-print-universal-time-as-short-date may-15th)))
    
    (let ((sept-3rd (encode-universal-time 0 0 0 3 9 2022 0)))
      (parachute:is string= "Sept. 3rd, 2022" 
          (pretty-print-universal-time-as-short-date sept-3rd))))
  
  ;; Test pretty-print-time-difference
  (parachute:define-test pretty-print-time-difference
    ;; Test days (< 31 days)
    (let* ((time1 (encode-universal-time 0 0 0 1 1 2022 0))
           (time2 (encode-universal-time 0 0 0 10 1 2022 0))
           (result (pretty-print-time-difference time1 time2)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:true (search "days" result))
      (parachute:true (search "10" result)))
    
    ;; Test months (31-365 days)
    (let* ((time1 (encode-universal-time 0 0 0 1 1 2022 0))
           (time2 (encode-universal-time 0 0 0 1 3 2022 0))  ; ~60 days
           (result (pretty-print-time-difference time1 time2)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:true (search "months" result)))
    
    ;; Test years (> 365 days)
    (let* ((time1 (encode-universal-time 0 0 0 1 1 2022 0))
           (time2 (encode-universal-time 0 0 0 1 1 2024 0))  ; 2 years
           (result (pretty-print-time-difference time1 time2)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:true (search "years" result))
      (parachute:true (search "2.0" result)))
    
    ;; Test edge case: 1 day
    (let* ((time1 0)
           (time2 0)
           (result (pretty-print-time-difference time1 time2)))
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (parachute:true (search "1 days" result)))))

;;; end
