(in-package :cl-utils)

;; TLDR: ;; 'speed 3' encourages TCO

(defconstant +nb-iterations+ 1000000)
(declaim (type fixnum +nb-iterations+))

;;; ===
;;; === (A) reference implementation, by iteration

(defun compute-pi-leibniz-A-reference ()
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i +nb-iterations+)
      (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
             (denominator (+ 1.0d0 (* 2.0d0 i)))
             (term (/ sign denominator)))
        (declare (type double-float sign denominator term))
        (incf sum term)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "pi = ~a~%" (* 4.0d0 sum)))))
;; --> pi = 3.1415916535897743d0
;; in 0.002 s

;;; ===
;;; === (B) recursion without tail-call 

(defun compute-pi-leibniz-B-recursive-without-TC ()
  (labels ((sub (i)
             (declare (type fixnum i))
             (if (= 0 i)
                 1.0d0
                 (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
                        (denominator (+ 1.0d0 (* 2.0d0 i)))
                        (term (/ sign denominator)))
                   (declare (type double-float sign denominator term))
                   (+ term (the double-float (sub (- i 1)))) ; no tail call
                   )))) 
    (let ((sum (sub +nb-iterations+)))
      (declare (type double-float sum))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "pi = ~a~%" (* 4.0d0 sum))))))

;; DOES NOT WORK:
;; Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.

;;; ===
;;; === (C) recursion with tail-call, non-optimized (speed 0)

(defun compute-pi-leibniz-C-recursive-with-TC-non-optimized ()
  (declare (optimize (debug 3) 
                     (safety 3)
                     (speed 0)))
  (labels ((sub (i acc)
             (declare (type fixnum i)
                      (type double-float acc))
             (if (= 0 i)
                 (+ acc 1)
                 (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
                        (denominator (the double-float (+ 1.0d0 (* 2.0d0 i))))
                        (term (/ sign denominator)))
                   (declare (type double-float sign denominator term))
                   (sub (- i 1) (+ acc term))))))
    (let ((sum (sub +nb-iterations+ 0.0d0)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "pi = ~a~%" (* 4.0d0 sum))))))

;; DOES NOT WORK:
;; Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.
;;
;; tail-call is not optimized due to 'speed 0'
;;
;; we could also have used a (+ 0 (sub...)) to prevent TCO

;;; ===
;;; === (D) recursion with tail-call, optimized (speed 3)

(defun compute-pi-leibniz-D-recursive-with-TC-optimized ()
  (declare (optimize (debug 0) 
                     (safety 0)
                     (speed 3)))
  (labels ((sub (i acc)
             (declare (type fixnum i)
                      (type double-float acc))
             (if (= 0 i)
                 (+ acc 1)
                 (let* ((sign (if (evenp i) 1.0d0 -1.0d0))
                        (denominator (the double-float (+ 1.0d0 (* 2.0d0 i))))
                        (term (/ sign denominator)))
                   (declare (type double-float sign denominator term))
                   (sub (- i 1) (+ acc term))))))
    (let ((sum (sub +nb-iterations+ 0.0d0)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "pi = ~a~%" (* 4.0d0 sum))))))

;; pi = 3.141593653588793d0
;; in 0.001 s

(defun SHOW-all-tco ()
  ""
  (format t "~%~%======~%=== TCO~%======~%")
  (format t "~%")
  (compute-pi-leibniz-D-recursive-with-TC-optimized))

;;; end
