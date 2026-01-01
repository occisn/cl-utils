(in-package :cl-utils)

(defun trampoline (thunk)
  "Trampoline for infinite recursion, even if no tail-call optimization.

Usage:
(trampoline (fn args))
where FN is the recursive function, which returns :
(i) either the result of the recursion (typically when the termination condition is met)
(ii) either (lambda () (fn ...)) for next recursion step.

The argument THUNK is the initial (fn ...)" 
  (loop while (functionp thunk)
        do (setq thunk (funcall thunk)))
  thunk)


(defun compute-pi-leibniz-A-recursive-with-TC-non-optimized-2 ()
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
    (let ((sum (sub 1000000 0.0d0)))
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

;; with TRAMPOLINE :

(defun compute-pi-leibniz-B-recursive-with-trampoline ()
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
                   (lambda () (sub (- i 1) (+ acc term)))))))
    (let ((sum (trampoline (sub 1000000 0.0d0))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "pi = ~a~%" (* 4.0d0 sum))))))
;; pi = 3.141593653588793d0
;; in 0.022 s
;;
;; to be compared with TCO:
;; pi = 3.141593653588793d0
;; Evaluation took:
;;   0.002 seconds of real time

(defun SHOW-all-trampoline ()
  ""
  (format t "~%~%======~%=== TRAMPOLINE~%======~%")
  (format t "~%")
  (compute-pi-leibniz-B-recursive-with-trampoline))

;;; end
