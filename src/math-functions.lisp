;;;; Utilities for math functions.

(in-package :cl-utils)

;;; ===
;;; === Constants
;;; ===

(declaim (type double-float +epsilon8+))
(defparameter +epsilon8+ 1.0d-8 "epsilon 10^-8")
(declaim (type double-float +epsilon12+))
(defparameter +epsilon12+ 1.0d-12 "epsilon 10^-12")
(declaim (type double-float +epsilon30+))
(defparameter +epsilon30+ 1.0d-30 "epsilon 10^-30")
(declaim (type double-float +epsilon60+))
(defparameter +epsilon60+ 1.0d-60 "epsilon 10^-60")
(declaim (type double-float +epsilon98+))
(defparameter +epsilon98+ 1.0d-98 "epsilon 10^-98")

(declaim (type double-float +1-over-12+))
(defparameter +1-over-12+ (/ 1.0d0 12.0d0))
(declaim (type double-float +1-over-360+))
(defparameter +1-over-360+ (/ 1.0d0 360.0d0))
(declaim (type double-float +1-over-1260+))
(defparameter +1-over-1260+ (/ 1.0d0 1260.0d0))
(declaim (type double-float +1-over-1680+))
(defparameter +1-over-1680+ (/ 1.0d0 1680.0d0))

(declaim (type double-float +ln-2pi-over-2+))
(defparameter +ln-2pi-over-2+ (* .5d0 (log (* 2.0d0 pi))))
(declaim (type double-float +1-over-sqrt-2pi+))
(defparameter +1-over-sqrt-2pi+ (/ 1.0d0 (the double-float (sqrt (the double-float (* 2.0d0 pi))))))


;;; ===
;;; === Legendre polynomials
;;; ===

(declaim (inline legendre1))
(defun legendre1 (n work1)
  "Compute first-order normalized orthogonal coefficients of discrete Legendre polynomial for N data
points into WORK1.

Source: Timothy Masters' LEGENDRE.CPP"
  (declare (type fixnum n)
           (type (simple-array double-float (*)) work1))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work1 i) (- (/ (* 2.0d0 i) (- n 1.0d0)) 1.0d0)
            sum (+ sum (* (aref work1 i) (aref work1 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work1 i) (/ (aref work1 i) sum)))))

(declaim (inline legendre2))
(defun legendre2 (n work1 work2)
  "Compute first- and second-order normalized orthogonal coefficients of discrete Legendre
polynomial for N data points into WORK1 and WORK2.

Source: Timothy Masters' LEGENDRE.CPP"
  (declare (type fixnum n)
           (type (simple-array double-float (*)) work1 work2))

  (legendre1 n work1)
  (let ((sum 0.0d0)
        (mean 0.0d0))
    (declare (type double-float sum mean))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work2 i) (* (aref work1 i) (aref work1 i))
            sum (+ sum (aref work2 i))))
    (setq mean (/ sum n))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work2 i) (- (aref work2 i) mean)
            sum (+ sum (* (aref work2 i) (aref work2 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work2 i) (/ (aref work2 i) sum)))))

(declaim (inline legendre3))
(defun legendre3 (n work1 work2 work3)
  "Compute first-, second- and third-order normalized orthogonal coefficients of discrete Legendre
polynomial for N data points into WORK1, WORK2 and WORK3.

Source: Timothy Masters' LEGENDRE.CPP"
  (declare (type fixnum n)
           (type (simple-array double-float (*)) work1 work2 work3))

  (legendre2 n work1 work2)
  (let ((sum 0.0d0)
        (mean 0.0d0)
        (proj 0.0d0))
    (declare (type double-float sum mean proj))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work3 i) (* (aref work1 i) (aref work1 i) (aref work1 i))
            sum (+ sum (aref work3 i))))
    (setq mean (/ sum n))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work3 i) (- (aref work3 i) mean)
            sum (+ sum (* (aref work3 i) (aref work3 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work3 i) (/ (aref work3 i) sum)))
    ;; Remove the projection of c1
    (setq proj 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (incf proj (* (aref work1 i) (aref work3 i))))
    (setq sum 0.0d0)
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work3 i) (- (aref work3 i) (* proj (aref work1 i)))
            sum (+ sum (* (aref work3 i) (aref work3 i)))))
    (setq sum (sqrt (the (double-float 0.0d0) sum)))
    (loop for i of-type fixnum from 0 below n do
      (setf (aref work3 i) (/ (aref work3 i) sum)))))


;;; ===
;;; === normal-cdf
;;; ===

(declaim (inline %normal-cdf-into))
(defun %normal-cdf-into (res-box z)
  "Calculate normal-cdf(Z) and store the result in RES-BOX.

Source: Timothy Masters
Same as formula 2.6.17 in Abramovitz & Stegun, Handbook of mathematical functions."
  (declare (type double-float z)
           (type type-df-box res-box))
  (let* ((abs-z 0.0d0) (pdf 0.0d0) (t1 0.0d0) (poly 0.0d0) (res 0.0d0) (exp1 0.0d0))
    (declare (type double-float abs-z pdf t1 poly res exp1))
    (setq abs-z (abs z)
          exp1 (exp (the double-float (* -0.5d0 abs-z abs-z)))
          pdf (* exp1 +1-over-sqrt-2pi+)
          t1 (/ 1.0d0 (+ 1.0d0 (* abs-z 0.2316419d0)))
          poly (* (+ (* (- (* (+ (* (- (* 1.3302745d0 t1)
                                       1.8212559d0)
                                    t1)
                                 1.7814779d0)
                              t1)
                           0.35656378d0)
                        t1)
                     0.31938154d0)
                  t1)
          res (if (> z 0.0d0)
                  (- 1.0d0 (* pdf poly))
                  (* pdf poly)))
    (put-into res-box res)
    nil))

(defmacro normal-cdf-m (tmp-box z)
  "Return normal-cdf(Z). TMP-BOX is a work one-cell vector.

Source: Timothy Masters."
  `(progn
     (%normal-cdf-into ,tmp-box ,z)
     (value-of ,tmp-box)))


;;; ===
;;; === lgamma (log-Gamma)
;;; ===

(declaim (inline %lgamma-into))
(defun %lgamma-into (res-box x)
  "Calculate lgamma(X) and store the result in RES-BOX.

Source: Timothy Masters
ln-gamma(x) = x.ln(x) - x - ln(x)/2 + ln(2pi)/2 + 1/(12x) - 1/(360x^3) + 1/(1260x^5) - 1/(1680x^7) +
..."
  (declare (type double-float x)
           (type type-df-box res-box))
  (block outer
    (let ((result 0.0d0) (z 0.0d0) (log1 0.0d0) (res 0.0d0))
      (declare (type double-float result z log1 res))
      (when (<= x 0.0d0)
        (put-into res-box 0.0d0)
        (return-from outer))
      (if (< x 7.0d0)
          (progn
            (setq result 1.0d0)
            (setq z x)
            (while1 (< z 7.0d0)
              (setq result (* result z)
                    x z
                    z (+ z 1.0d0)))
            (setq x (+ x 1.0d0)
                  log1 (log (the (double-float 0.0d0) result))
                  result (- log1)))
          (progn
            (setq result 0.0d0)))
      (setq z (/ 1.0d0 (* x x))
            log1 (log (the (double-float 0.0d0) x))
            res (+ (+ (- (+ result
                            (* (- x 0.5d0) log1))
                         x)
                      +ln-2pi-over-2+)
                   (/ (+ (* (- (* (+ (* (- 0.0d0 +1-over-1680+) z)
                                     +1-over-1260+)
                                  z)
                               +1-over-360+)
                            z)
                         +1-over-12+)
                      x)))
      (put-into res-box res)))
  nil)

(defmacro lgamma-m (tmp-box x)
  "Return lgamma(X). TMP-BOX is a one-cell work vector.

Source: Timothy Masters."
  `(progn
     (%lgamma-into ,tmp-box ,x)
     (value-of ,tmp-box)))


;;; ===
;;; === ibeta (incomplete beta function RATIO)
;;; ===

(declaim (inline %ibeta-into))
(defun %ibeta-into (res-box p q x)
  "Calculate ibeta(P, Q, X) and store the result in RES-BOX.
The calculation corresponds to incomplete beta function RATIO.

Source: Timothy Masters."
  (declare (type double-float p q x)
           (type type-df-box res-box))
  (let* ((switched_args t)
         (ib 0)
         (temp 0.0d0) (ps 0.0d0) (px 0.0d0) (pq 0.0d0) (p1 0.0d0) (d4 0.0d0) (xb 0.0d0) (infsum 0.0d0) (cnt 0.0d0) (wh 0.0d0) (finsum 0.0d0) (prob 0.0d0) (term 0.0d0) (xfac 0.0d0) (log1 0.0d0) (log2 0.0d0) (tmpA 0.0d0)
         (eps +epsilon12+)
         (inv-eps (/ 1.0d0 eps))
         (eps1 +epsilon98+)
         (aleps1 (log (the (double-float 0.0d0) eps1)))
         (inv-aleps1 (/ 1.0d0 aleps1)))
    (declare (type boolean switched_args))
    (declare (type fixnum ib))
    (declare (type double-float log1 log2 temp ps px pq p1 d4 xb infsum cnt wh finsum prob term xfac inv-eps inv-aleps1 tmpA))
    (declare (type double-float eps eps1 aleps1))
    (declare (ignorable pq p1 d4 tmpA inv-aleps1))

    (block outer

      (when (<= x 0.0d0) (return-from outer 0.0d0))
      (when (>= x 1.0d0) (return-from outer 0.0d0))
      (when (or (<= p 0.0d0) (<= q 0.0d0)) (return-from outer -1.0d0))

      (tagbody

         ;; switch the arguments if needed for better convergence
         (if (> x 0.5d0)
             (setq temp p
                   p q
                   q temp
                   x (- 1.0d0 x)
                   switched_args t)
             (setq switched_args nil))

         ;; Define ps as 1 if q is an integer, else q - (int) q
         (setq tmpA (* 1.0d0 (the fixnum (floor-of-positive-df q)))
               ps (- q tmpA))
         (when (< (abs ps) +epsilon60+) (setq ps 1.0d0))

         ;; Compute INFSUM
         (setq log1 (log (the (double-float 0.0d0) x))
               px (* p log1)
               pq (lgamma-m res-box (+ p q))
               p1 (lgamma-m res-box p)
               d4 (log (the (double-float 0.0d0) p))
               term (+ px (- (lgamma-m res-box (+ ps p)) (lgamma-m res-box ps) d4 p1)))

         (if (= 0 (the fixnum (floor-of-positive-df (* term inv-aleps1))))
             (progn
               (setq infsum (exp term)
                     cnt (* infsum p)
                     wh 1.0d0)
               (repeat-until
                 (setq cnt (* cnt (/ (* (- wh ps) x) wh))
                       term (/ cnt (+ p wh))
                       infsum (+ infsum term)
                       wh (+ wh 1.0d0))
                 :until (<= (* term inv-eps) infsum)))
             (progn
               (setq infsum 0.0d0)))

         ;; Compute FINSUM
         (setq finsum 0.0d0)
         (when (<= q 1.0d0) (go FINISH))
         (setq log1 (log (the (double-float 0.0d0) (- 1.0d0 x)))
               log2 (log (the (double-float 0.0d0) q))
               xb (+ px (* q log1) (- pq p1 log2 (lgamma-m res-box q)))
               ib (the fixnum (floor-of-positive-df (* xb inv-aleps1))))
         ;; (when (< ib 0) (setq ib 0)) ; floor-of-positive-df guarantees ib >= 0
         (setq xfac (/ 1.0d0 (- 1.0d0 x))
               term (exp (- xb (* ib aleps1)))
               ps q
               wh (- q 1.0d0))
         (block outer2
           (while1 (> wh 0.0d0)
             (setq px (* ps (/ xfac (+ p wh))))
             (when (and (<= px 1.0d0) (or (<= (* term inv-eps) finsum) (<= term (/ eps1 px)))) (return-from outer2))
             (setq ps wh
                   term (* term px))
             (when (> term 1.0d0)
               (decf ib)
               (setq term (* term eps1)))
             (when (= 0 ib)
               (incf finsum term))
             (setq wh (- wh 1.0d0))))

       FINISH
         (setq prob (+ finsum infsum))
         (if switched_args
             (put-into res-box (- 1.0d0 prob))
             (put-into res-box prob))
         ))
    nil))

(defmacro ibeta-m (tmp-box p q x)
  "Return ibeta(P, Q, X). TMP-BOX is a work one-cell vector.
The calculation corresponds to incomplete beta function RATIO.

Source: Timothy Masters."
  `(progn
     (%ibeta-into ,tmp-box ,p ,q ,x)
     (value-of ,tmp-box)))


;;; ===
;;; === igamma (complement to 1 of incomplete gamma function ratio)
;;; ===

(declaim (inline %igamma-into))
(defun %igamma-into (res-box a x)
  "Calculate igamma(A, X) and store the result in RES-BOX.
This corresponds to the complement to 1 of incomplete gamma function ratio.

Source: Timothy Masters."
  (declare (type double-float a x)
           (type type-df-box res-box))

  (let ((ap 0.0d0) (del 0.0d0) (sum 0.0d0) (b 0.0d0) (c 0.0d0) (d 0.0d0) (h 0.0d0) (an 0.0d0) (FPMIN +epsilon30+) (log1 0.0d0) (tmp1 0.0d0) (res1 0.0d0))
    (declare (type double-float ap del sum b c d h an FPMIN log1 tmp1 res1))
    (block outer

      (when (<= x 0.0d0)
        (setq res1 0.0d0)
        (return-from outer))

      (when (< x (+ a 1.0d0))
        (setq ap a
              del (/ 1.0d0 a)
              sum del)
        (repeat-until
          (setq ap (+ ap 1.0d0)
                del (* del (/ x ap))
                sum (+ sum del))
          :until (< del (* sum +epsilon8+)))
        (setq log1 (log (the (double-float 0.0d0) x))
              tmp1 (lgamma-m res-box a)
              res1 (* sum (exp (- (* a log1) x tmp1))))
        (return-from outer))

      (setq b (- (+ x 1.0d0) a)
            c (/ 1.0d0 FPMIN)
            d (/ 1.0d0 b)
            h d)
      (loop named outer2
            for i of-type fixnum from 1 below 1000 do
              (setq an (* 1.0d0 i (- a i))
                    b (+ b 2.0d0)
                    d (+ (* an d) b))
              (when (< (abs d) FPMIN) (setq d FPMIN))
              (setq c (+ b (/ an c)))
              (when (< (abs c) FPMIN) (setq c FPMIN))
              (setq d (/ 1.0d0 d)
                    del (* d c)
                    h (* h del))
              (when (< (abs (- del 1.0d0)) +epsilon8+)
                (return-from outer2)))
      (setq log1 (log (the (double-float 0.0d0) x))
            tmp1 (lgamma-m res-box a)
            res1 (- 1.0d0 (* h (exp (- (* a log1) x tmp1))))))
    (put-into res-box res1)
    nil))

(defmacro igamma-m (tmp-box a x)
  "Return igamma(A, X). TMP-BOX is a work one-cell vector.
This corresponds to the complement to 1 of incomplete gamma function ratio.

Source: Timothy Masters."
  `(progn
     (%igamma-into ,tmp-box ,a ,x)
     (value-of ,tmp-box)))


;;; ===
;;; === f-cdf (CDF of F-ratio distribution)
;;; ===

(declaim (inline %f-cdf-into))
(defun %f-cdf-into (res-box ndf1 ndf2 f)
  "Calculate f-cdf(NDF1, NDF2, F) and store the result in RES-BOX.
It calculates the CDF of F-ratio distribution.

Source: Timothy Masters."
  (declare (type fixnum ndf1 ndf2)
           (type double-float f)
           (type type-df-box res-box))
  (let ((p 0.0d0) (q 0.0d0) (x 0.0d0) (tmp 0.0d0) (prob1 0.0d0) (denom 0.0d0))
    (declare (type double-float p q x tmp prob1 denom))
    (setq p (* 0.5d0 ndf2)
          q (* 0.5d0 ndf1)
          denom (+ ndf2 (* (* 1.0d0 ndf1) f)))

    (if (> (abs (* 1.0d0 ndf2)) (abs denom))
        (setq tmp 1.0d0)
        (progn
          (setq x (/ ndf2 denom)
                tmp (ibeta-m res-box
                             (the double-float p)
                             (the double-float q)
                             (the double-float x)))))

    (setq prob1 (- 1.0d0 tmp))

    ;; trivial rounding errors can produce out-of-bound results
    (when (< prob1 0.0d0) (setq prob1 0.0d0))
    (when (> prob1 1.0d0) (setq prob1 1.0d0))
    (put-into res-box prob1)
    nil))

(defmacro f-cdf-m (tmp-box ndf1 ndf2 f)
  "Return F-cdf(NDF1, NDF2, F). TMP-BOX is a work one-cell vector.
It calculates the CDF of F-ratio distribution.

Source: Timothy Masters."
  `(progn
     (%f-cdf-into ,tmp-box ,ndf1 ,ndf2 ,f)
     (value-of ,tmp-box)))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-math-functions ()
  "Demonstrate math function utilities."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (format t "~%~%======~%=== MATH-FUNCTIONS~%======~%")
  (format t "~%--- Legendre polynomials ---~%")
  (let ((work1 (make-array 5 :element-type 'double-float)))
    (legendre1 5 work1)
    (format t "Legendre1(5) = ~s~%" work1))
  (format t "~%--- Statistical functions ---~%")
  (let ((tmp-box (make-box 0.0d0)))
    (format t "normal-cdf(0.88) = ~s~%" (normal-cdf-m tmp-box 0.88d0))
    (format t "lgamma(0.65) = ~s~%" (lgamma-m tmp-box 0.65d0))
    (format t "ibeta(0.6, 0.4, 0.8) = ~s~%" (ibeta-m tmp-box 0.6d0 0.4d0 0.8d0))
    (format t "igamma(0.4, 0.6) = ~s~%" (igamma-m tmp-box 0.4d0 0.6d0))
    (format t "f-cdf(6, 4, 2.0) = ~s~%" (f-cdf-m tmp-box 6 4 2.0d0)))
  (format t "~%"))

;;; end
