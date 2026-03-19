(in-package :cl-utils)

;;; ===========================
;;; === CONTINUED FRACTIONS ===
;;; ===========================

(defun rational-to-continued-fraction (a b)
  "Return continued fraction corresponding to rational A/B.

For instance:
0 1 --> (0 NIL)
3 7 --> (0 (2 3))
4 1 --> (4 NIL)"
  (declare (type fixnum a b))
  (let ((tmp (with-collector (collect)
               (loop while (not (= b 0))
                     with tt of-type fixnum = -1
                     with q of-type fixnum = -1
                     do (setq tt b
                              q (the fixnum (floor a b))
                              b (mod a b)
                              a tt)
                        (collect q)))))
    (declare (type list tmp))
    (list (car tmp) (cdr tmp))))

(defun integer-sqrt-to-continued-fraction (n)
  "Return continued fraction corresponding to sqrt(N) where N is supposed _not_ to be a perfect square.

For instance:
23 --> (4 (1 3 1 8)) ; = sqrt(23)

Refer to Project Euler 64."
  (declare (type fixnum n))
  (let* ((isqrt-n (isqrt n))
         (a0 isqrt-n)
         (a a0)
         (num2 isqrt-n)
         (denom 1))
    (declare (type fixnum isqrt-n a0 a num2 denom))

    (list a0
          (butlast
           (with-collector (collect)
             (loop with a-initial of-type fixnum = 0
                   with num2-initial of-type fixnum = 0
                   with denom-initial of-type fixnum = 0
                   with tmp of-type fixnum = 0
                   for i of-type fixnum from 1
                   do
                      (unless (= 0 (mod (the fixnum (- n (the fixnum (* num2 num2)))) denom)) (error "Not a perfect division"))
                      (setq tmp (the fixnum (floor (the fixnum (- n (the fixnum (* num2 num2)))) denom))
                            a (the fixnum (floor (the fixnum (+ isqrt-n num2)) tmp))
                            denom tmp
                            num2 (the fixnum (- (the fixnum (* a tmp)) num2)))
                      (when (= i 1)
                        (setq a-initial a
                              num2-initial num2
                              denom-initial denom))
                      (collect a)

                   until (and (> i 1) (= a a-initial) (= num2 num2-initial) (= denom denom-initial))))))))

(defun length-continued-fraction-isqrt (n)
  "Return length of continued fraction corresponding to sqrt(N) where N is supposed _not_ to be a perfect square.

For instance:
23 --> 4 ; (4 (1 3 1 8))

Refer to Project Euler 64."
  (declare (type fixnum n))
  (let ((cycle (cadr (integer-sqrt-to-continued-fraction n))))
    (declare (type list cycle))
    (length cycle)))

(defmacro with-successive-convergents ((i-symbol num-symbol denom-symbol _of sequ) &body body)
  "Execute BODY for successive convergents of continued fraction SEQU.
Within BODY, convergent is accessible as NUM-SYMBOL/DENOM-SYMBOL (not necessarily fixnum, could have greater values) and its rank I-SYMBOL.

Example:
(block outer
  (with-successive-convergents (i num denom :of (integer-sqrt-to-continued-fraction 2))
    (format t \"(~s) ~s / ~s~%%\" i num denom)
    (when (= i 10) (return-from outer))))"

  (declare (ignorable _of))

  (with-gensyms (start cycle1 cycle first-in-cycle j a b c d tmp m x new-x gcd-val)

    `(locally
         (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let ((,i-symbol 0)
             (,num-symbol 0)
             (,denom-symbol 0))

         (declare (ignorable ,i-symbol)
                  (type integer ,i-symbol)
                  (type integer ,num-symbol ,denom-symbol))

         (let* ((,start (car ,sequ))
                (,cycle1 (unliteral--fixnum-list (cadr ,sequ)))
                (,cycle (make-circular-DO-NOT-PRINT--AND-NOT-LITERAL ,cycle1))
                (,first-in-cycle (car ,cycle)))
           (declare (type fixnum ,start ,first-in-cycle)
                    (type list ,cycle1 ,cycle))

           (setq ,i-symbol 1
                 ,num-symbol ,start
                 ,denom-symbol 1)

           (progn ,@body)

           (let* ((,a ,start)
                  (,b 1)
                  (,c 1)
                  (,d 0)
                  (,x ,first-in-cycle)
                  (,m 0)
                  (,gcd-val 0))

             (declare (type integer ,start ,a ,b ,c ,d ,m ,gcd-val))

             (setq ,i-symbol 2
                   ,num-symbol (+ 1 (the fixnum (* ,start ,x)))
                   ,denom-symbol ,x
                   ,gcd-val (gcd ,num-symbol ,denom-symbol)
                   ,num-symbol (/ ,num-symbol ,gcd-val)
                   ,denom-symbol (/ ,denom-symbol ,gcd-val))

             (progn ,@body)

             (loop for ,j of-type fixnum from 3
                   for ,new-x of-type integer in (cdr ,cycle)
                   with ,tmp of-type integer = 0
                   do

                      (setq ,i-symbol ,j
                            ,tmp ,a
                            ,a (+ (* ,a ,x) ,b)
                            ,b ,tmp
                            ,tmp ,c
                            ,c (+ (* ,c ,x) ,d)
                            ,d ,tmp
                            ,m (gcd ,a ,b ,c ,d)
                            ,a (floor ,a ,m)
                            ,b (floor ,b ,m)
                            ,c (floor ,c ,m)
                            ,d (floor ,d ,m)
                            ,num-symbol (+ (* ,a ,new-x) ,b)
                            ,denom-symbol (+ (* ,c ,new-x) ,d)
                            ,gcd-val (gcd ,num-symbol ,denom-symbol)
                            ,num-symbol (/ ,num-symbol ,gcd-val)
                            ,denom-symbol (/ ,denom-symbol ,gcd-val)
                            ,x ,new-x)

                      (progn ,@body))))))))

(defun solve-pell-equation (D)
  "Solve Pell's equation x^2 - D*y^2 = 1 with continued fractions, and return x and y as values.

For instance: 3 --> 2 1 ; since 2^2 - 3*1^2 = 1."
  (declare (type fixnum D))
  (let* ((sequ (integer-sqrt-to-continued-fraction D)))
    (block outer
      (with-successive-convergents (i num denom :of sequ)
        (when (= (* num num)
                 (+ 1 (* D (* denom denom))))
          (return-from outer (values num denom)))))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-continued-fractions ()
  "Demonstrate continued fraction utilities."
  (format t "~%~%======~%=== CONTINUED-FRACTIONS~%======~%")
  (format t "~%--- rational-to-continued-fraction ---~%")
  (format t "3/7 --> ~s~%" (rational-to-continued-fraction 3 7))
  (format t "~%--- integer-sqrt-to-continued-fraction ---~%")
  (format t "sqrt(23) --> ~s~%" (integer-sqrt-to-continued-fraction 23))
  (format t "~%--- solve-pell-equation ---~%")
  (multiple-value-bind (x y) (solve-pell-equation 3)
    (format t "x^2 - 3*y^2 = 1 --> x=~s, y=~s~%" x y))
  (format t "~%"))

;;; === end
