(in-package :cl-utils-tests)

;;; === while ===

(parachute:define-test test-while
  (parachute:is
   =
   6
   (let ((i 0)
         (sum 0))
     (declare (type fixnum sum i))
     (while (<= i 3)
            (incf sum i)
            (incf i))
     sum)))

(parachute:define-test test-while--alternative-1
  (parachute:is
   =
   6
   (let ((i 0)
         (sum 0))
     (declare (type fixnum sum i))
     (cl-utils::while--alternative-1 (<= i 3)
                                     (incf sum i)
                                     (incf i))
     sum)))

(parachute:define-test test-while--alternative-2
  (parachute:is
   =
   6
   (let ((i 0)
         (sum 0))
     (declare (type fixnum sum i))
     (cl-utils::while--alternative-2 (<= i 3)
       (incf sum i)
       (incf i))
     sum)))

;;; === while1 ===

(parachute:define-test test-while1
  (parachute:is
   =
   6
   (let ((i 0)
         (sum 0))
     (declare (type fixnum sum i))
     (while1 (<= i 3)
             (incf sum i)
             (incf i))
     sum)))

;;; === do-while ===

(parachute:define-test test-do-while
  (parachute:is
   =
   4 ; which overshoots
   (let ((a 0))
     (declare (type fixnum a))
     (do-while
         (incf a)
       :while (<= (the fixnum (* a a)) 10))
     a)))

(parachute:define-test test-do-while--alternative
  (parachute:is
   =
   4 ; which overshoots
   (let ((a 0))
     (declare (type fixnum a))
     (cl-utils::do-while--alternative
         (incf a)
       :while (<= (the fixnum (* a a)) 10))
     a)))

;;; === repeat-until ===

(parachute:define-test test-repeat-until
  (parachute:is
   =
   3
   (let ((i 1))
     (declare (type fixnum i))
     (repeat-until
      (incf i)
      :until (>= i 3))
     i))
  (parachute:is
   =
   4                                    ; which overshoots
   (let ((a 0))
     (declare (type fixnum a))
     (repeat-until
      (incf a)
      :until (> (the fixnum (* a a)) 10))
     a)))

(parachute:define-test test-repeat-until--alternative
  (parachute:is
   =
   4 ; which overshoots
   (let ((a 0))
     (declare (type fixnum a))
     (repeat-until
      (incf a)
      :until (> (the fixnum (* a a)) 10))
     a)))

;;; === collecting ===

(parachute:define-test test-collecting
  (parachute:is
   equal
   '(-5 -4 4 5)
   (collecting
    (loop for i of-type fixnum from -5 to 5
          when (> (* i i) 10)
            do (collect1 i)))))

;;; === end
