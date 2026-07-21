;;;; Utilities for images.
;;;;
;;;; Internal-only helpers for PNG generation (uses [zpng library](https://github.com/xach/zpng)).
;;;; Not exported; copy-paste from source if needed.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun draw-pic-from-rgb-arrays (height width r-array g-array b-array export-file)
  "Export picture to file from RGB arrays.

Requires zpng"
  (declare (type fixnum height width)
           (type (simple-array fixnum) r-array g-array b-array))
  (let* ((alpha 255)
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (declare (type fixnum alpha))
    (with-open-file (stream export-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((r (aref r-array (- n 1) (- m 1)))
                (g (aref g-array (- n 1) (- m 1)))
                (b (aref b-array (- n 1) (- m 1))))
            (declare (type fixnum r g b))
            (zpng:write-pixel (list r g b alpha) png))))
      (zpng:finish-png png))))

(defun draw-heatmap-from-values (height width value-array export-file)
  "Create png file with heatmap from value-array.
Algorithm to convert value to HSL then RGB is inspired by:
https://stackoverflow.com/questions/17525215/calculate-color-values-from-green-to-red/30612603#30612603

Requires draw-pic-from-rgb-arrays
Requires zpng"
  (declare (type fixnum height width)
           (type (simple-array double-float) value-array))

  (labels ((hue-to-rgb (p q tt)
             (declare (type double-float p q tt))
             (when (< tt 0.0d0) (setq tt (+ tt 1.0d0)))
             (when (> tt 1.0d0) (setq tt (- tt 1.0d0)))
             (cond ((< tt (/ 1.0d0 6.0d0))
                    (+ p (* (- q p) 6.0d0 tt)))
                   ((< tt 0.5d0) q)
                   ((< tt (/ 2.0d0 3.0d0))
                    (+ p (* (- q p) (- (/ 2.0d0 3.0d0) tt) 6.0d0)))
                   (t p))))

    (declare (ftype (function (double-float double-float double-float) double-float) hue-to-rgb))
    
    (let* ((r-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (g-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (b-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (value-min 0.0d0)
           (value-max 0.0d0)
           (value-range 0.0d0))
      (declare (type (simple-array fixnum) r-array g-array b-array)
               (type double-float value-min value-max value-range))
      
      ;; (1) calculate values min, max and range
      (setq value-min (aref value-array 0 0))
      (setq value-max (aref value-array 0 0))
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((z (aref value-array (- n 1) (- m 1))))
            (declare (type double-float z))
            (when (> z value-max) (setq value-max z))
            (when (< z value-min) (setq value-min z)))))
      (setq value-range (- value-max value-min))

      ;; (2) calculate RGB arrays
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let* ((z (aref value-array (- n 1) (- m 1)))
                 (w (/ (- z value-min) value-range)) ; between 0 and 1
                 (h (* (* w 1.2d0) (/ 1.0d0 3.60d0))) ; hie between 0° and 120°/360°
                 (s 1.0d0)                  ; saturation
                 (l 0.5d0)                  ; lightness
                 (q (- (+ l s) (* l s)))
                 (p (- (* 2 l) q))

                 (r (truncate (* 255.0d0 (hue-to-rgb p q (+ h (/ 1.0d0 3.0d0))))))
                 
                 (g (truncate (* 255.0d0 (hue-to-rgb p q h))))
                 (b (truncate (* 255.0d0 (hue-to-rgb p q (- h (/ 1.0d0 3.0d0)))))))
            (declare (type double-float z w h s l p q)
                     (type fixnum r g b))
            (setf (aref r-array (- n 1) (- m 1)) r)
            (setf (aref g-array (- n 1) (- m 1)) g)
            (setf (aref b-array (- n 1) (- m 1)) b))))

      ;; (3) create heatmap picture
      (draw-pic-from-rgb-arrays height width r-array g-array b-array export-file))))

;;; end
