(in-package :cl-utils)

;;; =====================
;;; === CONSOLE CHARTS ===
;;; =====================

(declaim (type fixnum +default-console-width+))
(defparameter +default-console-width+ 94)

(declaim (type fixnum +default-console-height+))
(defparameter +default-console-height+ 35)

;;; ===
;;; === Internal: sort vector with slave ===
;;; ===

(defun %sort-vec-with-slave (master slave)
  "Sort MASTER vector in ascending order, rearranging SLAVE vector accordingly.
Both vectors are modified in place."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((n (length master))
           (indices (make-array n :element-type 'fixnum)))
      (declare (type fixnum n))
      (loop for i of-type fixnum from 0 below n do (setf (aref indices i) i))
      (sort indices (lambda (a b)
                      (< (elt master a) (elt master b))))
      (let ((new-master (map 'vector (lambda (i) (elt master i)) indices))
            (new-slave (map 'vector (lambda (i) (elt slave i)) indices)))
        (loop for i of-type fixnum from 0 below n do
          (setf (elt master i) (elt new-master i)
                (elt slave i) (elt new-slave i)))))))

;;; ===
;;; ==============================
;;; === (HORIZONTAL) BAR CHART ===
;;; ==============================

(defun %console-quick-bar-chart-from-sorted-vectors (xs ys &key (x-format "~a") (y-format "~a") (width-in-char +default-console-width+) (force-zero nil))
  "Draw a bar chart in the REPL showing XS and YS vectors, already sorted."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((max-x-label-length
             (loop for x across xs
                   maximize (length (format nil x-format x))))

           (max-y-label-length
             (loop for y across ys
                   maximize (length (format nil y-format y))))

           (n (let ((n-tmp (- width-in-char
                              max-x-label-length
                              3       ; " | " on the left
                              1       ; space on the right
                              max-y-label-length)))
                (declare (type fixnum n-tmp))
                (unless (>= n-tmp 2) (error "Not enough space (~s)" n-tmp))
                n-tmp))

           (y-min (let ((y-min-tmp
                          (loop for y across ys minimize y)))
                    (if force-zero (min 0 y-min-tmp) y-min-tmp)))

           (y-max (let ((y-max-tmp
                          (loop for y across ys maximize y)))
                    (if force-zero (max 0 y-max-tmp) y-max-tmp)))

           (delta (* (- y-max y-min) (/ 1.0d0 (- n 1))))
           (y1 (- y-min (* 0.5d0 delta))))

      (declare (type fixnum n)
               (type double-float delta y1))

      (loop for x across xs
            for y across ys
            for x-label1 = (format nil x-format x)
            for x-label2 = (string-add-space-at-left x-label1 :total-length max-x-label-length)
            for y-label1 = (format nil y-format y)
            for y-label2 = (string-add-space-at-left y-label1 :total-length max-y-label-length)
            for m of-type fixnum = (if (< (abs delta) 1.0d-30)
                                       (floor n 2)
                                       (+ 1 (floor (/ (- y y1) delta))))
            do
               (format t "~a | " x-label2)
               (format t (string-repeat-string m "*"))
               (format t (string-repeat-string (- n m) " "))
               (format t " ~a~%" y-label2)))))

(defun console-quick-bar-chart-from-lists (xs ys &key (sort-by 'none) (x-format "~a") (y-format "~a") (width-in-char +default-console-width+) (force-zero nil))
  "Draw a horizontal bar chart in the REPL showing XS and YS lists.
SORT-BY: 'none, 'x-asc, or 'y-desc (default: 'none)."
  (declare (type fixnum width-in-char)
           (type list xs ys))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (unless (= (length xs) (length ys)) (error "Length of x data (~s) is different from length of y data (~s)" (length xs) (length ys)))
    (let ((xs (make-array (length xs) :initial-contents xs))
          (ys (make-array (length ys) :initial-contents ys)))
      (cond
        ((eql sort-by 'none) (values))
        ((eql sort-by 'x-asc) (%sort-vec-with-slave xs ys))
        ((eql sort-by 'y-desc) (progn (%sort-vec-with-slave ys xs)
                                      (setq xs (nreverse xs))
                                      (setq ys (nreverse ys))))
        (t (error "Sorting order not recognized: ~s" sort-by)))
      (%console-quick-bar-chart-from-sorted-vectors xs ys :x-format x-format :y-format y-format :width-in-char width-in-char :force-zero force-zero))))

(defun console-quick-bar-chart-from-vectors (xs ys &key (sort-by 'none) (x-format "~a") (y-format "~a") (width-in-char +default-console-width+) (force-zero nil))
  "Draw a horizontal bar chart in the REPL showing XS and YS vectors.
SORT-BY: 'none, 'x-asc, or 'y-desc (default: 'none)."
  (declare (type fixnum width-in-char))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (unless (= (length xs) (length ys)) (error "Length of x data (~s) is different from length of y data (~s)" (length xs) (length ys)))
    (let ((xs (copy-seq xs))
          (ys (copy-seq ys)))
      (cond
        ((eql sort-by 'none) (values))
        ((eql sort-by 'x-asc) (%sort-vec-with-slave xs ys))
        ((eql sort-by 'y-desc) (progn (%sort-vec-with-slave ys xs)
                                      (setq xs (nreverse xs))
                                      (setq ys (nreverse ys))))
        (t (error "Sorting order not recognized: ~s" sort-by)))
      (%console-quick-bar-chart-from-sorted-vectors xs ys :x-format x-format :y-format y-format :width-in-char width-in-char :force-zero force-zero))))

;;; ===
;;; ==================
;;; === LINE CHART ===
;;; ==================

(defun console-quick-line-chart (xs ys-or-list-of-ys &key (x-format "~a") (y-format "~a") (width-in-char +default-console-width+) (height-in-char +default-console-height+) (force-zero nil) (chars (list #\* #\& #\+)) (legends nil))
  "Draw a line chart in the REPL. XS and YS are either lists or vectors.
YS-OR-LIST-OF-YS can be a single data series or a list of data series for multi-series charts.
CHARS: characters for drawing each series (default: * & +).
LEGENDS: list of legend labels."
  (declare (type fixnum width-in-char height-in-char))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((len (length xs))
           (list-of-ys (if (atom (elt ys-or-list-of-ys 0))
                           (list ys-or-list-of-ys)
                           ys-or-list-of-ys))
           (nb-ys (length list-of-ys)))
      (declare (type list chars list-of-ys)
               (type fixnum len))

      (loop for ys in list-of-ys
            for idx from 1
            when (not (= len (length ys)))
              do (error "Length of xs (~s) is different from length of ys #~s (~s)" len idx (length ys)))
      (when (not (null legends))
        (unless (= nb-ys (length legends))
          (error "The number of legends (~s) is different from the number of ys (~s)" (length legends) nb-ys)))

      (let* ((H (let ((H-tmp (- height-in-char 2)))
                  (declare (type fixnum H-tmp))
                  (unless (>= H-tmp 2) (error "Not enough height (~s)" H-tmp))
                  H-tmp))

             (y-min (let ((y-min-tmp
                            (loop for ys in list-of-ys
                                  minimize (if (consp ys)
                                               (apply #'min ys)
                                               (loop for y across ys minimize y)))))
                      (if force-zero (min 0 y-min-tmp) y-min-tmp)))

             (y-max (let ((y-max-tmp
                            (loop for ys in list-of-ys
                                  maximize (if (consp ys)
                                               (apply #'max ys)
                                               (loop for y across ys maximize y)))))
                      (if force-zero (max 0 y-max-tmp) y-max-tmp)))

             (delta-y (* (- y-max y-min) (/ 1.0d0 (- H 1))))
             (y1 (- y-min (* 0.5d0 delta-y)))

             (max-y-label-length
               (max (length (format nil y-format y-max))
                    (loop for j from 1 below H
                          for y = y-min then (+ y delta-y)
                          maximize (length (format nil y-format y)))))

             (L (let ((L-tmp (- width-in-char max-y-label-length 2)))
                  (declare (type fixnum L-tmp))
                  (unless (>= L-tmp 2) (error "Not enough width (~s)" L-tmp))
                  L-tmp))

             (x-min (if (consp xs)
                        (apply #'min xs)
                        (loop for x across xs minimize x)))

             (x-max (if (consp xs)
                        (apply #'max xs)
                        (loop for x across xs maximize x)))

             (delta-x (* (- x-max x-min) (/ 1.0d0 (- L 1))))
             (x1 (- x-min (* 0.5d0 delta-x)))

             (arr (make-array (list L H) :element-type 'character :initial-element #\Space)))

        (declare (type fixnum L H)
                 (type double-float delta-x x1 delta-y y1))

        (loop for idx from 0 below len
              for x = (elt xs idx)
              for m of-type fixnum = (floor (/ (- x x1) delta-x))
              do (loop for idy below nb-ys
                       for ys = (elt list-of-ys idy)
                       for y = (elt ys idx)
                       for p of-type fixnum = (floor (/ (- y y1) delta-y))
                       do (setf (aref arr m p) (elt chars idy))))

        (format t "~%")
        (loop for j from (- H 1) downto 0
              for y-label1 = (format nil y-format (+ y-min (* delta-y j)))
              for y-label = (string-add-space-at-left y-label1 :total-length max-y-label-length)
              do
                 (format t y-label)
                 (format t " |")
                 (loop for i from 0 below L
                       do (format t "~a" (aref arr i j)))
                 (format t "~%"))
        (format t (string-add-space-at-left "" :total-length max-y-label-length))
        (format t "  ")
        (format t (string-repeat-string L "-"))
        (format t "~%")
        (let ((xmin-label (format nil x-format x-min))
              (xmax-label (format nil x-format x-max)))
          (when (< (+ (length xmin-label) (length xmax-label)) L)
            (format t (string-add-space-at-left "" :total-length max-y-label-length))
            (format t "  ")
            (format t x-format x-min)
            (format t (string-add-space-at-left "" :total-length (- L (length xmin-label) (length xmax-label))))
            (format t x-format x-max)
            (format t "~%")))

        (unless (null legends)
          (loop for char in chars
                for legend in legends
                do (format t "   ~c~c~c ~a~%" char char char legend)))))))

;;; ===
;;; ====================
;;; === SCATTER PLOT ===
;;; ====================

(defun console-quick-scatter-plot-xs-ys (xs ys &key (x-format "~a") (y-format "~a") (width-in-char +default-console-width+) (height-in-char +default-console-height+) (force-zero nil) (point-char #\*))
  "Draw a scatter plot in the REPL. XS and YS are lists.
Density is shown by numbers (2-9) and + for > 9."
  (declare (type fixnum width-in-char height-in-char)
           (type list xs ys))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (unless (= (length xs) (length ys)) (error "Length of x data (~s) is different from length of y data (~s)" (length xs) (length ys)))

    (let* ((H (let ((H-tmp (- height-in-char 2)))
                (declare (type fixnum H-tmp))
                (unless (>= H-tmp 2) (error "Not enough height (~s)" H-tmp))
                H-tmp))

           (y-min (let ((y-min-tmp (loop for y in ys minimize y)))
                    (if force-zero (min 0 y-min-tmp) y-min-tmp)))

           (y-max (let ((y-max-tmp (loop for y in ys maximize y)))
                    (if force-zero (max 0 y-max-tmp) y-max-tmp)))

           (delta-y (* (- y-max y-min) (/ 1.0d0 (- H 1))))
           (y1 (- y-min (* 0.5d0 delta-y)))

           (max-y-label-length
             (max (length (format nil y-format y-max))
                  (loop for j from 1 below H
                        for y = y-min then (+ y delta-y)
                        maximize (length (format nil y-format y)))))

           (L (let ((L-tmp (- width-in-char max-y-label-length 2)))
                (declare (type fixnum L-tmp))
                (unless (>= L-tmp 2) (error "Not enough width (~s)" L-tmp))
                L-tmp))

           (x-min (loop for x in xs minimize x))
           (x-max (loop for x in xs maximize x))

           (delta-x (* (- x-max x-min) (/ 1.0d0 (- L 1))))
           (x1 (- x-min (* 0.5d0 delta-x)))

           (arr (make-array (list L H) :element-type 'fixnum :initial-element 0)))

      (declare (type fixnum L H)
               (type double-float delta-x x1 delta-y y1))

      (loop for x in xs
            for y in ys
            for m of-type fixnum = (floor (/ (- x x1) delta-x))
            for p of-type fixnum = (floor (/ (- y y1) delta-y))
            do (incf (aref arr m p)))

      (format t "~%")
      (loop for j from (- H 1) downto 0
            for y-label1 = (format nil y-format (+ y-min (* delta-y j)))
            for y-label = (string-add-space-at-left y-label1 :total-length max-y-label-length)
            do
               (format t y-label)
               (format t " |")
               (loop for i from 0 below L
                     for nb of-type fixnum = (aref arr i j)
                     do (format t "~a"
                                (cond ((> nb 9) #\+)
                                      ((> nb 1) nb)
                                      ((= nb 1) point-char)
                                      ((= nb 0) #\Space)
                                      (t (error "Should not happen: nb = ~s" nb)))))
               (format t "~%"))
      (format t (string-add-space-at-left "" :total-length max-y-label-length))
      (format t "  ")
      (format t (string-repeat-string L "-"))
      (format t "~%")
      (let ((xmin-label (format nil x-format x-min))
            (xmax-label (format nil x-format x-max)))
        (when (< (+ (length xmin-label) (length xmax-label)) L)
          (format t (string-add-space-at-left "" :total-length max-y-label-length))
          (format t "  ")
          (format t x-format x-min)
          (format t (string-add-space-at-left "" :total-length (- L (length xmin-label) (length xmax-label))))
          (format t x-format x-max)
          (format t "~%"))))))

;;; ===
;;; =================
;;; === HISTOGRAM ===
;;; =================

(defun console-quick-histogram (xs &key (nb-bars 20) (verbose nil) (width-in-char +default-console-width+) (height-in-char +default-console-height+) (print-values t) (x-format "~a"))
  "Draw a histogram in the REPL showing XS which is a list of values.
NB-BARS: number of bars (default: 20).
VERBOSE: print details (default: nil).
PRINT-VALUES: print counts on top of bars (default: t)."
  (declare (type fixnum nb-bars width-in-char height-in-char)
           (type list xs))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((x-min (loop for x in xs minimize x))
           (x-max (loop for x in xs maximize x))
           (delta-x (* (- x-max x-min) (/ 1.0d0 nb-bars)))

           (skipped-on-the-left 0)
           (skipped-on-the-right 0)

           (nbs (let ((tmp (make-array nb-bars :element-type 'fixnum :initial-element 0)))
                  (loop for x in xs
                        do (cond ((< x x-min) (incf skipped-on-the-left))
                                 ((> x x-max) (incf skipped-on-the-right))
                                 ((= x x-max) (incf (aref tmp (- nb-bars 1))))
                                 (t (incf (aref tmp (the fixnum (floor (- x x-min) delta-x)))))))
                  tmp))

           (nb-max (loop for nb across nbs maximize nb))

           (H (let ((H-tmp (- height-in-char 1 2)))
                (declare (type fixnum H-tmp))
                (unless (>= H-tmp 2) (error "Not enough height (~s)" H-tmp))
                H-tmp))

           (L (let ((L-tmp (- width-in-char 1)))
                (declare (type fixnum L-tmp))
                (unless (>= L-tmp 2) (error "Not enough width (~s)" L-tmp))
                L-tmp))

           (width-of-a-bar (let ((tmp (the fixnum (floor L nb-bars))))
                             (declare (type fixnum tmp))
                             (when (< tmp 1) (error "Not enough space (~s) for all bars (~s)" L nb-bars))
                             tmp))

           (width-of-all-bars (* nb-bars width-of-a-bar))
           (nb-empty-cols-on-left (the fixnum (floor (- L width-of-all-bars) 2)))
           (nb-empty-cols-on-right (- L nb-empty-cols-on-left width-of-all-bars)))

      (declare (type fixnum skipped-on-the-left skipped-on-the-right nb-max H L
                     width-of-a-bar width-of-all-bars nb-empty-cols-on-left
                     nb-empty-cols-on-right)
               (type (simple-array fixnum) nbs))

      (when verbose
        (format t "Nb of data = ~s; nb of bars = ~s~%" (length xs) nb-bars)
        (format t "x-min = ~s; x-max = ~s; delta-x = ~s~%" x-min x-max delta-x)
        (format t "Nb of data point(s) skipped: ~s on the left and ~s on the right~%" skipped-on-the-left skipped-on-the-right)
        (loop for i from 0 below nb-bars
              for x0 = x-min then (+ x0 delta-x)
              for x1 = (+ x0 delta-x)
              do (format t "Bar #~s begins at ~s, ends at ~s, and contains ~s data point(s)~%" i x0 x1 (aref nbs i))))

      (format t "~%")
      (loop for j of-type fixnum from H downto 0
            do
               (format t "|")
               (format t (string-repeat-string nb-empty-cols-on-left " "))
               (loop for i of-type fixnum from 0 below nb-bars
                     for nb of-type fixnum = (aref nbs i)
                     for nb-as-string = (format nil "~a" nb)
                     for y-in-chars of-type fixnum = (cond ((= nb nb-max) (- H 1))
                                                           ((= nb 0) -1)
                                                           (t (floor (* nb (- H 1)) nb-max)))
                     do
                        (cond ((and print-values (= j (+ y-in-chars 1)))
                               (if (<= (length nb-as-string) width-of-a-bar)
                                   (format t (string-add-space-at-left nb-as-string :total-length width-of-a-bar))
                                   (format t (string-repeat-string width-of-a-bar "+"))))
                              ((<= j y-in-chars) (format t (string-repeat-string width-of-a-bar "*")))
                              (t (format t (string-repeat-string width-of-a-bar " ")))))
               (format t (string-repeat-string nb-empty-cols-on-right " "))
               (format t "~%"))
      (format t "|")
      (format t (string-repeat-string L "-"))
      (format t "~%")
      (let ((xmin-label (format nil x-format x-min))
            (xmax-label (format nil x-format x-max)))
        (when (< (+ (length xmin-label) (length xmax-label)) L)
          (format t " ")
          (format t (string-repeat-string nb-empty-cols-on-left " "))
          (format t xmin-label)
          (format t (string-add-space-at-left "" :total-length (- L nb-empty-cols-on-left (length xmin-label) (length xmax-label) nb-empty-cols-on-right)))
          (format t xmax-label)
          (format t (string-repeat-string nb-empty-cols-on-right " "))
          (format t "~%"))))))

;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-console-quick-bar-chart ()
  "Example of console-quick-bar-chart-from-lists."
  (console-quick-bar-chart-from-lists
   '(1 2 30 40 150)
   '(75.1 96.567 151.1 80 88.12)
   :y-format "~,2f"))

(defun SHOW-console-quick-line-chart ()
  "Example of console-quick-line-chart."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((xs (loop for i from -15 to 15 collect i))
           (ys (loop for x in xs collect (exp (- (* 0.05d0 x x))))))
      (declare (type list xs ys))
      (console-quick-line-chart xs ys :y-format "~,3f"))))

(defun SHOW-console-quick-scatter-plot ()
  "Example of console-quick-scatter-plot-xs-ys."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((xs (new-random-fixnum-list 600 :mini -40 :maxi 41))
           (ys (loop for x in xs collect (+ 1.0d0 (cos (/ (* 6.28d0 x) 30.0d0))))))
      (declare (type list xs ys))
      (console-quick-scatter-plot-xs-ys xs ys :y-format "~,3f"))))

(defun SHOW-console-quick-histogram ()
  "Example of console-quick-histogram."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let* ((xs (new-random-fixnum-list 600 :mini -40 :maxi 41)))
      (declare (type list xs))
      (console-quick-histogram xs :nb-bars 20 :verbose nil))))

(defun SHOW-all-console-charts ()
  ""
  (format t "~%~%======~%=== CONSOLE-CHARTS~%======~%")
  (format t "~%--- bar chart ---~%")
  (SHOW-console-quick-bar-chart)
  (format t "~%--- line chart ---~%")
  (SHOW-console-quick-line-chart)
  (format t "~%--- scatter plot ---~%")
  (SHOW-console-quick-scatter-plot)
  (format t "~%--- histogram ---~%")
  (SHOW-console-quick-histogram))

;;; end
