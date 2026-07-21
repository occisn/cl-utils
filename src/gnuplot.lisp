;;;; Utilities for gnuplot.

(in-package :cl-utils)

;;; ==============
;;; === GNUPLOT ===
;;; ==============

;;; ===
;;; === Gnuplot constants
;;; ===

(defparameter +gnuplot-chart-default-width+ 1100 "Gnuplot chart default width")
(defparameter +gnuplot-chart-default-height+ 550 "Gnuplot chart default height")
(defparameter +gnuplot-default-line-width+ 2 "Gnuplot default line width")
(defparameter +gnuplot-default-general-font+ "Helvetica,10" "Gnuplot default general font")
(defparameter +gnuplot-default-title-size+ 14 "Gnuplot default title size")
(defparameter +gnuplot-default-line-color+ "black" "Gnuplot default line color")
(defparameter +gnuplot-subtitle-font-size-multiplier+ 0.7 "Gnuplot subtitle font size multiplier")
(defparameter +gnuplot-histogram-default-color+ "blue" "Gnuplot default histogram color")
(defparameter +gnuplot-default-x-label-font-size+ 10 "Gnuplot default xlabel font size")
(defparameter +gnuplot-default-y-label-font-size+ 10 "Gnuplot default ylabel font size")
(defparameter +gnuplot-default-bar-chart-box-width+ 0.5 "Gnuplot default bar charts box width")
(defparameter +gnuplot-default-legend-font-size+ 8 "Gnuplot default legend font size")


;;; ===
;;; === Line chart
;;; ===

(defun gnuplot-plot-line-chart (x-vector ys gnuplot-program tmp-directory &key (title nil) (width +gnuplot-chart-default-width+) (height +gnuplot-chart-default-height+) (from 0) (general-font +gnuplot-default-general-font+) (title-font-size +gnuplot-default-title-size+) (x-axis-label nil) (top-y-axis-label nil) (subtitle nil) (top-y-axis-force-zero nil) (below1-y-axis-label nil) (below1-y-axis-force-zero nil) (below2-y-axis-label nil) (below2-y-axis-force-zero nil) (to-file nil) (legend-font-size +gnuplot-default-legend-font-size+))
  "Plot line chart with Gnuplot.

X-VECTOR: vector containing x values (or nil for auto-generated)
YS: list of (position vector &key properties) where position is :top, :below1, or :below2
GNUPLOT-PROGRAM: path to Gnuplot executable
TMP-DIRECTORY: temp directory for gnuplot scripts"
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((nb-data (length (cadr (car ys))))
          (gp-tmp-file (random-file-name tmp-directory "gnuplot-plot-line-chart" "gp"))
          (top-ys)
          (below1-ys)
          (below1-p nil)
          (below2-ys)
          (below2-p nil)
          (multiplot-p nil)
          (nb-subplots 0))

      (when (null x-vector)
        (setq x-vector (new-sequence-fixnum-vector nb-data)))

      (setq top-ys (loop for y in ys when (eq :top (car y)) collect (cdr y)))
      (when (null top-ys) (error "No top y"))
      (loop for y in top-ys for y-vector = (car y) for i from 1 do
        (unless (= nb-data (length y-vector))
          (error "Length of TOP y data n~s (~s) is not the same length as length of x data (~s)" i (length y-vector) nb-data)))

      (setq below1-ys (loop for y in ys when (eq :below1 (car y)) collect (cdr y)))
      (setq below1-p below1-ys)
      (setq below2-ys (loop for y in ys when (eq :below2 (car y)) collect (cdr y)))
      (setq below2-p below2-ys)
      (when (and (not below1-p) below2-p) (error "Below2 without below1"))
      (setq multiplot-p below1-p)
      (setq nb-subplots (cond (below2-p 2) (below1-p 1) (t 0)))

      (with-open-file (stream gp-tmp-file :direction :output :if-exists :supersede)

        ;; Data
        (format stream "$topdata << EOD~%")
        (loop for i from (max 0 from) below nb-data do
          (format stream "~s" (aref x-vector i))
          (loop for y in top-ys for y-vector = (car y) do (format stream " ~f" (aref y-vector i)))
          (format stream "~%"))
        (format stream "EOD~%~%")

        (when below1-p
          (format stream "$below1data << EOD~%")
          (loop for i from (max 0 from) below nb-data do
            (format stream "~s" (aref x-vector i))
            (loop for y in below1-ys for y-vector = (car y) do (format stream " ~f" (aref y-vector i)))
            (format stream "~%"))
          (format stream "EOD~%~%"))

        (when below2-p
          (format stream "$below2data << EOD~%")
          (loop for i from (max 0 from) below nb-data do
            (format stream "~s" (aref x-vector i))
            (loop for y in below2-ys for y-vector = (car y) do (format stream " ~f" (aref y-vector i)))
            (format stream "~%"))
          (format stream "EOD~%~%"))

        ;; Configuration
        (format stream "set term ~a size ~a, ~a font \"~a\"~%" (if to-file "png" "wxt") width height general-font)
        (when to-file (format stream "set output ~s~%" to-file))

        (when (and title subtitle)
          (setq title (format nil "~a\\n{/*~f ~a}" title +gnuplot-subtitle-font-size-multiplier+ subtitle)))
        (when (and (not multiplot-p) title)
          (format stream "set title \"~a\" font \",~s\"~%" title title-font-size))
        (format stream "set key font \",~s\"~%" legend-font-size)

        (when multiplot-p
          (format stream "set bmargin 0~%")
          (format stream "set multiplot layout ~s, 1 spacing 0, 0" (+ nb-subplots 1))
          (when title (format stream " title \"~a\" font \",~s\"" title title-font-size))
          (format stream "~%"))

        (format stream "~%set xrange[*:*] noextend~%")

        ;; TOP plot
        (format stream "~%# TOP data~%")
        (cond (below2-p (format stream "set size 1.0, 0.45~%set origin 0.0, 0.47~%"))
              (below1-p (format stream "set size 1.0, 0.55~%set origin 0.0, 0.35~%")))
        (format stream "set yrange [~a:*] extend~%" (if top-y-axis-force-zero "0" "*"))
        (when below1-p (format stream "unset xtics~%"))
        (when top-y-axis-label (format stream "set ylabel \"~a\"~%" top-y-axis-label))
        (format stream "plot \\~%")
        (loop for y in top-ys for y-prop = (cdr y) for y-nb from 0 with nb-ys = (length top-ys) do
          (format stream "\"$topdata\" using 1:~s" (+ y-nb 2))
          (format stream " with ~a" (getf y-prop :type "line"))
          (when (member :color y-prop) (format stream " lt rgb \"~a\"" (getf y-prop :color)))
          (format stream " lw ~s" (getf y-prop :width +gnuplot-default-line-width+))
          (if (member :legend y-prop) (format stream " title \"~a\"" (getf y-prop :legend)) (format stream " notitle"))
          (when (< y-nb (- nb-ys 1)) (format stream ", \\~%")))
        (format stream "~%unset title~%")

        ;; BELOW1 plot
        (when below1-p
          (format stream "~%# BELOW1 data~%")
          (if below2-p
              (format stream "set size 1.0, 0.2~%set origin 0.0, 0.27~%")
              (format stream "set size 1.0, 0.35~%set origin 0.0, 0.0~%"))
          (unless below2-p (format stream "unset bmargin~%"))
          (format stream "set tmargin 0~%set xtics~%")
          (format stream "set yrange [~a:*] extend~%" (if below1-y-axis-force-zero "0" "*"))
          (when below2-p (format stream "unset xtics~%"))
          (when below1-y-axis-label (format stream "set ylabel \"~a\"~%" below1-y-axis-label))
          (format stream "plot \\~%")
          (loop for y in below1-ys for y-prop = (cdr y) for y-nb from 0 with nb-ys = (length below1-ys) do
            (format stream "\"$below1data\" using 1:~s with ~a lt rgb \"~a\" lw ~s"
                    (+ y-nb 2) (getf y-prop :type "line") (getf y-prop :color +gnuplot-default-line-color+) (getf y-prop :width +gnuplot-default-line-width+))
            (if (member :legend y-prop) (format stream " title \"~a\"" (getf y-prop :legend)) (format stream " notitle"))
            (when (< y-nb (- nb-ys 1)) (format stream ", \\~%")))
          (format stream "~%unset ylabel~%"))

        ;; BELOW2 plot
        (when below2-p
          (format stream "~%# BELOW2 data~%")
          (format stream "set size 1.0, 0.27~%set origin 0.0, 0.0~%unset bmargin~%set tmargin 0~%set xtics~%")
          (format stream "set yrange [~a:*] extend~%" (if below2-y-axis-force-zero "0" "*"))
          (when below2-y-axis-label (format stream "set ylabel \"~a\"~%" below2-y-axis-label))
          (format stream "plot \\~%")
          (loop for y in below2-ys for y-prop = (cdr y) for y-nb from 0 with nb-ys = (length below2-ys) do
            (format stream "\"$below2data\" using 1:~s with ~a lt rgb \"~a\" lw ~s"
                    (+ y-nb 2) (getf y-prop :type "line") (getf y-prop :color +gnuplot-default-line-color+) (getf y-prop :width +gnuplot-default-line-width+))
            (if (member :legend y-prop) (format stream " title \"~a\"" (getf y-prop :legend)) (format stream " notitle"))
            (when (< y-nb (- nb-ys 1)) (format stream ", \\~%")))
          (format stream "~%unset ylabel~%"))

        (when x-axis-label (format stream "~%set xlabel \"~a\"~%" x-axis-label))
        (when below1-p (format stream "~%unset multiplot~%")))

      (sb-ext:run-program gnuplot-program (list "-persist" gp-tmp-file)))))


;;; ===
;;; === Bar chart
;;; ===

(defun gnuplot-chart-one-bar-chart (x-vector y-vector gnuplot-program tmp-directory &key (legend nil) (title nil) (term '("wxt")) (x-label nil) (y-label nil) (width +gnuplot-chart-default-width+) (height +gnuplot-chart-default-height+) (general-font +gnuplot-default-general-font+) (title-font-size +gnuplot-default-title-size+) (y-axis-force-zero t) (color +gnuplot-histogram-default-color+) (x-label-font-size +gnuplot-default-x-label-font-size+) (y-label-font-size +gnuplot-default-y-label-font-size+))
  "Plot a bar chart with Gnuplot.

X-VECTOR: vector containing x values (or nil for auto-generated)
Y-VECTOR: vector containing y values
GNUPLOT-PROGRAM: path to Gnuplot executable
TMP-DIRECTORY: temp directory for gnuplot scripts"
  (declare (type (simple-array) y-vector))
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((nb-data (length y-vector))
          (tmp-file (random-file-name tmp-directory "gnuplot-chart-one-bar-chart" "gp")))
      (when (null x-vector)
        (setq x-vector (new-sequence-fixnum-vector nb-data)))
      (unless (= nb-data (length y-vector))
        (error "Y data has not the same length as X data."))
      (with-open-file (stream tmp-file :direction :output :if-exists :supersede)
        (format stream "$data << EOD~%")
        (loop for i from 0 below nb-data do
          (format stream "~s ~f~%" (aref x-vector i) (aref y-vector i)))
        (format stream "EOD~%~%")
        (format stream "set term ~a size ~a, ~a font \"~a\"~%" (car term) width height general-font)
        (when (string= "png" (car term))
          (format stream "set output ~s~%" (cadr term)))
        (format stream "set boxwidth ~1,1f~%" +gnuplot-default-bar-chart-box-width+)
        (format stream "set xrange[*:*] noextend~%")
        (when y-axis-force-zero (format stream "set yrange [0:*]~%"))
        (format stream "set style fill solid~%")
        (when x-label (format stream "set xlabel \"~a\" font \",~d\"~%" x-label x-label-font-size))
        (when y-label (format stream "set ylabel \"~a\" font \",~d\"~%" y-label y-label-font-size))
        (when title (format stream "set title \"~a\" font \",~s\"~%" title title-font-size))
        (format stream "plot \\~%\"$data\" using 1:2 with boxes")
        (if legend (format stream " title \"~a\"" legend) (format stream " notitle"))
        (format stream " linecolor rgb \"~a\"" color))
      (sb-ext:run-program gnuplot-program (list "-persist" tmp-file)))))


;;; ===
;;; === SHOW ===
;;; ===

(defun SHOW-all-gnuplot ()
  "Demonstrate gnuplot utilities."
  (format t "~%~%======~%=== GNUPLOT~%======~%")
  (format t "~%(gnuplot-plot-line-chart and gnuplot-chart-one-bar-chart available; require Gnuplot installation)~%")
  (format t "~%"))

;;; === end
