(in-package :cl-utils)

(defparameter *leibniz-n* 10000000000) ; 10 zeros
(defparameter *leibniz-nb-cores* 8)
(defparameter *leibniz-nb-chunks* (* 8 3))
(declaim (type fixnum *leibniz-n* *leibniz-nb-cores* *leibniz-nb-chunks*))

;; Calculate an approximation of π using Leibniz formula

;; Each function is tested 5 times, and the fastest duration is kept.

;; |---------------------------------------------|
;; | leibniz-1 | 9.9 s | no parallelism          |
;; | leibniz-2 | 3.1 s | lparallel               |
;; | leibniz-3 | 3.2 s | sb-thread & mutex       | 
;; | leibniz-4 | 3.2 s | bordeaux-thread & mutex | 
;; | leibniz-5 | 2.7 s | sb-thread & queue       |
;; | leibniz-6 | 3.1 s | lparallel & futures     |
;; | leibniz-7 | 3.3 s | static dispatch         |
;; | leibniz-8 | 3.3 s | sb-thread & mailbox     |
;; |---------------------------------------------|

;; leibniz-7 suffers from load imbalance: even though each thread is assigned 3 chunks statically, real-world variations (OS scheduling, CPU differences, cache effects, background tasks) mean threads finish at different times, and your total runtime is bottlenecked by the slowest thread sitting idle.

;; leibniz-5's dynamic queue allows faster threads to immediately grab more work instead of waiting, keeping all cores busy and making total time closer to the average rather than the worst case. The queue synchronization overhead (microseconds) is negligible compared to processing 400+ million operations per chunk (milliseconds), so the load balancing benefit far outweighs the cost.

;; To be tested also: atomic operations via (sb-ext:atomic-incf ...)

;; no parallelism
(defun SHOW-leibniz-1 () 
  ""
  (let ((start-time (get-internal-real-time))
        (n *leibniz-n*)
        (tmp 0.0d0))
    (declare (type fixnum n)
             (type double-float tmp))
    (dotimes (i n)
      (declare (type fixnum i))
      (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
        (incf tmp (* sign (/ 1.0d0 (the fixnum (+ (* 2 i) 1)))))))
    (setq tmp (* 4 tmp))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let* ((end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
        (format t "Leibniz formula: pi = ~,20f in ~f seconds~%" tmp duration)
        duration))))

(defun SHOW-leibniz-1-benchmark-5-times ()
  ""
  (let ((durations '()))
    (dotimes (i 5)
      (let ((duration (SHOW-leibniz-1)))
        (format t "Run ~D: ~A seconds~%" (1+ i) duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations)))
      (format t "~%Quickest time: ~A seconds~%" quickest)
      quickest)))

(defun SHOW-leibniz-2 (&key (print-info nil))
  ""
  (let* ((n *leibniz-n*)
         (nb-cores *leibniz-nb-cores*)
         (nb-chunks *leibniz-nb-chunks*)
         (start-time (get-internal-real-time))
         (chunk-size (ceiling n nb-chunks)))
    (declare (type fixnum n chunk-size)
             (type fixnum nb-cores nb-chunks))

    (setq lparallel:*kernel* (lparallel:make-kernel nb-cores))

    (let ((partial-sums
            (lparallel:pmap
             'list
             (lambda (chunk-idx)
               (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) ; to avoid float to pointer coercion (cost 13) from PARTIAL-SUM to "<return value>"
               (declare (type fixnum chunk-idx))
               (let* ((chunk-start (* chunk-idx chunk-size))
                      (chunk-end  (min (the fixnum (- n 1))
                                       (the fixnum (- (+ chunk-start chunk-size) 1))))
                      (partial-sum 0.0d0))
                 (declare (type fixnum chunk-start chunk-end)
                          (type double-float partial-sum))
                 (when print-info
                   (format t "[thread #~A] computes task #~A [~D .. ~D]~%"
                           (lparallel.kernel:kernel-worker-index)
                           chunk-idx
                           chunk-start chunk-end))
                 (loop for i of-type fixnum from chunk-start to chunk-end do
                   (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0) (+ (* 2.0d0 i) 1.0d0))))
                 partial-sum))
             (loop for i from 0 below nb-chunks collect i))))

      (let ((res (* 4.0d0 (the double-float (reduce #'+ partial-sums))))
            (duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (declare (type double-float res))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Leibniz formula: ~D chunks, pi = ~20,20F (in ~F s)~%"
                  nb-chunks res duration)
          duration)))))

(defun SHOW-leibniz-2-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-2)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

;; work is divided in 24 chunks
;; and SB-THREADS progressively increases task-idx (protected by mutex), and buids tasks around it, modifying the result, also protected by a mutex
(defun SHOW-leibniz-3 (&key (print-info nil))
  ""

  (let* ((n *leibniz-n*)
         (start-time (get-internal-real-time))
         (nb-cores *leibniz-nb-cores*)
         (nb-chunks *leibniz-nb-chunks*)
         (chunk-size (ceiling n nb-chunks))
         (next-chunk 0)
         (next-chunk-lock (sb-thread:make-mutex))
         (sum 0.0d0) 
         (sum-lock (sb-thread:make-mutex))
         (num-threads (min nb-chunks
                           nb-cores)) 
         (threads (make-array num-threads)))
    (declare (type fixnum n nb-chunks chunk-size next-chunk nb-cores num-threads)
             (type double-float sum))

    ;; Thread function
    (flet ((worker (thread-id)
             (loop
               (let ((my-chunk -1))
                 
                 ;; Acquire next chunk index
                 (sb-thread:with-mutex (next-chunk-lock)
                   (if (>= next-chunk nb-chunks)
                       (return) ;; no more chunks
                       (progn
                         (setq my-chunk next-chunk)
                         (incf next-chunk))))

                 ;; Compute local sum
                 (let* ((chunk-start (* my-chunk chunk-size))
                        (chunk-end (min (the fixnum (- n 1))
                                        (the fixnum (- (+ chunk-start chunk-size) 1))))
                        (partial-sum 0.0d0))
                   (declare (type fixnum chunk-start chunk-end)
                            (type double-float partial-sum))
                   (when print-info
                     (format t "[thread ~D] chunk #~A [~D .. ~D]~%"
                             thread-id my-chunk chunk-start chunk-end))
                   (loop for i from chunk-start to chunk-end do
                     (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0)
                                          (+ (* 2.0d0 i) 1.0d0))))
                   ;; Update shared sum
                   (sb-thread:with-mutex (sum-lock)
                     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                     (incf sum partial-sum)))))))

      ;; Spawn threads
      (loop for tid of-type fixnum from 0 below num-threads do
        (let ((tid2 tid)) ;; capture
          (setf (aref threads tid2)
                (sb-thread:make-thread (lambda () (worker tid2))))))

      ;; Wait for threads to finish
      (loop for thread across threads do
        (sb-thread:join-thread thread))

      ;; Compute pi and duration
      (let ((pi1 (* 4.0d0 sum))
            (duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Leibniz formula: ~D chunks, pi = ~20,20F (in ~F s)~%"
                  nb-chunks pi1 duration)
          duration)))))

(defun SHOW-leibniz-3-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-3)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))


;; work is divided in 24 chunks
;; and BORDEAUX-THREADS progressively increases task-idx (protected by mutex), and buids tasks around it, modifying the result, also protected by a mutex
(defun SHOW-leibniz-4 (&key (print-info nil))
  ""

  ;; Shared state
  (let* ((n *leibniz-n*) 
         (nb-chunks *leibniz-nb-chunks*)
         (nb-cores *leibniz-nb-cores*)
         (chunk-size (ceiling n nb-chunks))
         (next-chunk 0)
         (next-chunk-lock (bt:make-lock))
         (shared-sum 0.0d0)
         (shared-sum-lock (bt:make-lock))
         (num-threads (min nb-chunks nb-cores))
         (threads (make-array num-threads)) 
         (start-time (get-internal-real-time)))
    (declare (type fixnum chunk-size next-chunk)
             (type double-float shared-sum))

    ;; Thread function
    (flet ((worker (thread-id)
             (loop
               (let ((my-chunk -1))
                 ;; Acquire next chunk index
                 (bt:with-lock-held (next-chunk-lock)
                   (if (>= next-chunk nb-chunks)
                       (return) ;; no more chunks
                       (progn
                         (setq my-chunk next-chunk)
                         (incf next-chunk))))
                 
                 ;; Compute local sum
                 (let* ((chunk-start (* my-chunk chunk-size))
                        (chunk-end (min (the fixnum (- n 1))
                                        (the fixnum (- (+ chunk-start chunk-size) 1))))
                        (partial-sum 0.0d0))
                   (declare (type fixnum chunk-start chunk-end)
                            (type double-float partial-sum))
                   (when print-info
                     (format t "[thread ~D] chunk #~A [~D .. ~D]~%"
                             thread-id my-chunk chunk-start chunk-end))
                   (loop for i of-type fixnum from chunk-start to chunk-end
                         do
                     (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0)
                                          (+ (* 2.0d0 i) 1.0d0))))
                   ;; Update shared sum
                   (bt:with-lock-held (shared-sum-lock)
                     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                     (incf shared-sum partial-sum)))))))

      ;; Spawn threads
      (loop for tid of-type fixnum from 0 below num-threads do
        (let ((tid2 tid))               ; capture
          (setf (aref threads tid2)
                (bt:make-thread (lambda () (worker tid2))))))

      ;; Wait for threads to finish
      (loop for thread across threads do
        (bt:join-thread thread))

      ;; Compute pi and duration
      (let ((pi1 (* 4.0d0 shared-sum))
            (duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Leibniz formula: ~D chunks, pi = ~20,20F (in ~F s)~%"
                  nb-chunks pi1 duration)
          duration)))))

(defun SHOW-leibniz-4-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-4)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

;; work is divided into 24 parts
;; 8 worker threads are created
;; tasks are progressively enqueued
;; each worker returns its result when it has finished
(defun SHOW-leibniz-5 (&key (print-info nil))
  ""
  
  (let* ((n *leibniz-n*)
         (nb-cores *leibniz-nb-cores*)
         (nb-chunks *leibniz-nb-chunks*)
         (start-time (get-internal-real-time))
         (chunk-size (ceiling n nb-chunks))
         (queue (sb-concurrency:make-queue))
         (workers '()))
    (declare (type fixnum n nb-chunks nb-cores chunk-size))

    (flet ((worker (thread-id)
             (when print-info
               (format t "Thread ~A created.~%" thread-id))
             (let ((thread-sum 0.0d0))
               (declare (type double-float thread-sum))
               (loop
                 (let ((item (sb-concurrency:dequeue queue)))
                   (when (eq item :done)
                     (when print-info
                       (format t "Thread ~A: termination instruction received.~%" thread-id))
                     (return thread-sum))
                   (when print-info
                     (format t "Thread ~A: task ~A received.~%" thread-id item))
                   ;; task:
                   (let ((partial-sum 0.0d0))
                     (declare (type double-float partial-sum))
                     (loop with chunk-idx of-type fixnum = item
                           with chunk-start of-type fixnum = (* chunk-idx chunk-size)
                           with chunk-end of-type fixnum = (min (the fixnum (- n 1))
                                                                (the fixnum (- (+ chunk-start chunk-size) 1)))
                           for i of-type fixnum from chunk-start to chunk-end do
                             (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0) (the double-float (+ (* 2.0d0 i) 1.0d0)))))
                     (incf thread-sum partial-sum))))
               thread-sum)))
      
      ;; Start thread pools
      (dotimes (m nb-cores)
        (declare (type fixnum m))
        (let ((thread-id m))            ; capture
          (declare (type fixnum thread-id))
          (locally
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (push (sb-thread:make-thread (lambda () (worker thread-id))) workers))))

      ;; Enqueue tasks and finish by termination markers
      (dotimes (chunk-idx nb-chunks)
        (declare (type fixnum chunk-idx))
        (sb-concurrency:enqueue chunk-idx queue))
      (dotimes (m nb-cores)
        (declare (type fixnum m))
        (sb-concurrency:enqueue :done queue))

      ;; Wait for completion
      (let ((total-sum 0.0d0))
        (declare (type double-float total-sum))
        (dolist (w workers)
          (incf total-sum (the double-float (sb-thread:join-thread w))))
        (setq total-sum (* 4.0d0 total-sum))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (let ((duration (/ (- (get-internal-real-time) start-time)
                             internal-time-units-per-second)))
            (format t "~%pi = ~a (in ~F s)~%" total-sum duration)
            duration))))))

(defun SHOW-leibniz-5-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-5)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

;; work is divided into 24 parts
;; lparallel creates a kernel of 8 threads
;;     then creates 24 futures, progressively processes threads
(defun SHOW-leibniz-6 (&key (print-info nil))
  ""
  
  (let* ((start-time (get-internal-real-time))
         (nb-cores *leibniz-nb-cores*)
         (n *leibniz-n*)
         (nb-chunks *leibniz-nb-chunks*)
         (chunk-size (ceiling n nb-chunks))
         (futures '())
         (total-sum 0.0d0))
    (declare (type fixnum n nb-chunks chunk-size)
             (type double-float total-sum))

    (setf lparallel:*kernel* (lparallel:make-kernel nb-cores))
    
    ;; task:
    (loop for task-idx from 0 below nb-chunks
          do (let ((chunk-idx task-idx)) ; capture current value
               (push (lparallel:future 
                       (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) ; to avoid float to pointer coercion (cost 13) from PARTIAL-SUM to "<return value>"
                       (let* ((thread sb-thread:*current-thread*)
                              (chunk-start (* chunk-idx chunk-size))
                              (chunk-end (min (the fixnum (- n 1))
                                              (the fixnum (- (+ chunk-start chunk-size) 1))))

                              (partial-sum 0.0d0))
                         (declare (type fixnum chunk-start chunk-end)
                                  (type double-float partial-sum))
                         (when print-info
                           (format t "Thread ~A computing task #~A~%" thread
                                   chunk-idx))
                         (loop for i of-type fixnum from chunk-start to chunk-end do
                           (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0) (+ (* 2.0d0 i) 1.0d0))))
                         partial-sum))
                     
                     futures)
               (when print-info
                 (format t "Future #~A created.~%" chunk-idx))))

    ;; Wait for all tasks to complete
    (setq total-sum
          (reduce #'+
                  (mapcar #'lparallel:force (nreverse futures))))
    
    (lparallel:end-kernel :wait t)

    (setq total-sum (* 4.0d0 total-sum))

    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((duration (/ (- (get-internal-real-time) start-time) (float internal-time-units-per-second))))
        (format t "~%pi = ~a~% (in ~F seconds)" total-sum duration)
        duration))))

(defun SHOW-leibniz-6-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-6)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))


(defun SHOW-leibniz-7 (&key (print-info nil))
  ""
  
  (let* ((n *leibniz-n*)
         (nb-cores *leibniz-nb-cores*)
         (nb-chunks *leibniz-nb-chunks*) 
         (start-time (get-internal-real-time))
         (chunk-size (ceiling n nb-chunks))
         (workers '()))

    (declare (type fixnum nb-chunks nb-cores chunk-size))
    
    ;; Worker function - each thread knows which chunks to process
    (flet ((worker (thread-id)
             (when print-info
               (format t "Thread ~A created.~%" thread-id))
             (let ((thread-sum 0.0d0))
               (declare (type double-float thread-sum))
               
               ;; Process chunks assigned to this thread (round-robin)
               (loop for chunk-idx of-type fixnum from thread-id below nb-chunks by nb-cores do
                 (when print-info
                   (format t "Thread ~A: processing chunk ~A~%" thread-id chunk-idx))
                 
                 (let* ((chunk-start (* chunk-idx chunk-size))
                        (chunk-end (min (the fixnum (1- n))
                                        (the fixnum (1- (+ chunk-start chunk-size))))))
                   (declare (type fixnum chunk-start chunk-end))
                   
                   (loop for i of-type fixnum from chunk-start to chunk-end do
                     (incf thread-sum 
                           (/ (if (oddp i) -1.0d0 1.0d0) 
                              (the double-float (+ (* 2.0d0 i) 1.0d0)))))))
               
               thread-sum)))
      
      ;; Start thread pool
      (dotimes (m nb-cores)
        (let ((thread-id m))
          (locally
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (push (sb-thread:make-thread (lambda () (worker thread-id))) workers))))
      
      ;; Wait for completion
      (let ((total-sum 0.0d0))
        (declare (type double-float total-sum))
        (dolist (w workers)
          (incf total-sum (the double-float (sb-thread:join-thread w))))
        (setq total-sum (* 4.0d0 total-sum))
        (let ((duration (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
          (locally
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (format t "~%pi = ~a (in ~F s)~%" total-sum duration))
          duration)))))

(defun SHOW-leibniz-7-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-7)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

;; mailbox
(defun SHOW-leibniz-8 (&key (print-info nil))
  ""
  
  (let* ((n *leibniz-n*)
         (nb-cores *leibniz-nb-cores*)
         (nb-chunks *leibniz-nb-chunks*)
         (start-time (get-internal-real-time))
         (chunk-size (the fixnum (ceiling n nb-chunks)))
         (mailbox (sb-concurrency:make-mailbox))
         (workers '()))

    (declare (type fixnum n nb-chunks nb-cores))
    
    ;; Worker function
    (flet ((worker (thread-id)
             (when print-info
               (format t "Thread ~A created.~%" thread-id))
             (let ((thread-sum 0.0d0))
               (declare (type double-float thread-sum))
               (loop
                 (let ((item (sb-concurrency:receive-message mailbox)))
                   (when (eq item :done)
                     (when print-info
                       (format t "Thread ~A: done~%" thread-id))
                     (return thread-sum))
                   
                   (when print-info
                     (format t "Thread ~A: chunk ~A~%" thread-id item))
                   
                   (let* ((chunk-idx item)
                          (chunk-start (* chunk-idx chunk-size))
                          (chunk-end (min (the fixnum (1- n))
                                          (the fixnum (1- (the fixnum (+ chunk-start chunk-size)))))))
                     (declare (type fixnum chunk-idx chunk-start chunk-end))
                     
                     (loop for i of-type fixnum from chunk-start to chunk-end do
                       (incf thread-sum 
                             (/ (if (oddp i) -1.0d0 1.0d0) 
                                (the double-float (+ (* 2.0d0 i) 1.0d0)))))))))))
      
      ;; Start threads FIRST
      (dotimes (m nb-cores)
        (let ((thread-id m))
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (push (sb-thread:make-thread (lambda () (worker thread-id))) workers)))
      
      ;; THEN enqueue tasks
      (dotimes (chunk-idx nb-chunks)
        (sb-concurrency:send-message mailbox chunk-idx))
      
      (dotimes (m nb-cores)
        (sb-concurrency:send-message mailbox :done))
      
      ;; Wait for completion
      (let ((total-sum 0.0d0))
        (declare (type double-float total-sum))
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (dolist (w workers)
          (incf total-sum (sb-thread:join-thread w)))
        (setq total-sum (* 4.0d0 total-sum))
        (let ((duration (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
          (format t "~%pi = ~a (in ~F s)~%" total-sum duration)
          duration)))))

(defun SHOW-leibniz-8-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (SHOW-leibniz-8)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

;;; end

