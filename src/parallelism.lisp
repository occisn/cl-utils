;;;; Utilities for parallelism.

(in-package :cl-utils)

;;; ===
;;; === Parallelism utilities
;;; ===

(defparameter *nb-cores* -1 "Number of cores on the computer on which the code is executed (vs compiled)")
(declaim (type fixnum *nb-cores*))

(defun nb-cores ()
  "Return the number of cores and update *nb-cores*."
  (if (> *nb-cores* 0)
      *nb-cores*
      (progn
        (setq *nb-cores* (cpus:get-number-of-processors))
        *nb-cores*)))

(defmacro with-parallelism (&body body)
  "Create lparallel kernel, execute BODY and end kernel.
The number of cores (nb-cores) that is to say the number of cores of the machine on which code is
executed (vs compiled)."
  (with-gensyms (nb-cores res)
    `(progn
       (if lparallel:*kernel*
           (format t "Strange... kernel already existing.~%")
           (let ((,nb-cores (nb-cores)))
             (format t "Creating kernel with ~s cores..." ,nb-cores)
             (setf lparallel:*kernel* (lparallel:make-kernel ,nb-cores :name "custom-kernel"))
             (format t " done.~%")))
       (let ((,res (progn ,@body)))
         (lparallel:end-kernel :wait t)
         (format t "Kernel stopped.~%")
         ,res))))

(defun show-kernel-info ()
  "Print kernel info."
  (if (null lparallel:*kernel*)
      (format t "No kernel available.")
      (let ((name (lparallel:kernel-name))
            (count (lparallel:kernel-worker-count))
            (context (lparallel:kernel-context))
            (bindings (lparallel:kernel-bindings)))
        (format t "Kernel name = ~a~%" name)
        (format t "Worker threads count = ~d~%" count)
        (format t "Kernel context = ~a~%" context)
        (format t "Kernel bindings = ~a~%" bindings))))

;;; ===
;;; === ploop macros
;;; ===

(defmacro ploop--based-on-pmap (&key generator thread-fn aggregate-fn)
  "Execute GENERATOR, which is typically a loop, and which shall contain a (submit ...) sexp.
For each 'submitted' value(s), THREAD-FN is called on a separate thread.
Then AGGREGATE-FN is called on the successive return values.

Under the hood, a list of input values is created and lparallel:pmap is applied to this list.

Example: see below."
  (with-gensyms (xs ys y)
    `(let* ((,xs (with-collector (collect)
                  ,(sexp-replace-sexp-beginning-by
                    generator
                    'submit
                    (lambda (submit-sexp) `(collect ,(cadr submit-sexp))))))
            (,ys (with-parallelism
                     (lparallel:pmap 'list ,thread-fn ,xs))))
       (loop for ,y in ,ys do (funcall ,aggregate-fn ,y)))))

(defun SHOW-ploop--based-on-pmap (&optional (n 40))
  "Example of usage of ploop--based-on-pmap."
  (let ((sum 0))
    (declare (type fixnum sum))
    (ploop--based-on-pmap
     :generator (loop for i of-type fixnum from 1 to n do (submit i))
     :thread-fn (lambda (n1)
                  (declare (type fixnum n1))
                  (%long-function-A 3000)
                  (the fixnum (* n1 10)))
     :aggregate-fn (lambda (m) (declare (type fixnum m)) (incf sum m)))
    sum))

(defmacro ploop--throwable-threads (&key generator thread-fn aggregate-fn (nb-threads nil) (verbose nil))
  "Execute GENERATOR, which is typically a loop, and which shall contain a (submit ...) sexp.
For each 'submitted' value(s), THREAD-FN is called on a separate thread.
Then AGGREGATE-FN is called on the successive return values.

Under the hood, a new thread is launched for each input value.

If NB-THREADS is null, the number of cores is used.
VERBOSE prints some information.

Example: see below."
  (with-gensyms (nb-tasks-sent nb-results-received channel1 result1 _m nb-threads3 real-base thread-fn2 task-input format-lock)
    `(with-parallelism
         (let* ((,nb-threads3 (if (null ,nb-threads) (nb-cores) ,nb-threads))
                (,nb-tasks-sent 0)
                (,nb-results-received 0)
                (,channel1 (lparallel:make-channel))
                (,format-lock (bt:make-lock))
                (,thread-fn2
                  ,(if verbose
                      `(lambda (,task-input)
                        (let ((,real-base 0))
                          (declare (type fixnum ,real-base))
                          (setq ,real-base (get-internal-real-time))
                          (let ((,result1
                                  (funcall ,thread-fn ,task-input)))
                            (bt:with-lock-held (,format-lock) (format t "Thread received ~s has calculated ~s in ~,2f seconds~%" ,task-input ,result1 (* (- (get-internal-real-time) ,real-base) (/ 1.0 internal-time-units-per-second))))
                            ,result1)))
                      thread-fn)))
           (declare (type fixnum ,nb-tasks-sent ,nb-results-received ,nb-threads3)
                    (ignorable ,format-lock))
           ,(sexp-replace-sexp-beginning-by
             generator
             'submit
             (lambda (submit-sexp)
               (let ((task-input (cadr submit-sexp)))
                 `(progn
                    ;; send task:
                    (lparallel:submit-task ,channel1 ,thread-fn2 ,task-input)
                    (incf ,nb-tasks-sent)
                    ;; only when the first NB-THREADS tasks have been sent,
                    ;; wait for result:
                    (when (>= ,nb-tasks-sent ,nb-threads3)
                      (let ((,result1 (lparallel:receive-result ,channel1)))
                        (incf ,nb-results-received)
                        (funcall ,aggregate-fn ,result1)))))))
           ;; retrieve last results:
           (loop for ,_m from ,nb-results-received below ,nb-tasks-sent
                 do
                    (let ((,result1 (lparallel:receive-result ,channel1)))
                      (funcall ,aggregate-fn ,result1)))
           (when ,verbose (format t "~s tasks sent.~%" ,nb-tasks-sent))))))

(defun SHOW-ploop--throwable-threads (&optional (n 40))
  "Example of usage of ploop--throwable-threads."
  (let ((sum 0))
    (declare (type fixnum sum))
    (ploop--throwable-threads
     :generator (loop for i of-type fixnum from 1 to n do (submit i))
     :thread-fn (lambda (n1)
                  (declare (type fixnum n1))
                  (%long-function-A 3000)
                  (the fixnum (* n1 10)))
     :aggregate-fn (lambda (m) (declare (type fixnum m)) (incf sum m))
     :verbose t)
    sum))

(defmacro ploop--reusable-threads (&key generator thread-fn aggregate-fn (nb-threads nil) (verbose nil))
  "Execute GENERATOR, which is typically a loop, and which shall contain a (submit ...) sexp.
For each 'submitted' value(s), THREAD-FN is called on a separate thread.
Then AGGREGATE-FN is called on the successive return values.

Under the hood, NB-THREADS threads are launched and reused.

If NB-THREADS is null, the number of cores is used.
VERBOSE prints some information.

Example: see below."
  (with-gensyms (tasks-queue results-queue thread-pool nb-tasks-sent nb-results-received _i result1 thread1 task-input-name nb-threads3 thread-id real-base format-lock)
    `(with-parallelism
         (let* ((,nb-threads3 (if (null ,nb-threads) (nb-cores) ,nb-threads))
                (,tasks-queue (lparallel.queue:make-queue))
                (,results-queue (lparallel.queue:make-queue))
                (,thread-pool nil)
                (,nb-tasks-sent 0)
                (,nb-results-received 0)
                (,format-lock (bt:make-lock)))
           (declare (type fixnum ,nb-tasks-sent ,nb-results-received ,nb-threads3))
           ;; create thread pool:
           (setq ,thread-pool
                 (loop for ,_i of-type fixnum from 0 below ,nb-threads3
                       collect
                       (bt:make-thread
                        (lambda ()
                          (let ((,thread-id ,_i)
                                (,real-base 0))
                            (declare (type fixnum ,thread-id ,real-base))
                            (when ,verbose (bt:with-lock-held (,format-lock) (format t "Thread n~s created.~%" ,thread-id)))
                            (loop do
                              (let ((,task-input-name (lparallel.queue:pop-queue ,tasks-queue)))
                                (when ,verbose (setq ,real-base (get-internal-real-time)))
                                (let ((,result1 (funcall ,thread-fn ,task-input-name)))
                                  (when ,verbose (bt:with-lock-held (,format-lock) (format t "Thread n~s received ~s has calculated ~s in ~,2f seconds~%" ,thread-id ,task-input-name ,result1 (* (- (get-internal-real-time) ,real-base) (/ 1.0 internal-time-units-per-second)))))
                                  (lparallel.queue:push-queue ,result1 ,results-queue)))))))))
           ,(sexp-replace-sexp-beginning-by
             generator
             'submit
             (lambda (submit-sexp)
               (let ((task-input (cadr submit-sexp)))
                 `(progn
                    ;; send task:
                    (lparallel.queue:push-queue ,task-input ,tasks-queue)
                    (incf ,nb-tasks-sent)
                    ;; only when the first NB-THREADS tasks have been sent,
                    ;; wait for result:
                    (when (>= ,nb-tasks-sent ,nb-threads3)
                      (let ((,result1 (lparallel.queue:pop-queue ,results-queue)))
                        (incf ,nb-results-received)
                        (funcall ,aggregate-fn ,result1)))))))
           ;; Retrieve last results:
           (loop for _m from ,nb-results-received below ,nb-tasks-sent
                 do
                    (let ((,result1 (lparallel.queue:pop-queue ,results-queue)))
                      (incf ,nb-results-received)
                      (funcall ,aggregate-fn ,result1)))
           ;; Destroy thread pool:
           (loop for ,thread1 in ,thread-pool
                 do (bt:destroy-thread ,thread1))
           (when ,verbose (format t "~s tasks sent.~%" ,nb-tasks-sent))))))

(defun SHOW-ploop--reusable-threads (&optional (n 40))
  "Example of usage of ploop--reusable-threads."
  (declare (type fixnum n))
  (let ((sum 0))
    (declare (type fixnum sum))
    (ploop--reusable-threads
     :generator (loop for i of-type fixnum from 1 to n do (submit i))
     :thread-fn (lambda (n1)
                  (declare (type fixnum n1))
                  (%long-function-A 3000)
                  (the fixnum (* n1 10)))
     :aggregate-fn (lambda (m) (declare (type fixnum m)) (incf sum m))
     :verbose t)
    sum))

;;; ===
;;; === pfor by blocks
;;; ===

(defun pfor-by-blocks-with-pmap (&key from below nb-parts thread-fn aggregate-fn)
  "Split |[FROM, BELOW|[ in NB-PARTS parts (typically: 16, 32).
With lparallel:pmap, a thread is launched to process each part.
Thread is based on a function THREAD-FN, which accepts two arguments, which are the 'from' and
'below' of the part; the function/thread yields a return value.
AGGREGATE-FN manages the vector of results.

Example: see below."
  (declare (type fixnum from below)
           (type fixnum nb-parts)
           (type function thread-fn aggregate-fn))
  (let* ((nb (- below from))
         (chunk-size (floor nb nb-parts))
         (mins (make-array nb-parts :element-type 'fixnum :initial-contents
                           (loop for j of-type fixnum from 1 to nb-parts
                                 for m of-type fixnum = from then (+ m chunk-size)
                                 collect m)))
         (maxs (make-array nb-parts :element-type 'fixnum :initial-contents
                           (loop for j of-type fixnum from 1 to nb-parts
                                 for m of-type fixnum = (+ from chunk-size) then (+ m chunk-size)
                                 collect m))))
    (declare (type fixnum nb chunk-size)
             (type (simple-array fixnum (*)) mins maxs))
    (setf (aref maxs (- nb-parts 1)) below)
    (with-parallelism
        (funcall aggregate-fn (lparallel:pmap 'vector thread-fn mins maxs)))))

(defun SHOW-pfor-by-blocks-with-pmap (&optional (nb-parts 16))
  "Example of usage of pfor-by-blocks-with-pmap."
  (pfor-by-blocks-with-pmap :from 1 :below 101
                            :nb-parts nb-parts
                            :thread-fn (lambda (a b)
                                         (declare (type fixnum a b))
                                         (let ((sum 0))
                                           (declare (type fixnum sum))
                                           (loop for i of-type fixnum from a below b
                                                 do (%long-function-A 300)
                                                    (incf sum i))
                                           sum))
                            :aggregate-fn (lambda (v) (reduce #'+ v))))

;;; ===
;;; === Parallel first-which
;;; ===

(defmacro p-first-which (&key from fn target-reached-fn block-size (nb-threads '(nb-cores)) (verbose nil))
  "Index idx is incremented from FROM to infinity.
A pool of NB-THREADS (default: nb of cores) reusable threads processes the values of idx by blocks
of size BLOCK-SIZE (typically: 30). These threads are based on an augmented version of FN, which
accepts one argument (idx).
The function returns the first value of idx for which TARGET-REACHED-FN is true.
VERBOSE prints information.

Example: see below."
  (with-gensyms (continue1 min1 result1 task-input a b n n2 tmp1 tmp2 target-reached-p)
    `(let ((,continue1 t)
           (,min1 nil))
       (declare (type boolean ,continue1)
                (type list ,min1))
       (ploop--reusable-threads
        :generator (loop for i of-type fixnum from ,from while ,continue1 do (submit i))
        :thread-fn (lambda (,task-input)
                     (declare (type fixnum ,task-input))
                     (let* ((,a (* ,task-input ,block-size))
                            (,b (+ ,a ,block-size))
                            (,n2 0)
                            ,tmp2)
                       (declare (type fixnum ,a ,b ,n2))
                       (loop for ,n of-type fixnum from ,a below ,b
                             for ,tmp1 = (funcall ,fn ,n)
                             for ,target-reached-p of-type boolean = (funcall ,target-reached-fn ,tmp1)
                             until ,target-reached-p
                             finally (setq ,n2 ,n
                                           ,tmp2 ,tmp1))
                       (if (= ,n2 ,b) nil (list ,n2 ,tmp2))))
        :aggregate-fn (lambda (,result1)
                        (declare (type list ,result1))
                        (when ,result1
                          (let ((,n2 (car ,result1)))
                            (declare (type fixnum ,n2))
                            (when ,verbose (format t "(aggregator) ---> Found: ~s~%" ,result1))
                            (if ,continue1
                                (progn
                                  (setf ,continue1 nil)
                                  (setf ,min1 ,result1))
                                (progn
                                  (when (< ,n2 (the fixnum (car ,min1))) (setf ,min1 ,result1)))))))
        :nb-threads ,nb-threads
        :verbose ,verbose)
       ,min1)))

;;; ===
;;; === Parallel maximizing/minimizing (pmap-based)
;;; ===

(defmacro %p-maximizing-minimizing--based-on-pmap (predicate &key generator thread-fn)
  "Internal sub-macro for parallel maximizing/minimizing using lparallel:pmap."
    `(let ((res nil))
     (declare (type list res))
     (ploop--based-on-pmap
      :generator ,generator
      :thread-fn ,thread-fn
      :aggregate-fn (lambda (y)
                      (declare (type list y))
                      (unless (null y)
                        (let ((car-y (car y)))
                          (declare (type fixnum car-y))
                          (if (null res)
                              (setq res y)
                              (let ((car-res (car res)))
                                (declare (type fixnum car-res))
                                (cond ((,predicate car-y car-res)
                                       (setq res y))
                                      ((= car-y car-res)
                                       (setq res (list car-res
                                                       (append (cadr res)
                                                               (cadr y))))))))))))
     res))

(defmacro p-maximizing--based-on-pmap (&key generator thread-fn)
  "Execute the GENERATOR, which contains a 'submit', for instance: (loop for i from 1 do (submit
i)).
'Submitted' values are gathered in a list, on which lparallel:pmap is called.
Threads are based on THREAD-FN, which typically contains itself maximizing... maximize.
The macro returns (y xs) where y is the _fixnum_ maximum and xs is the list of values maximizing y.

Example: see below."
  `(%p-maximizing-minimizing--based-on-pmap > :generator ,generator :thread-fn ,thread-fn))

(defmacro p-minimizing--based-on-pmap (&key generator thread-fn)
  "Execute the GENERATOR, which contains a 'submit', for instance: (loop for i from 1 do (submit
i)).
'Submitted' values are gathered in a list, on which lparallel:pmap is called.
Threads are based on THREAD-FN, which typically contains itself minimizing... minimize.
The macro returns (y xs) where y is the _fixnum_ minimum and xs is the list of values minimizing y.

Example: see below."
  `(%p-maximizing-minimizing--based-on-pmap < :generator ,generator :thread-fn ,thread-fn))

;;; ===
;;; === Parallel maximizing/minimizing (throwable threads)
;;; ===

(defmacro %p-maximizing-minimizing--throwable-threads (predicate &key generator thread-fn (nb-threads nil) (verbose nil))
  "Internal sub-macro for parallel maximizing/minimizing using throwable threads."
  `(let ((res nil))
     (declare (type list res))
     (ploop--throwable-threads
      :generator ,generator
      :thread-fn ,thread-fn
      :aggregate-fn (lambda (y)
                      (declare (type list y))
                      (unless (null y)
                        (let ((car-y (car y)))
                          (declare (type fixnum car-y))
                          (if (null res)
                              (setq res y)
                              (let ((car-res (car res)))
                                (declare (type fixnum car-res))
                                (cond ((,predicate car-y car-res)
                                       (setq res y))
                                      ((= car-y car-res)
                                       (setq res (list car-res
                                                       (append (cadr res)
                                                               (cadr y)))))))))))
      :nb-threads ,nb-threads
      :verbose ,verbose)
     res))

(defmacro p-maximizing--throwable-threads (&key generator thread-fn (nb-threads nil) (verbose nil))
  "Execute the GENERATOR, which contains a 'submit', for instance: (loop for i from 1 do (submit
i)).
'Submitted' values are processed by throwable threads.
Threads are based on THREAD-FN, which typically contains itself maximizing... maximize.
The macro returns (y xs) where y is the _fixnum_ maximum and xs is the list of values maximizing y.
If NB-THREADS is null, the number of cores is used.

Example: see below."
  `(%p-maximizing-minimizing--throwable-threads > :generator ,generator :thread-fn ,thread-fn :nb-threads ,nb-threads :verbose ,verbose))

(defmacro p-minimizing--throwable-threads (&key generator thread-fn (nb-threads nil) (verbose nil))
  "Execute the GENERATOR, which contains a 'submit', for instance: (loop for i from 1 do (submit
i)).
'Submitted' values are processed by throwable threads.
Threads are based on THREAD-FN, which typically contains itself minimizing... minimize.
The macro returns (y xs) where y is the _fixnum_ minimum and xs is the list of values minimizing y.
If NB-THREADS is null, the number of cores is used.

Example: see below."
  `(%p-maximizing-minimizing--throwable-threads < :generator ,generator :thread-fn ,thread-fn :nb-threads ,nb-threads :verbose ,verbose))

;;; ===
;;; === Parallel maximizing/minimizing by blocks (with pmap)
;;; ===

(defmacro %p-maximizing-minimizing-by-blocks-with-pmap (predicate value-type &key from below nb-parts thread-fn (verbose nil))
  "Internal sub-macro for block-based parallel maximizing/minimizing."
    (with-gensyms (res)
    `(let ((,res nil))
       (declare (type list ,res))
       (pfor-by-blocks-with-pmap
        :from ,from
        :below ,below
        :nb-parts ,nb-parts
        :thread-fn ,thread-fn
        :aggregate-fn (lambda (v)
                        (declare (type (simple-array list) v))
                        (when ,verbose (format t "Threads results:~%~s~%" v))
                        (let ((at-least-one nil)
                              (val-opt ,(if (eq 'double-float value-type) '0.0d0 0))
                              (ns-opt nil))
                          (declare (type boolean at-least-one)
                                   (type ,value-type val-opt)
                                   (type list ns-opt))
                          (loop for i of-type fixnum from 0 below (length v)
                                for tmp of-type list = (aref v i)
                                unless (null tmp)
                                  do (let ((val ,(if (eq 'double-float value-type)
                                                     `(let ((tmp2 (car tmp))) (declare (type (simple-array double-float) tmp2)) (aref tmp2 0))
                                                     `(car tmp)))
                                           (ns (cadr tmp)))
                                       (declare (type ,value-type val)
                                                (type list ns))
                                       (if (not at-least-one)
                                           (setq val-opt val
                                                 ns-opt ns
                                                 at-least-one t)
                                           (if ,(if (eq 'fixnum value-type)
                                                    `(,predicate val val-opt)
                                                    `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) (,predicate val val-opt)))
                                               (setq val-opt val
                                                     ns-opt ns)
                                               (when ,(if (eq 'fixnum value-type)
                                                          `(= val val-opt)
                                                          `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) (= val val-opt)))
                                                 (setq ns-opt (append ns-opt ns)))))))
                          (setq ,res (if at-least-one
                                         (list ,(if (eq 'double-float value-type)
                                                   `(make-array 1 :element-type 'double-float :initial-contents (list val-opt))
                                                   `val-opt)
                                               ns-opt)
                                         nil)))))
       ,res)))

(defun p-maximizing-by-blocks-with-pmap--fixnum (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the fixnum maximum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (maximizing--fixnum ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap > fixnum :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun p-minimizing-by-blocks-with-pmap--fixnum (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the fixnum minimum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (minimizing--fixnum ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap < fixnum :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun p-maximizing-by-blocks-with-pmap--rational (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the rational maximum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (maximizing--rational ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap > rational :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun p-minimizing-by-blocks-with-pmap--rational (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the rational minimum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (minimizing--rational ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap < rational :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun p-maximizing-by-blocks-with-pmap--df (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the double-float maximum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (maximizing--df ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap > double-float :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun p-minimizing-by-blocks-with-pmap--df (&key from below nb-parts thread-fn (verbose nil))
  "Split |[FROM, BELOW|[ in NB-PARTS parts and find the double-float minimum using lparallel:pmap.
THREAD-FN accepts two arguments (from, below) and returns (minimizing--df ...) result.

Example: see below."
  (declare (type function thread-fn)
           (type fixnum from below nb-parts))
  (%p-maximizing-minimizing-by-blocks-with-pmap < double-float :from from :below below :nb-parts nb-parts :thread-fn thread-fn :verbose verbose))

(defun SHOW-all-parallelism-utilities ()
  "Run all parallelism utility demonstrations."
  (format t "~%~%======~%=== PARALLELISM UTILITIES~%======~%")
  (format t "~%--- pfor-by-blocks-with-pmap ---~%")
  (format t "Result: ~a~%" (SHOW-pfor-by-blocks-with-pmap 8)))

;;; ===
;;; === Leibniz formula demonstrations (parallelism examples)
;;; ===

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

