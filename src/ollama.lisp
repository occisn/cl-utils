(in-package :cl-utils)

;;; requires :jonathan :dexador :bordeaux-threads

(defun call-ollama1 (prompt &key (model "llama3") (stream :false))
  "Call Ollama's API with a prompt and return the response.
Use: (cl-utils:call-ollama1 'define recursion in three sentences')
Requires: jonathan, dexador"
  (let* ((url "http://localhost:11434/api/generate")
         (payload (jonathan:to-json
                   (list :|model| model
                         :|prompt| prompt
                         :|stream| stream)))
         (response (dex:post url
                             :content payload
                             :headers '(("Content-Type" . "application/json"))
                             :read-timeout 120 ; seconds
                             :connect-timeout 10
                             )))
    (let ((parsed (jonathan:parse response)))
      (getf parsed :|response|))))

(defun call-ollama2 (prompt &key (model "llama3") (stream :false))
  "Call Ollama's API with a prompt and return the response + timer and stats.
Prints 'Ollama not available' if connection is impossible.
Use: (cl-utils:call-ollama2 'define recursion in three sentences')
Requires: jonathan, dexador, bordeaux-threads"
  (let* ((url "http://localhost:11434/api/generate")
         (payload (jonathan:to-json
                   (list :|model| model
                         :|prompt| prompt
                         :|stream| stream)))
         (done nil)
         (timer (bt:make-thread
                 (lambda ()
                   (loop until done
                         do (write-char #\.)
                            (force-output)
                            (sleep 1)))
                 :name "ollama-timer")))
    (handler-case
        (unwind-protect
             (let* ((response (dex:post url
                                        :content payload
                                        :headers '(("Content-Type" . "application/json"))
                                        :read-timeout 300
                                        :connect-timeout 10))
                    (parsed (jonathan:parse response))
                    (answer          (getf parsed :|response|))
                    (total-ns        (getf parsed :|total_duration|))
                    (prompt-tokens   (getf parsed :|prompt_eval_count|))
                    (response-tokens (getf parsed :|eval_count|))
                    (tokens-per-sec  (getf parsed :|eval_duration|)))
               (setf done t)
               (bt:join-thread timer)
               (format t "~%~%--- Stats ---~%")
               (format t "Duration:         ~,2Fs~%" (/ total-ns 1.0d9))
               (format t "Prompt tokens:    ~d~%" prompt-tokens)
               (format t "Response tokens:  ~d~%" response-tokens)
               (format t "Total tokens:     ~d~%" (+ prompt-tokens response-tokens))
               (when (and tokens-per-sec (> tokens-per-sec 0))
                 (format t "Speed:            ~,1F tokens/s~%"
                         (/ response-tokens (/ tokens-per-sec 1.0d9))))
               (format t "~%")
               answer)
          (setf done t))
      (error ()
        (setf done t)
        (format t "~%Ollama not available~%")
        nil))))

(defun SHOW-all-ollama ()
  (format t "~%~%======~%=== OLLAMA~%======~%")
  (format t "~%")
  (format t "call-ollama1: skipped~%")
  (format t "call-ollama2:~%")
  (format t "~a~%" (call-ollama2 "Describe recursion in one sentence")))

;;; end
