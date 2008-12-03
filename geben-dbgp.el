(require 'geben-common)
(require 'geben-session)
(require 'geben-dbgp-util)
(require 'geben-session)
(require 'geben-cmd)

;;==============================================================
;; DBGp protocol handler
;;==============================================================

(defsubst geben-dbgp-tid-read (msg)
  "Get a transaction id of MSG."
  (let ((tid (xml-get-attribute-or-nil msg 'transaction_id)))
    (and tid
	 (string-to-number tid))))

(defun geben-dbgp-entry (session msg)
  "Analyze MSG and dispatch to a specific handler."
  ;; remain session status ('connect, 'init, 'break, 'stopping, 'stopped)
  (let ((handler (intern-soft (concat "geben-dbgp-handle-"
				      (symbol-name (xml-node-name msg)))))
	(status (xml-get-attribute-or-nil msg 'status)))
    (and status
	 (setf (geben-session-state session) (intern (concat ":" status))))
    (and (functionp handler)
	 (funcall handler session msg))))

(defvar geben-dbgp-init-hook nil)

(defun geben-dbgp-handle-init (session msg)
  "Handle a init message."
  (geben-session-init session msg)
  (run-hook-with-args 'geben-dbgp-init-hook session))

(defun geben-dbgp-handle-response (session msg)
  "Handle a response message."
  (let* ((tid (geben-dbgp-tid-read msg))
	 (cmd (geben-session-cmd-remove session tid))
	 (err (dbgp-xml-get-error-node msg)))
    (geben-dbgp-handle-status session msg)
    (geben-dbgp-process-command-queue session)
    (cond
     (err
      (message "Command error: %s"
	       (dbgp-xml-get-error-message msg)))
     (cmd
      (let* ((operand (replace-regexp-in-string
		       "_" "-" (xml-get-attribute msg 'command)))
	     (func-name (concat "geben-dbgp-response-" operand))
	     (func (intern-soft func-name)))
	(and (functionp func)
	     (funcall func session cmd msg)))))
    (mapc (lambda (callback)
	    (funcall callback session cmd msg err))
	  (plist-get cmd :callback))))

(defun geben-dbgp-handle-status (session msg)
  "Handle status code in a response message."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopping")
      (accept-process-output)
      (and (geben-session-active-p session)
	   (geben-dbgp-command-stop session))))))

;;; command sending

(defun geben-dbgp-send-string (session string)
  (and (string< "" string)
       (geben-session-active-p session)
       (dbgp-session-send-string (geben-session-process session) string t)))

(defun geben-send-raw-command (session fmt &rest arg)
  "Send a command string to a debugger engine.
The command string will be built up with FMT and ARG with a help of
the string formatter function `format'."
  (let ((cmd (apply #'format fmt arg)))
    (geben-dbgp-send-string session cmd)))

(defun geben-dbgp-send-command (session operand &rest params)
  "Send a command to a debugger engine.
Return a cmd list."
  (if (geben-session-active-p session)
      (let ((cmd (geben-session-cmd-make session operand params)))
	(geben-session-cmd-append session cmd)
	(unless (geben-session-sending-p session)
	  (setf (geben-session-sending-p session) t)
	  (geben-dbgp-process-command-queue session))
	cmd)))

(defun geben-dbgp-process-command-queue (session)
  (let ((cmd (car (geben-session-cmd session))))
    (if cmd
	(geben-dbgp-send-string session (geben-cmd-expand cmd))
      (setf (geben-session-sending-p session) nil))))

(defvar geben-dbgp-continuous-command-hook nil)

;;--------------------------------------------------------------
;; continuous commands
;;--------------------------------------------------------------

;; step_into

(defun geben-dbgp-command-step-into (session)
  "Send \`step_into\' command."
  (geben-dbgp-send-command session "step_into"))

(defun geben-dbgp-response-step-into (session cmd msg)
  "A response message handler for \`step_into\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; step_over

(defun geben-dbgp-command-step-over (session)
  "Send \`step_over\' command."
  (geben-dbgp-send-command session "step_over"))

(defun geben-dbgp-response-step-over (session cmd msg)
  "A response message handler for \`step_over\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; step_out

(defun geben-dbgp-command-step-out (session)
  "Send \`step_out\' command."
  (geben-dbgp-send-command session "step_out"))

(defun geben-dbgp-response-step-out (session cmd msg)
  "A response message handler for \`step_out\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;; run

(defun geben-dbgp-command-run (session)
  "Send \`run\' command."
  (geben-dbgp-send-command session "run"))

(defun geben-dbgp-response-run (session cmd msg)
  "A response message handler for \`run\' command."
  (run-hook-with-args 'geben-dbgp-continuous-command-hook session))

;;; stop

(defun geben-dbgp-command-stop (session)
  "Send \`stop\' command."
  (geben-dbgp-send-command session "stop"))

;;; eval

(defun geben-dbgp-command-eval (session exp)
  "Send \`eval\' command."
  (geben-dbgp-send-command
   "eval"
   (format "-- {%s}" (base64-encode-string exp))))

(defun geben-dbgp-response-eval (session cmd msg)
  "A response message handler for \`eval\' command."
  (message "result: %S" 
	   (geben-dbgp-decode-value (car-safe (xml-get-children msg 'property)))))

(defun geben-dbgp-decode-value (prop)
  "Decode a VALUE passed by debugger engine."
  (let ((type (xml-get-attribute prop 'type))
	result)
    (setq result
	  (cond
	   ((or (string= "array" type)
		(string= "object" type))
	    (mapcar (lambda (value)
		      (geben-dbgp-decode-value value))
		    (xml-get-children prop 'property)))
	   ((string= "null" type)
	    nil)
	   (t
	    (let ((value (car (last prop))))
	      (assert (stringp value))
	      (when (string= "base64" (xml-get-attribute prop 'encoding))
		(setq value (base64-decode-string value)))
	      (if (string= "string" type)
		  (decode-coding-string value 'utf-8)
		(string-to-number value))))))
    (let ((name (xml-get-attribute-or-nil prop 'name)))
      (if name
	  (cons name result)
	result))))

(provide 'geben-dbgp)
