;;--------------------------------------------------------------
;; DBGp protocol handler
;;--------------------------------------------------------------

(require 'geben-common)
(require 'geben-session)
(require 'geben-dbgp-util)
(require 'geben-session)
(require 'geben-cmd)

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
  (run-hook-with-args 'geben-dbgp-init-hook session msg)
  ;;  (geben-dbgp-fetch-source-file session (xml-get-attribute msg 'fileuri))
  ;;  (geben-dbgp-feature-init session)
  ;;  (geben-dbgp-redirect-init session)
  ;;  (geben-dbgp-command-context-names session)
  ;;  (geben-dbgp-breakpoint-restore session)
  ;;  (and nil
  ;;       (geben-dbgp-command-step-into session)))
  )

(defun geben-dbgp-handle-response (session msg)
  "Handle a response message."
  (let* ((tid (geben-dbgp-tid-read msg))
	 (cmd (geben-session-cmd-remove session tid))
	 (err (dbgp-xml-get-error-message msg)))
    (geben-dbgp-handle-status session msg)
    (geben-dbgp-process-command-queue session)
    (cond
     (err
      (message "Command error: %s"
	       (third (car-safe (xml-get-children (car err) 'message)))))
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
      (and (geben-session-active-p session)
	   (geben-dbgp-command-stop session))))))

;;; command sending

(defun geben-dbgp-send-string (session string)
  (and (geben-session-active-p session)
       (dbgp-session-send-string session string t)))

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
;; features
;;--------------------------------------------------------------

(defcustom geben-dbgp-feature-list
  '((:set max_data 32768)
    (:set max_depth 1)
    (:set max_children 32)
    (:get breakpoint_types geben-dbgp-store-breakpoint-types))
  "*Specifies set of feature variables for each new debugging session.
Each entry forms a list (METHOD FEATURE_NAME VALUE_OR_CALLBACK).
METHOD is either `:get' or `:set'.
FEATURE_NAME is a feature name described in DBGp specification.
VALUE_OR_CALLBACK is, if the METHOD is `:get' then it should
be symbol of a callback function will be invoked 3 arguments
\(CMD MSG ERR), which are results of feature_get DBGp command.
If the method is `:set' VALUE_OR_CALLBACK can be either a value
or a symbol of a function. In the latter case the result value
of the function is passed to feature_set DBGp command."
  :group 'geben
  :type '(repeat (list (radio (const :get)
			      (const :set))
		       (radio (const :help-echo ":get" :tag "language_supports_threads (:get)" language_supports_threads)
			      (const :tag "language_name (:get)" language_name)
			      (const :tag "encoding (:get)" encoding)
			      (const :tag "protocol_version (:get)" protocol_version)
			      (const :tag "supports_async (:get)" supports_async)
			      (const :tag "data_encoding (:get)" data_encoding)
			      (const :tag "breakpoint_languages (:get)" breakpoint_languages)
			      (const :tag "breakpoint_types (:get)" breakpoint_types)
			      (const :tag "multiple_sessions (:get :set)" multiple_sessions)
			      (const :tag "encoding (:get :set)" encoding)
			      (const :tag "max_children (:get :set)" max_children)
			      (const :tag "max_data (:get :set)" max_data)
			      (const :tag "max_depth (:get :set)" max_depth)
			      (const :tag "supports_postmortem (:get)" supports_postmortem)
			      (const :tag "show_hidden (:get :set)" show_hidden)
			      (const :tag "notify_ok (:get :set)" notify_ok))
		       sexp)))

(defun geben-dbgp-feature-init (session)
  "Configure debugger engine with value of `geben-dbgp-feature-list'."
  (let ((features (or (geben-session-feature session)
		      geben-dbgp-feature-list)))
    (dolist (entry features)
      (let ((method (car entry))
	    (name (symbol-name (nth 1 entry)))
	    (param (nth 2 entry)))
	(case method
	      (:set 
	       (let ((value (cond
			     ((null param) nil)
			     ((symbolp param)
			      (if (fboundp param)
				  (funcall param)
				(if (boundp param)
				    (symbol-value param)
				  (symbol-name param))))
			     (t param))))
		 (geben-dbgp-command-feature-set session name value)))
	      (:get
	       (if (and (symbolp param)
			(fboundp param))
		   (geben-dbgp-sequence
		    (geben-dbgp-command-feature-get session name)
		     param))
		 (error "`geben-dbgp-feature-alist' has invalid entry: %S" entry)))))))

;;--------------------------------------------------------------
;; continuous commands
;;--------------------------------------------------------------

;; step_into

(defun geben-dbgp-command-step-into (session)
  "Send \`step_into\' command."
  (geben-dbgp-send-command session "step_into"))

(defun geben-dbgp-response-step-into (session cmd msg)
  "A response message handler for \`step_into\' command."
  (run-hooks 'geben-dbgp-continuous-command-hook))

;; step_over

(defun geben-dbgp-command-step-over (session)
  "Send \`step_over\' command."
  (geben-dbgp-send-command session "step_over"))

(defun geben-dbgp-response-step-over (session cmd msg)
  "A response message handler for \`step_over\' command."
  (run-hooks 'geben-dbgp-continuous-command-hook))

;; step_out

(defun geben-dbgp-command-step-out (session)
  "Send \`step_out\' command."
  (geben-dbgp-send-command session "step_out"))

(defun geben-dbgp-response-step-out (session cmd msg)
  "A response message handler for \`step_out\' command."
  (run-hooks 'geben-dbgp-continuous-command-hook))

;; run

(defun geben-dbgp-command-run (session)
  "Send \`run\' command."
  (geben-dbgp-send-command session "run"))

(defun geben-dbgp-response-run (session cmd msg)
  "A response message handler for \`run\' command."
  (run-hooks 'geben-dbgp-continuous-command-hook))

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

;; feature

(defun geben-dbgp-command-feature-get (session feature)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_get" (cons "-n" feature)))

(defun geben-dbgp-command-feature-set (session feature value)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_set"
			   (cons "-n" feature)
			   (cons "-v" (format "%S" (eval value)))))

(provide 'geben-dbgp)
