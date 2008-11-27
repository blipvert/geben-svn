;;------------------------------------------------------------------------
;; DBGp protocol handler
;;------------------------------------------------------------------------

(require 'geben-session)
(require 'geben-bp)

(defsubst geben-dbgp-tid-read (msg)
  "Get a transaction id of MSG."
  (let ((tid (xml-get-attribute-or-nil msg 'transaction_id)))
    (and tid
	 (string-to-number tid))))

(defun geben-dbgp-entry (session msg)
  "Analyze MSG and dispatch to a specific handler."
  ;; remain session status ('connect, 'init, 'break, 'stopping, 'stopped)
  (case (xml-node-name msg)
    ('connect
     (geben-session-put session :state'connect))
    ('init
     (geben-session-put session :state 'init)
     (geben-dbgp-handle-init session msg))
    ('response
     (let ((status (xml-get-attribute-or-nil msg 'status)))
       (and status
       (geben-session-put session :state (intern status))))
     (geben-dbgp-handle-response session msg))
    ('stream
     (geben-dbgp-handle-stream session msg))
    ('otherwise
     ;;TODO
     (warn "GEBEN- unknown message type, %S" msg))))

(defun geben-dbgp-handle-init (session msg)
  "Handle a init message."
  (run-hook-with-args 'geben-session-start-hook session)
  (geben-session-init-msg-store session msg)
  (geben-dbgp-fetch-source-file session (xml-get-attribute msg 'fileuri))
  (geben-dbgp-feature-init session)
  (geben-dbgp-redirect-init session)
  (geben-dbgp-command-context-names session)
  (geben-dbgp-restore-breakpoints session)
  (and nil
       (geben-dbgp-command-step-into session)))

(defun geben-dbgp-reset (session)
  "Reset GEBEN session."
  (maphash (lambda (fileuri source)
	     (geben-dbgp-source-release source))
	   (geben-session-source session))
  (when geben-show-breakpoints-debugging-only
    (geben-bp-hide-overlays session))
  ;;  (geben-session-initial-msg-put nil)
  ;;  (geben-session-stack-put session) nil)
  ;;  (setq geben-dbgp-context-names-alist nil)
  ;;  (setq geben-dbgp-context-variables nil)
  ;;  (clrhash (geben-session-cmd-get session))
  ;;  (clrhash geben-dbgp-source-hash)
  ;;  (setq geben-dbgp-send-command-queue nil)
  (geben-session-release session))

(defun geben-dbgp-handle-response (session msg)
  "Handle a response message."
  (let* ((tid (geben-dbgp-tid-read msg))
	 (cmd (geben-dbgp-cmd-remove session tid))
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

(defun geben-dbgp-handle-stream (session msg)
  "Handle a stream message."
  (let ((type (case (intern-soft (xml-get-attribute msg 'type))
		('stdout :stdout)
		('stderr :stderr)))
	(encoding (xml-get-attribute msg 'encoding))
	(content (car (last msg)))
	bufname buf outwin)
    (geben-dbgp-redirect-stream session type encoding content)))

(defun geben-dbgp-handle-status (session msg)
  "Handle status code in a response message."
  (let ((status (xml-get-attribute msg 'status)))
    (cond
     ((equal status "stopping")
      (and (geben-session-active-p session)
	   (geben-dbgp-command-stop session)))
     ((equal status "stopped")
      (run-hook-with-args 'geben-session-finished-hook session)
      (message "GEBEN debugging session is finished.")))))

