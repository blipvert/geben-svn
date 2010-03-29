(require 'dbgp)
(require 'geben-common)
(require 'geben-util)
(require 'geben-session)
(require 'geben-dbgp)

;;==============================================================
;; DBGp starter
;;==============================================================

(defun geben-dbgp-start (port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  (condition-case error-sexp
      (let* ((result (dbgp-exec port
				:session-accept 'geben-dbgp-session-accept-p
				:session-init 'geben-dbgp-session-init
				:session-filter 'geben-dbgp-session-filter
				:session-sentinel 'geben-dbgp-session-sentinel))
	     (listener (and (consp result)
			    (car result))))
	(when (processp listener)
	  (message "Waiting for debug server to connect at port %s." port)))
    (error
     (beep)
     (read-char (format "[port %s] %s" port (second error-sexp))
		nil 3))))

(defun geben-dbgp-start-proxy (ip-or-addr port idekey ;;multi-session-p
					  session-port)
  "Create DBGp listeners at each CONNECTION-POINTS."
  (condition-case error-sexp
      (let* ((result
	      (dbgp-proxy-register-exec ip-or-addr port idekey nil ;; multi-session-p
					session-port
					:session-accept 'geben-dbgp-session-accept-p
					:session-init 'geben-dbgp-session-init
					:session-filter 'geben-dbgp-session-filter
					:session-sentinel 'geben-dbgp-session-sentinel))
	     (listener (and (consp result)
			    (car result))))
	(when (processp listener)
	  (message "Waiting for debug server to connect.")))
    (error
     (beep)
     (read-char (format "[proxy %s:%s-%s] %s"
			ip-or-addr port idekey (second error-sexp))
		nil 3))))

(defun geben-dbgp-session-accept-p (proc)
  "Judge whether the SESSION is to be processed or to be terminated."
  ;; accept the new session if:
  ;;  a. capable for multi sessions.
  ;;  b. not used yet; it's the first session for the connection-point.
  (let ((accept-p
	 (if (dbgp-proxy-p proc)
	     (let ((proxy (dbgp-plist-get proc :proxy)))
	       (or (plist-get proxy :multi-session)
		   (not (some (lambda (session)
				(eq proxy (dbgp-plist-get proc :proxy)))
			      geben-sessions))))
	   (let ((port (dbgp-port-get (dbgp-listener-get proc))))
	     (not (some (lambda (session)
			  (let ((oproc (geben-session-process session)))
			    (and oproc
				 (not (dbgp-proxy-p oproc))
				 (eq port (dbgp-port-get (dbgp-listener-get oproc))))))
			geben-sessions))))))
    (unless accept-p
      (message "GEBEN: Rejected new connection from %s (Already in debugging)"
	       (car (process-contact proc))))
    accept-p))
	
(defun geben-dbgp-session-init (proc)
  "Initialize SESSION environment."
  (let ((session (geben-session-make :process proc)))
    (push session geben-sessions)
    (dbgp-plist-put proc :session session)
    (with-current-buffer (process-buffer proc)
      (set (make-local-variable 'geben-current-session) session)
      (rename-buffer (geben-session-buffer-name session geben-process-buffer-name) t))))
  
(defun geben-dbgp-session-filter (proc string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `geben-dbgp-entry' with each chunk."
  (let ((session (dbgp-plist-get proc :session))
	xml output)
    (with-temp-buffer
      (insert string)
      (setq output
	    (or (ignore-errors
		  (setq xml (xml-parse-region (point-min) (point-max)))
		  (goto-char (point-min))
		  (when (re-search-forward "\\?>" nil t)
		    (delete-region (match-end 0) (point-max))
		    (insert "\n")
		    (xml-print xml)
		    (propertize (buffer-string)
				'front-sticky t
				'font-lock-face 'dbgp-response-face)))
		string)))
    (when xml
      (condition-case error-sexp
	  (geben-dbgp-entry session (car xml))
	(error
	 (warn "GEBEN internal error: %S" error-sexp))))
    output))

(defun geben-dbgp-session-sentinel (proc string)
  (when (buffer-live-p (process-buffer proc))
    (dbgp-session-echo-input proc "\nDisconnected.\n\n"))
  (let ((session (dbgp-plist-get proc :session)))
    (when session
      (ignore-errors
	(geben-session-release session))
      (accept-process-output)
      (setq geben-sessions (remq session geben-sessions)))))

(add-hook 'kill-emacs-hook (lambda ()
			     (dolist (session geben-sessions)
			       (ignore-errors
				(geben-session-release session)))))

(provide 'geben-dbgp-start)