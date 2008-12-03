(require 'dbgp)
(require 'geben-common)
(require 'geben-util)
(require 'geben-project)
(require 'geben-session)
(require 'geben-dbgp)

;;==============================================================
;; DBGp starter
;;==============================================================

(defun geben-dbgp-start (projects)
  "Create DBGp listeners for each PROJECTS."
  (let (succeeded)
    (dolist (project projects)
      (condition-case error-sexp
	  (let* ((result
		  (cond
		   ((geben-project-p project)
		    (dbgp-exec (geben-project-listen-port project)
			       :project project
			       :session-accept 'geben-dbgp-session-accept-p
			       :session-init 'geben-dbgp-session-init
			       :session-filter 'geben-dbgp-session-filter
			       :session-sentinel 'geben-dbgp-session-sentinel))
		   ((geben-proxy-project-p project)
		    (dbgp-proxy-register-exec (geben-proxy-project-addr project)
					      (geben-proxy-project-port project)
					      (geben-proxy-project-idekey project)
					      (geben-proxy-project-multi-session project)
					      :project project
					      :session-accept 'geben-dbgp-session-accept-p
					      :session-init 'geben-dbgp-session-init
					      :session-filter 'geben-dbgp-session-filter
					      :session-sentinel 'geben-dbgp-session-sentinel))))
		 (listener (and (consp result)
				(car result))))
	    (when (processp listener)
	      (add-to-list 'geben-running-projects (cons project listener))
	      (setq succeeded t)))
	(error
	 (let ((msg (format "[%s] %s"
			    (if (geben-project-p project)
				(format "port %s"
					(geben-project-listen-port project))
			      (format "proxy %s:%s-%s"
				      (geben-proxy-project-addr project)
				      (geben-proxy-project-port project)
				      (geben-proxy-project-idekey project)))
			    (second error-sexp))))
	   (beep)
	   (read-char msg nil 3)))))
    (if succeeded
	(message "Waiting for debug server to connect."))))

(defun geben-dbgp-session-accept-p (proc)
  "Judge whether the SESSION is to be processed or to be terminated."
  (let ((project (dbgp-plist-get proc :project)))
    ;; accept session if the project of the session is:
    ;;  a. capable for multi sessions.
    ;;  b. not used yet; it's the first session for the project.
    (or (and (geben-proxy-project-p project)
	     (geben-proxy-project-multi-session project))
	(not (find-if (lambda (session)
			(eq project (dbgp-plist-get proc :project)))
		      geben-sessions)))))
	
(defun geben-dbgp-session-init (proc)
  "Initialize SESSION environment."
  (let ((session (geben-session-make :project (dbgp-plist-get proc :project)
				     :process proc)))
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
    (with-current-buffer (process-buffer proc)
      (insert "\nDisconnected.\n\n")))
  (let ((session (dbgp-plist-get proc :session)))
    (when session
      (ignore-errors
	(geben-session-release session))
      (accept-process-output)
      (setq geben-sessions (remq session geben-sessions)))))

(provide 'geben-dbgp-start)