(require 'cl)
(require 'geben-common)
(require 'geben-session)
(require 'geben-dbgp)
(require 'geben-cmd)

;;==============================================================
;; redirect
;;==============================================================

(defconst geben-redirect-combine-buffer-name "*GEBEN<%s> output*"
  "Name for the debuggee script's STDOUT and STDERR redirection buffer.")
(defconst geben-redirect-stdout-buffer-name "*GEBEN<%s> stdout*"
  "Name for the debuggee script's STDOUT redirection buffer.")
(defconst geben-redirect-stderr-buffer-name "*GEBEN<%s> stderr*"
  "Name for the debuggee script's STDERR redirection buffer.")

(defstruct (geben-redirect
	    (:constructor nil)
	    (:constructor geben-redirect-make))
  (stdout :redirect)
  (stderr :redirect)
  (combine t)
  (coding-system 'utf-8))

(defcustom geben-dbgp-redirect-buffer-init-hook nil
  "*Hook running at when a redirection buffer is created."
  :group 'geben
  :type 'hook)

(defun geben-session-redirect-init (session)
  (setf (geben-session-redirect session) (geben-redirect-make))
  (when (geben-session-process session)
    (dolist (type '(:stdout :stderr))
      (let ((buf (get-buffer (geben-session-redirect-buffer-name session type))))
	(when (buffer-live-p buf)
	  (with-current-buffer buf
	    (let ((inhibit-read-only t)
		  (inhibit-modification-hooks t))
	      (erase-buffer))))))))

(add-hook 'geben-session-enter-hook #'geben-session-redirect-init)

(defun geben-session-redirect-buffer (session type)
  (let ((bufname (geben-session-redirect-buffer-name session type)))
    (when bufname
      (or (get-buffer bufname)
	  (with-current-buffer (get-buffer-create bufname)
	    (unless (local-variable-p 'geben-dynamic-property-buffer-p)
	      (set (make-local-variable 'geben-dynamic-property-buffer-p) t)
	      (setq buffer-undo-list t)
	      (run-hook-with-args 'geben-dbgp-redirect-buffer-init-hook (current-buffer)))
	    (current-buffer))))))
  
(defun geben-session-redirect-buffer-name (session type)
  "Select buffer name for a redirection type."
  (let ((redirect (geben-session-redirect session)))
    (when (or (and (eq type :stdout)
		   (geben-redirect-stdout redirect))
	      (and (eq type :stderr)
		   (geben-redirect-stderr redirect)))
      (geben-session-buffer-name session 
				 (cond
				  ((geben-redirect-combine redirect)
				   geben-redirect-combine-buffer-name)
				  ((eq :stdout type)
				   geben-redirect-stdout-buffer-name)
				  (t
				   geben-redirect-stderr-buffer-name))))))

(defun geben-session-redirect-buffer-existp (session)
  "Check whether any redirection buffer exists."
  (let (name)
    (or (and (setq name (geben-session-redirect-buffer-name session :stdout))
	     (get-buffer name))
	(and (setq name (geben-session-redirect-buffer-name session :stderr))
	     (get-buffer name)))))

(defun geben-dbgp-redirect-init (session)
  "Initialize redirection related variables."
  (let ((stdout (geben-redirect-stdout (geben-session-redirect session)))
	(stderr (geben-redirect-stderr (geben-session-redirect session))))
    (when stdout
      (geben-dbgp-command-stdout session stdout))
    (when stderr
      (geben-dbgp-command-stderr session stderr))))

(defun geben-dbgp-handle-stream (session msg)
  "Handle a stream message."
  (let ((type (case (intern-soft (xml-get-attribute msg 'type))
		('stdout :stdout)
		('stderr :stderr)))
	(encoding (xml-get-attribute msg 'encoding))
	(content (car (last msg))))
    (geben-dbgp-redirect-stream session type encoding content)))

(defun geben-dbgp-redirect-stream (session type encoding content)
  "Print redirected string to specific buffers."
  (let ((buf (geben-session-redirect-buffer session type))
	save-pos)
    (when buf
      (with-current-buffer buf
	(setq save-pos (unless (eobp) (point)))
	(save-excursion
	  (goto-char (point-max))
	  (insert (decode-coding-string
		   (if (string= "base64" encoding)
		       (base64-decode-string content)
		     content)
		   (geben-redirect-coding-system (geben-session-redirect session)))))
	(goto-char (or save-pos
		       (point-max))))
      (geben-dbgp-display-window buf))))

(defun geben-dbgp-command-stdout (session mode)
  "Send `stdout' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command session "stdout" (cons "-c" m)))))

(defun geben-dbgp-response-stdout (session cmd msg)
  "A response message handler for `stdout' command."
  (setf (geben-redirect-stdout (geben-session-redirect session))
	(case (geben-cmd-param-get cmd "-c")
	  (0 nil)
	  (1 :redirect)
	  (2 :intercept))))

(defun geben-dbgp-command-stderr (session mode)
  "Send `stderr' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command session "stderr" (cons "-c" m)))))

(defun geben-dbgp-response-stderr (session cmd msg)
  "A response message handler for `stderr' command."
  (setf (geben-redirect-stderr (geben-session-redirect session))
	(case (geben-cmd-param-get cmd "-c")
	  (0 nil)
	  (1 :redirect)
	  (2 :intercept))))

(provide 'geben-redirect)
