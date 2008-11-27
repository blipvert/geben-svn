;;--------------------------------------------------------------
;; redirect
;;--------------------------------------------------------------

(defvar geben-dbgp-redirect-stdout-current nil)
(defvar geben-dbgp-redirect-stderr-current nil)
(defvar geben-dbgp-redirect-combine-current nil)

(defcustom geben-dbgp-redirect-stdout :redirect
  "*If non-nil, GEBEN redirects the debuggee script's STDOUT.
If the value is \`:redirect', then STDOUT goes to both GEBEN and
default destination.
If the value is \`:intercept', then STDOUT never goes to the
regular destination but to GEBEN."
  :group 'geben
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Redirect" :redirect)
		 (const :tag "Intercept" :intercept))
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-stdout value
	       geben-dbgp-redirect-stdout-current value)))

(defcustom geben-dbgp-redirect-stderr :redirect
  "*If non-nil, GEBEN redirects the debuggee script's STDERR.
If the value is \`:redirect', then STDERR goes to both GEBEN and
default destination.
If the value is \`:intercept', then STDERR never goes to the
regular destination but to GEBEN."
  :group 'geben
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Redirect" :redirect)
		 (const :tag "Intercept" :intercept))
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-stderr value
	       geben-dbgp-redirect-stderr-current value)))

(defcustom geben-dbgp-redirect-combine t
  "*If non-nil, redirection of STDOUT and STDERR go to same buffer.
Or to each own buffer."
  :group 'geben
  :type 'boolean
  :set (lambda (sym value)
	 (setq geben-dbgp-redirect-combine value
	       geben-dbgp-redirect-combine-current value)))

(defcustom geben-dbgp-redirect-coding-system 'utf-8-dos
  "*Coding system for decoding redirect content."
  :group 'geben
  :type 'coding-system)

(defcustom geben-dbgp-redirect-buffer-init-hook nil
  "*Hook running at when a redirection buffer is created."
  :group 'geben
  :type 'hook)

(defvar geben-dbgp-redirect-bufferp nil)

(defun geben-dbgp-redirect-init (session)
  "Initialize redirection related variables."
  (when geben-dbgp-redirect-stdout-current
    (geben-dbgp-command-stdout session geben-dbgp-redirect-stdout-current))
  (when geben-dbgp-redirect-stderr-current
    (geben-dbgp-command-stderr session geben-dbgp-redirect-stderr-current)))

(defun geben-dbgp-redirect-stream (type encoding content)
  "Print redirected string to specific buffers."
  (let ((bufname (geben-dbgp-redirect-buffer-name type)))
    (when bufname
      (let* ((buf (or (get-buffer bufname)
		      (progn
			(with-current-buffer (get-buffer-create bufname)
			  (set (make-local-variable 'geben-dbgp-redirect-bufferp) t)
			  (setq buffer-undo-list t)
			  (run-hook-with-args 'geben-dbgp-redirect-buffer-init-hook)
			  (current-buffer)))))
      (outwin (display-buffer buf))
      save-pos)
	(with-current-buffer buf
	(setq save-pos (and (eq (point) (point-max))
			    (point)))
	(save-excursion
	  (goto-char (point-max))
	  (insert (decode-coding-string
		   (if (string= "base64" encoding)
		       (base64-decode-string content)
		     content)
		   geben-dbgp-redirect-coding-system))))
	(unless save-pos
	(save-selected-window
	  (select-window outwin)
	  (goto-char (point-max))))))))

(defun geben-dbgp-redirect-buffer-name (type)
  "Select buffer name for a redirection type."
  (when (or (and (eq type :stdout) geben-dbgp-redirect-stdout-current)
	    (and (eq type :stderr) geben-dbgp-redirect-stderr-current))
    (cond
     (geben-dbgp-redirect-combine-current
      geben-redirect-combine-buffer-name)
     ((eq :stdout type)
      geben-redirect-stdout-buffer-name)
     (t
      geben-redirect-stderr-buffer-name))))

(defun geben-dbgp-redirect-buffer-existp ()
  "Check whether any redirection buffer exists."
  (let (name)
    (or (and (setq name (geben-dbgp-redirect-buffer-name :stdout))
	     (get-buffer name))
	(and (setq name (geben-dbgp-redirect-buffer-name :stderr))
	     (get-buffer name)))))

(defun geben-dbgp-dynamic-property-bufferp (buf)
  (when (buffer-live-p buf)
    (or (eq buf (get-buffer geben-context-buffer-name))
	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name :stdout)))
	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name :stderr))))))

(defun geben-dbgp-dynamic-property-buffer-visiblep ()
  "Check whether any window displays any property buffer."
  (condition-case nil
      (and (mapc (lambda (buf)
		   (and (buffer-live-p buf)
			(get-buffer-window buf)
			(error nil)))
		 (list (get-buffer geben-context-buffer-name)
		 (geben-dbgp-redirect-buffer-existp)))
	   nil)
    (error t)))

  
