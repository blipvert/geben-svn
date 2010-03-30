(require 'cl)
(require 'xml)
(require 'dbgp)
(require 'geben-storage)
(require 'geben-common)
(require 'geben-util)

;;==============================================================
;; session
;;==============================================================

;;--------------------------------------------------------------
;; constants
;;--------------------------------------------------------------

(defconst geben-process-buffer-name "*GEBEN<%s> process*"
  "Name for DBGp client process console buffer.")
(defconst geben-backtrace-buffer-name "*GEBEN<%s> backtrace*"
  "Name for backtrace buffer.")
(defconst geben-breakpoint-list-buffer-name "*GEBEN<%s> breakpoint list*"
  "Name for breakpoint list buffer.")
(defconst geben-context-buffer-name "*GEBEN<%s> context*"
  "Name for context buffer.")

(defvar geben-sessions nil)
(defvar geben-current-session nil)

;; geben session start/finish hooks

(defcustom geben-session-enter-hook nil
  "*Hook running at when the geben debugging session is starting.
Each function is invoked with one argument, SESSION"
  :group 'geben
  :type 'hook)

(defcustom geben-session-exit-hook nil
  "*Hook running at when the geben debugging session is finished."
  :group 'geben
  :type 'hook)

(defcustom geben-pause-at-entry-line t
  "*Specify whether debuggee script should be paused at the entry line.
If the value is t, GEBEN will automatically pause the starting program
at the entry line of the script."
  :group 'geben
  :type 'boolean)

(defstruct (geben-session
	    (:constructor nil)
	    (:constructor geben-session-make))
  "Represent a DBGp protocol connection session."
  storage
  process
  (tid 30000)
  (state :created)
  initmsg
  xdebug-p
  language
  feature
  redirect
  breakpoint
  cmd
  sending-p
  source
  stack
  context
  (cursor (list :overlay nil :position nil))
  tempdir
  )
  
(defmacro geben-with-current-session (binding &rest body)
  (declare (indent 1)
	   (debug (symbolp &rest form)))
  (cl-macroexpand-all
   `(let ((,binding geben-current-session))
      (when ,binding
	,@body))))

;; initialize

(defsubst geben-session-init (session init-msg)
  "Initialize a session of a process PROC."
  (geben-session-tempdir-setup session)
  (setf (geben-session-initmsg session) init-msg)
  (setf (geben-session-xdebug-p session)
	(equal "Xdebug" (car (xml-node-children
			      (car (xml-get-children init-msg 'engine))))))
  (setf (geben-session-language session)
	(let ((lang (xml-get-attribute-or-nil init-msg 'language)))
	  (and lang
	       (intern (concat ":" (downcase lang))))))
  (setf (geben-session-storage session) (or (geben-session-storage-find session)
					    (geben-session-storage-create session)))
  (run-hook-with-args 'geben-session-enter-hook session))

(defun geben-session-storage-create (session)
  (let* ((initmsg (geben-session-initmsg session))
	 (process (geben-session-process session))
	 (listener (dbgp-plist-get process :listener))
	 (storage (if (dbgp-proxy-p process)
		      (list :proxy t
			    :addr (xml-get-attribute initmsg 'hostname)
			    :idekey (xml-get-attribute initmsg 'idekey))
		    (list :proxy nil
			  :port (second (process-contact listener))))))
    (nconc storage (list :language (geben-session-language session)
			 :fileuri (xml-get-attribute initmsg 'fileuri)))
    (add-to-list 'geben-storages storage)
    storage))

(defun geben-session-storage-find (session)
  (unless geben-storage-loaded
    (geben-storage-load)
    (setq geben-storage-loaded t))
  (let* ((initmsg (geben-session-initmsg session))
	 (addr (xml-get-attribute initmsg 'hostname))
	 (fileuri (xml-get-attribute initmsg 'fileuri))
	 (idekey (xml-get-attribute initmsg 'idekey))
	 (process (geben-session-process session))
	 (listener (dbgp-plist-get process :listener))
	 (proxy-p (dbgp-proxy-p listener))
	 (port (second (process-contact listener))))
    (find-if (lambda (storage)
	       (and (eq (not proxy-p)
			(not (plist-get storage :proxy)))
		    (eq (geben-session-language session)
			(plist-get storage :language))
		    (equal fileuri (plist-get storage :fileuri))
		    (if proxy-p
			(and (equal addr (plist-get storage :addr))
			     (equal idekey (plist-get storage :idekey)))
		      (eq port (plist-get storage :port)))))
	     geben-storages)))

(defsubst geben-session-release (session)
  "Initialize a session of a process PROC."
  (setf (geben-session-process session) nil)
  (setf (geben-session-cursor session) nil)
  (geben-session-tempdir-remove session)
  (geben-storage-save)
  (run-hook-with-args 'geben-session-exit-hook session))
  
(defsubst geben-session-active-p (session)
  (let ((proc (geben-session-process session)))
    (and (processp proc)
	 (eq 'open (process-status proc)))))

;; tid

(defsubst geben-session-next-tid (session)
  "Get transaction id for next command."
  (prog1
      (geben-session-tid session)
    (incf (geben-session-tid session))))

;; buffer

(defsubst geben-session-buffer-name (session format-string)
  (let* ((proc (geben-session-process session))
	 (idekey (plist-get (dbgp-proxy-get proc) :idekey)))
    (format format-string
	    (concat (if idekey
			(format "%s:" idekey)
		      "")
		    (format "%s:%s"
			    (dbgp-ip-get proc)
			    (dbgp-port-get (dbgp-listener-get proc)))))))

(defsubst geben-session-buffer (session format-string)
  (get-buffer-create (geben-session-buffer-name session format-string)))

(defsubst geben-session-buffer-get (session format-string)
  (get-buffer (geben-session-buffer-name session format-string)))

(defsubst geben-session-buffer-live-p (session format-string)
  (buffer-live-p (get-buffer (geben-session-buffer-name session format-string))))

(defsubst geben-session-buffer-visible-p (session format-string)
  (let ((buf (get-buffer (geben-session-buffer-name session format-string))))
    (and buf
	 (buffer-live-p buf)
	 (get-buffer-window buf))))

;; temporary directory

(defun geben-session-tempdir-setup (session)
  "Setup temporary directory."
  (let* ((proc (geben-session-process session))
	 (gebendir (file-truename geben-temporary-file-directory))
	 (leafdir (format "%d" (second (process-contact proc))))
	 (tempdir (expand-file-name leafdir gebendir)))
    (unless (file-directory-p gebendir)
      (make-directory gebendir t)
      (set-file-modes gebendir #o1777))
    (setf (geben-session-tempdir session) tempdir)))

(defun geben-session-tempdir-remove (session)
  "Remove temporary directory."
  (let ((tempdir (geben-session-tempdir session)))
    (when (file-directory-p tempdir)
      (geben-remove-directory-tree tempdir))))

;; misc

(defsubst geben-session-ip-get (session)
  "Get ip address of the host server."
  (let* ((proc (geben-session-process session))
	 (listener (dbgp-listener-get proc)))
    (format-network-address (dbgp-ip-get proc) t)))

(defun geben-session-remote-p (session)
  "Get ip address of the host server."
  (geben-remote-p (geben-session-ip-get session)))

(provide 'geben-session)