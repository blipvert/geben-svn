;;==============================================================
;; session
;;==============================================================

(require 'cl)
(require 'xml)
(require 'dbgp)
(require 'geben-common)
(require 'geben-util)

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

(defstruct (geben-session
	    (:constructor nil)
	    (:constructor geben-session-make))
  "Represent a DBGp protocol connection session."
  project
  process
  (tid 30000)
  (state :created)
  initmsg
  xdebug-p
  language
  feature
  redirect
  bp
  cmd
  sending-p
  source
  stack
  context
  (cursor (list :overlay nil :position nil))
  tempdir
  )
  
(defmacro geben-with-current-session (binding &rest body)
  (declare (indent 1))
  (cl-macroexpand-all
   `(let ((,binding geben-current-session))
      (when ,binding
	,@body))))

;; initialize

(defsubst geben-session-init (session init-msg)
  "Initialize a session of a process PROC."
  (let ((buf (process-buffer (geben-session-process session))))
    (and buf
	 (pop-to-buffer buf)))
  (geben-session-tempdir-setup session)
  (setf (geben-session-initmsg session) init-msg)
  (setf (geben-session-xdebug-p session)
	(equal "Xdebug" (car (xml-node-children
			      (car (xml-get-children init-msg 'engine))))))
  (setf (geben-session-language session)
	(let ((lang (xml-get-attribute-or-nil init-msg 'language)))
	  (and lang
	       (intern (concat ":" (downcase lang))))))
  (run-hook-with-args 'geben-session-enter-hook session))
  
(defsubst geben-session-release (session)
  "Initialize a session of a process PROC."
  (setf (geben-session-project session) nil)
  (setf (geben-session-process session) nil)
  (setf (geben-session-cursor session) nil)
  (geben-session-tempdir-remove session)
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

(defsubst geben-session-buffer-get (session format-string)
  (get-buffer-create (geben-session-buffer-name session format-string)))

;; temporary directory

(defcustom geben-temporary-file-directory temporary-file-directory
  "*Base directory path where GEBEN creates a temporary directory."
  :group 'geben
  :type 'directory)

(defun geben-session-tempdir-setup (session)
  "Setup temporary directory."
  (let* ((proc (geben-session-process session))
	 (topdir (file-truename geben-temporary-file-directory))
	 (gebendir (expand-file-name "emacs-geben" topdir))
	 (leafdir (format "%d" (second (process-contact proc))))
	 (tempdir (expand-file-name leafdir gebendir)))
    (unless (file-directory-p gebendir)
      (make-directory gebendir)
      (set-file-modes gebendir #o1777))
    (setf (geben-session-tempdir session) tempdir)))

(defun geben-session-tempdir-remove (session)
  "Remove temporary directory."
  (let ((tempdir (geben-session-tempdir session)))
    (when (file-directory-p tempdir)
      (geben-remove-directory-tree tempdir))))

(provide 'geben-session)