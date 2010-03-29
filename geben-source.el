(require 'cl)
(require 'geben-common)
(require 'geben-session)
(require 'geben-cmd)
(require 'geben-dbgp)
(eval-when-compile
  (require 'tramp))

;;==============================================================
;; source
;;==============================================================

;; file hooks

(defcustom geben-source-visit-hook nil
  "*Hook running at when GEBEN visits a debuggee script file.
Each function is invoked with one argument, BUFFER."
  :group 'geben
  :type 'hook)

(defcustom geben-close-mirror-file-after-finish t
  "*Specify whether GEBEN should close fetched files from remote site after debugging.
Since the remote files is stored temporary that you can confuse
they were editable if they were left after a debugging session.
If the value is non-nil, GEBEN closes temporary files when
debugging is finished.
If the value is nil, the files left in buffers."
  :group 'geben
  :type 'boolean)

(defun geben-source-find-file-handler ()
  (let* ((local-path (buffer-file-name))
	 (session (and local-path (geben-source-find-session local-path))))
    (if session
	(run-hook-with-args 'geben-source-visit-hook session (current-buffer)))))

(add-hook 'find-file-hook #'geben-source-find-file-handler)

;;--------------------------------------------------------------
;; source hash
;;--------------------------------------------------------------

(defcustom geben-source-coding-system 'utf-8
  "Coding system for source code retrieving remotely via the debugger engine."
  :group 'geben
  :type 'coding-system)

(defmacro geben-source-make (fileuri local-path)
  "Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
  `(list :fileuri ,fileuri :local-path ,local-path))

(defvar geben-source-release-hook nil)

(defun geben-source-release (source)
  "Release a SOURCE object."
  (let ((buf (find-buffer-visiting (or (plist-get source :local-path) ""))))
    (when buf
      (with-current-buffer buf
	(when (and (boundp 'geben-mode)
		   (symbol-value 'geben-mode))
	  (run-hooks 'geben-source-release-hook))
	;;	  Not implemented yet
	;; 	  (and (buffer-modified-p buf)
	;; 	       (switch-to-buffer buf)
	;; 	       (yes-or-no-p "Buffer is modified. Save it?")
	;; 	       (geben-write-file-contents this buf))
	(when geben-close-mirror-file-after-finish
	  (set-buffer-modified-p nil)
	  (kill-buffer buf))))))

(defsubst geben-source-fileuri-regularize (fileuri)
  ;; for bug of Xdebug 2.0.3 and below:
  (replace-regexp-in-string "%28[0-9]+%29%20:%20runtime-created%20function$" ""
			    fileuri))
  
(defun geben-source-fileuri (session local-path)
  "Guess a file uri string which counters to LOCAL-PATH."
  (let* ((tempdir (geben-session-tempdir session))
	 (templen (length tempdir))
	 (tramp-spec (plist-get (geben-session-storage session) :tramp))
	 (tramp-spec-len (and tramp-spec (length tramp-spec))))
    (concat "file://"
	    (cond
	     ((and (< templen (length local-path))
		   (string= tempdir (substring local-path 0 templen)))
	      (substring local-path
			 (- templen
			    (if (string< "" (file-name-nondirectory tempdir)) 0 1))))
	     ((and tramp-spec
		   (< tramp-spec-len (length local-path))
		   (string= tramp-spec (substring local-path 0 tramp-spec-len)))
	      (substring local-path tramp-spec-len))
	     (t
	      local-path)))))

(defun geben-source-local-path (session fileuri)
  "Generate path string from FILEURI to store temporarily."
  (let ((local-path (geben-source-local-path-in-server session fileuri)))
    (when local-path
      (expand-file-name (substring local-path (if (string-match "^[A-Z]:" local-path) 3 1))
			(geben-session-tempdir session)))))

(defun geben-source-local-path-in-server (session fileuri &optional disable-completion)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (require 'url-util)
      (setq path (url-unhex-string path))
      (when (string-match "^/[A-Z]:" path) ;; for HTTP server on Windows
	(setq path (substring path 1)))
      (if (and (not disable-completion)
	       (string= "" (file-name-nondirectory path)))
	  (expand-file-name (geben-source-default-file-name session)
			    path)
	path))))

(defun geben-source-default-file-name (session)
  (case (geben-session-language session)
    (:php "index.php")
    (:python "index.py")
    (:perl "index.pl")
    (:ruby "index.rb")
    (t "index.html")))

(defun geben-source-find-session (temp-path)
  "Find a session which may have a file at TEMP-PATH in its temporary directory tree."
  (find-if (lambda (session)
	     (let ((tempdir (geben-session-tempdir session)))
	       (ignore-errors
		 (string= tempdir (substring temp-path 0 (length tempdir))))))
	   geben-sessions))

(defun geben-source-visit (local-path)
  "Visit to a local source code file."
  (let ((buf (or (find-buffer-visiting local-path)
		 (if (file-exists-p local-path)
		     (let* ((session (geben-source-find-session local-path))
			    (storage (and session
					  (geben-session-storage session)))
			    (coding-system (or (plist-get storage :source-coding-system)
					       geben-source-coding-system)))
		       (if coding-system
			   (let ((coding-system-for-read coding-system)
				 (coding-system-for-write coding-system))
			     (find-file-noselect local-path))
			 (find-file-noselect local-path)))))))
    (when buf
      (geben-dbgp-display-window buf)
      buf)))

;; session storage

(defun geben-session-source-storage-add (session fileuri)
  (let* ((storage (geben-session-storage session))
	 (list (plist-get storage :source)))
    (if (and (string-match "^file:/" fileuri)
	     (not (find list fileuri :test #'equal)))
	(if list
	    (nconc list (list fileuri))
	  (plist-put storage :source (list fileuri))))))

;; session

(defun geben-session-source-init (session)
  "Initialize a source hash table of the SESSION."
  (setf (geben-session-source session) (make-hash-table :test 'equal)))

(add-hook 'geben-session-enter-hook #'geben-session-source-init)

(defun geben-session-source-add (session fileuri local-path content)
  "Add a source object to SESSION."
  (let ((tempdir (geben-session-tempdir session)))
    (unless (file-directory-p tempdir)
      (make-directory tempdir t)
      (set-file-modes tempdir #o0700)))
  (geben-session-source-write-file session local-path content)
  (puthash fileuri (geben-source-make fileuri local-path) (geben-session-source session))
  (geben-session-source-storage-add session fileuri))

(defun geben-session-source-release (session)
  "Release source objects."
  (maphash (lambda (fileuri source)
	     (geben-source-release source))
	   (geben-session-source session)))

(add-hook 'geben-session-exit-hook #'geben-session-source-release)

(defsubst geben-session-source-get (session fileuri)
  (gethash fileuri (geben-session-source session)))

(defsubst geben-session-source-append (session fileuri local-path)
  (puthash fileuri (list :fileuri fileuri :local-path local-path)
	   (geben-session-source session)))

(defsubst geben-session-source-local-path (session fileuri)
  "Find a known local-path that counters to FILEURI."
  (plist-get (gethash fileuri (geben-session-source session))
	     :local-path))

(defsubst geben-session-source-fileuri (session local-path)
  "Find a known fileuri that counters to LOCAL-PATH."
  (block geben-session-souce-fileuri
    (maphash (lambda (fileuri path)
	       (and (equal local-path (plist-get path :local-path))
		    (return-from geben-session-souce-fileuri fileuri)))
	     (geben-session-source session))))

(defsubst geben-session-source-content-coding-system (session content)
  "Guess a coding-system for the CONTENT."
  (or (plist-get (geben-session-storage session) :source-coding-system)
      geben-source-coding-system
      (detect-coding-string content t)))

(defun geben-session-source-write-file (session path content)
  "Write CONTENT to file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
			     (create-file-buffer path))
      (let ((inhibit-read-only t)
	    (coding-system (geben-session-source-content-coding-system session content)))
	(buffer-disable-undo)
	(widen)
	(erase-buffer)
	(font-lock-mode 0)
	(unless (eq 'undecided coding-system)
	  (set-buffer-file-coding-system coding-system))
	(insert (decode-coding-string content coding-system)))
      (with-temp-message ""
	(write-file path)
	(kill-buffer (current-buffer))))
    t))

;;; dbgp

(defun geben-dbgp-command-source (session fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (geben-dbgp-send-command session "source" (cons "-f"
						  (geben-source-fileuri-regularize fileuri))))

(defun geben-dbgp-response-source (session cmd msg)
  "A response message handler for \`source\' command."
  (let* ((fileuri (geben-cmd-param-get cmd "-f"))
	 (local-path (geben-source-local-path session fileuri)))
    (when local-path
      (geben-session-source-add session fileuri local-path (base64-decode-string (third msg)))
      (geben-source-visit local-path))))

(defun geben-dbgp-source-fetch (session fileuri)
  "Fetch the content of FILEURI."
  ;;(let ((fileuri (geben-dbgp-regularize-fileuri fileuri)))
  (unless (geben-session-source-local-path session fileuri)
    ;; haven't fetched remote source yet; fetch it.
    (geben-dbgp-command-source session fileuri)))

(defcustom geben-visit-remote-file nil
  ""
  :group 'geben
  :type 'function)

(defcustom geben-get-tramp-spec-for nil
  "Function to retrieve TRAMP spec for a file path of a remove server.
This function is called when visiting a remote server file, with
a parameter `remote-path'. (e.g. \"/192.168.1.32:/var/www/index.php\")
If `remote-path' is unknown to the function, it should return nil.
Or return specific TRAMP spec. (e.g. \"/user@example.com:\""
  :group 'geben
  :type 'function)

(defun geben-session-source-visit-original-file (session fileuri &optional disable-completion)
  (let ((target-path (geben-session-source-read-file-name session fileuri disable-completion)))
    (and target-path
	 (prog1
	     (find-file target-path)
	   (message "visited: %s" target-path)))))

(defun geben-session-source-read-file-name (session fileuri &optional disable-completion)
  (let* ((proc (geben-session-process session))
	 (listener (dbgp-listener-get proc))
	 (ip (format-network-address (dbgp-ip-get proc) t))
	 (local-path (geben-source-local-path-in-server session fileuri disable-completion))
	 target-path)
    (if (or (equal ip "127.0.0.1")
	    (and (fboundp 'network-interface-list)
		 (member ip (mapcar (lambda (addr)
				      (format-network-address (cdr addr) t))
				    (network-interface-list)))))
	;; local file
	(progn
	  (unless (file-regular-p local-path)
	    (while (not (file-regular-p (setq local-path
					      (read-file-name "Find local file: "
							      local-path local-path t ""))))
	      (beep)))
	  (setq target-path local-path))
      ;; remote file
      (condition-case nil
	  (if (fboundp 'geben-visit-remote-file)
	      (funcall geben-visit-remote-file session fileuri)
	    (let* ((storage (geben-session-storage session))
		   (path-prefix (or (plist-get storage :tramp)
				    (and (fboundp 'geben-get-tramp-spec-for)
					 (funcall 'geben-get-tramp-spec-for
						  (format "/%s:%s" ip local-path)))))
		   (find-file-default (if path-prefix
					  (concat path-prefix local-path)
					(format "/%s:%s" ip local-path))))
	      (setq target-path (read-file-name "Find remote file: "
						(file-name-directory find-file-default)
						find-file-default t
						(file-name-nondirectory find-file-default)))
	      (require 'tramp)
	      (when (tramp-tramp-file-p target-path)
		(plist-put storage :tramp (replace-regexp-in-string ":[^:]+$" ":" target-path)))))
	(quit (beep))))
    (and target-path
	 (geben-source-fileuri session target-path))))

(provide 'geben-source)
