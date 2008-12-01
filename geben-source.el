;;--------------------------------------------------------------
;; source hash
;;--------------------------------------------------------------

(require 'cl)
(require 'geben-common)
(require 'geben-session)
(require 'geben-cmd)
(require 'geben-dbgp)

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

(defmacro geben-source-make (fileuri local-path)
  "Create a new source object.
A source object forms a property list with three properties
:fileuri, :remotep and :local-path."
  `(list :fileuri ,fileuri :local-path ,local-path))

(defvar geben-source-release-hook nil)

(defun geben-source-release (source)
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
  (let* ((tempdir (geben-session-tempdir session))
	 (templen (length tempdir)))
    (concat "file://"
	    (if (and (< templen (length local-path))
		     (string= tempdir (substring local-path 0 templen)))
		(substring local-path
			   (- templen
			      (if (string< "" (file-name-nondirectory tempdir)) 0 1)))
	      local-path))))

(defun geben-source-local-path (session fileuri)
  "Generate path string from FILEURI to store files temporarily."
  (let ((local-path (geben-source-local-path-in-server session fileuri)))
    (when local-path
      (expand-file-name (substring local-path 1)
			(geben-session-tempdir session)))))

(defun geben-source-local-path-in-server (session fileuri)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (setq path (or (and (eq system-type 'windows-nt)
			  (require 'url-util)
			  (url-unhex-string path))
		     path))
      (if (string= "" (file-name-nondirectory path))
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

(defun geben-source-write-file (path content)
  "Write CONTENT to file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
			     (create-file-buffer path))
      (let ((inhibit-read-only t))
	(buffer-disable-undo)
	(widen)
	(erase-buffer)
	(font-lock-mode 0)
	(let ((encoding (detect-coding-string content t)))
	  (unless (eq 'undecided encoding)
	    (set-buffer-file-coding-system encoding))
	  (insert (decode-coding-string content encoding))))
      (with-temp-message ""
	(write-file path)
	(kill-buffer (current-buffer))))
    t))

(defun geben-source-visit (local-path)
  "Visit to a local source code file."
  (when (file-exists-p local-path)
    (let ((buf (find-file-noselect local-path)))
      (geben-dbgp-display-window buf)
      (run-hook-with-args 'geben-source-visit-hook buf)
      buf)))

;; session

(defun geben-session-source-init (session)
  (setf (geben-session-source session) (make-hash-table :test 'equal)))

(add-hook 'geben-session-enter-hook #'geben-session-source-init)

(defun geben-session-source-add (session fileuri local-path content)
  (let ((tempdir (geben-session-tempdir session)))
    (unless (file-directory-p tempdir)
      (make-directory tempdir t)
      (set-file-modes tempdir 1023)))
  (geben-source-write-file local-path content)
  (puthash fileuri (geben-source-make fileuri local-path) (geben-session-source session)))

(defun geben-session-source-release (session)
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
  (plist-get (gethash fileuri (geben-session-source session))
	     :local-path))

(defsubst geben-session-source-fileuri (session local-path)
  (block geben-session-souce-fileuri
    (maphash (lambda (fileuri path)
	       (and (equal local-path path)
		    (return-from geben-session-souce-fileuri fileuri))))))

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

(provide 'geben-source)
