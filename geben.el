;;; geben.el --- DBGp protocol frontend, a script debugger
;; $Id$
;; 
;; Filename: geben.el
;; Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: reedom <fujinaka.tohru@gmail.com>
;; Version: 0.20
;; URL: http://code.google.com/p/geben-on-emacs/
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo
;; Compatibility: Emacs 22.1
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; GEBEN is a software package that interfaces Emacs to DBGp protocol
;; with which you can debug running scripts interactive. At this present
;; DBGp protocol are supported in several script languages with help of
;; custom extensions.
;;
;;; Usage
;;
;; 1. Insert autoload hooks into your .Emacs file.
;;    -> (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)
;; 2. Start GEBEN. By default, M-x geben will start it.
;;    GEBEN starts to listening to DBGp protocol session connection.
;; 3. Run debuggee script.
;;    When the connection is established, GEBEN loads the entry script
;;    file in geben-mode.
;; 4. Start debugging. To see geben-mode key bindings, type ?.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Requirements:
;;
;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;; - Perl, Python, Ruby, Tcl with Komodo Debugger Extension
;;    http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
;;
;; [Client side]
;; - Emacs 22.1 and later
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
	    (string< emacs-version "22.1"))
    (error (concat "geben.el: This package requires Emacs 22.1 or later."))))

(eval-and-compile
  (require 'cl)
  (load-library "cl-macs")
  (require 'xml)
  (require 'tree-widget)
  (require 'dbgp))

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; For compatibility between versions of custom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	   ;; Some XEmacsen w/ custom don't have :set keyword.
	   ;; This protects them against custom.
	   (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
	nil
      (defmacro defgroup (&rest args)
	nil))
    (if (boundp 'defcustom)
	nil
      (defmacro defcustom (var value doc &rest args)
	`(defvar (,var) (,value) (,doc))))))

;; customize group

(defgroup geben nil
  "A PHP Debugging environment."
  :group 'debug)

(defgroup geben-highlighting-faces nil
  "Faces for GEBEN."
  :group 'geben
  :group 'font-lock-highlighting-faces)

;; geben session start/finish hooks

(defcustom geben-session-start-hook nil
  "*Hook running at when the geben debugging session is starting.
Each function is invoked with one argument, SESSION"
  :group 'geben
  :type 'hook)

(defcustom geben-session-finished-hook nil
  "*Hook running at when the geben debugging session is finished."
  :group 'geben
  :type 'hook)

;; file hooks

(defcustom geben-after-visit-hook 'geben-enter-geben-mode
  "*Hook running at when GEBEN visits a debuggee script file.
Each function is invoked with one argument, BUFFER."
  :group 'geben
  :type 'hook)

;; display window behavior

(defcustom geben-display-window-function 'pop-to-buffer
  "*Function to display a debuggee script's content.
Typically `pop-to-buffer' or `switch-to-buffer'."
  :group 'geben
  :type 'function)

(defun geben-dbgp-display-window (buf)
  "Display a buffer anywhere in a window, depends on the circumstance."
  (cond
   ((get-buffer-window buf)
    (select-window (get-buffer-window buf))
    (switch-to-buffer buf))
   ((or (eq 1 (count-windows))
	(not (geben-dbgp-dynamic-property-buffer-visiblep)))
    (funcall geben-display-window-function buf))
   (t
    (let ((candidates (make-vector 3 nil))
	  (dynamic-p (geben-dbgp-dynamic-property-bufferp buf)))
      (block finder
	     (walk-windows (lambda (window)
			     (if (geben-dbgp-dynamic-property-bufferp (window-buffer window))
				 (if dynamic-p
				     (unless (aref candidates 1)
				       (aset candidates 1 window)))
			       (if (eq (selected-window) window)
				   (aset candidates 2 window)
				 (aset candidates 0 window)
				 (return-from finder))))))
      (select-window (or (aref candidates 0)
			 (aref candidates 1)
			 (aref candidates 2)
			 (selected-window)))
      (switch-to-buffer buf))))
  buf)

;;==============================================================
;; DBGp
;;==============================================================

;;--------------------------------------------------------------
;; DBGp utilities
;;--------------------------------------------------------------

(defun geben-dbgp-decode-string (string data-encoding coding-system)
  "Decode encoded STRING."
  (when string
    (let ((s string))
      (when (consp s)
	(setq s (car s)))
      (when (stringp s)
	(setq s (cond
		 ((equal "base64" data-encoding)
		  (base64-decode-string s))
		 (t s)))
	(if coding-system
	    (decode-coding-string s coding-system)
	  s)))))

  
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
  (let ((features (or (geben-session-get session :features)
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
		   (geben-dbgp-cmd-sequence
		    (geben-dbgp-command-feature-get session name)
		    (nil
		     param))
		 (error "`geben-dbgp-feature-alist' has invalid entry: %S" entry))))))))

(defun geben-where ()
  "Move to the current breaking point."
  (interactive)
  (geben-dbgp-with-current-session session
    (if (geben-session-get session :stack)
	(let* ((stack (second (car (geben-session-get session :stack))))
	       (fileuri (geben-dbgp-regularize-fileuri (cdr (assq 'filename stack))))
	       (lineno (cdr (assq 'lineno stack))))
	  (geben-dbgp-indicate-current-line fileuri lineno t))
      (when (interactive-p)
	(message "GEBEN is not started.")))))

;;; command sending

(defun geben-dbgp-send-string (session string)
  (and (geben-session-active-p session)
       (dbgp-session-send-string session string t)))

(defun geben-send-raw-command (session fmt &rest arg)
  "Send a command string to a debugger engine.
The command string will be built up with FMT and ARG with a help of
the string formatter function `format'."
  (let ((cmd (apply #'format fmt arg)))
    (geben-dbgp-send-string cmd)))

(defun geben-dbgp-send-command (session operand &rest params)
  "Send a command to a debugger engine.
Return a cmd list."
  (if (geben-session-active-p session)
      (let ((cmd (geben-dbgp-cmd-make session operand params)))
	(geben-session-append session :cmd cmd)
	(unless (geben-session-get session :sending-p)
	  (geben-session-put session :sending-p t)
	  (geben-dbgp-process-command-queue session))
	cmd)))

(defun geben-dbgp-process-command-queue (session)
  (let ((cmd (car (geben-session-get session :cmd))))
    (if cmd
	(geben-dbgp-send-string session (geben-dbgp-cmd-expand cmd))
      (geben-session-put session :sending-p nil))))
  
;;;
;;; command/response handlers
;;;

;; step_into

(defun geben-dbgp-command-step-into (session)
  "Send \`step_into\' command."
  (geben-dbgp-send-command session "step_into"))

(defun geben-dbgp-response-step-into (session cmd msg)
  "A response message handler for \`step_into\' command."
  nil)

;; step_over

(defun geben-dbgp-command-step-over (session)
  "Send \`step_over\' command."
  (geben-dbgp-send-command session "step_over"))

(defun geben-dbgp-response-step-over (session cmd msg)
  "A response message handler for \`step_over\' command."
  nil)

;; step_out

(defun geben-dbgp-command-step-out (session)
  "Send \`step_out\' command."
  (geben-dbgp-send-command session "step_out"))

(defun geben-dbgp-response-step-out (session cmd msg)
  "A response message handler for \`step_out\' command."
  nil)

;; run

(defun geben-dbgp-command-run (session)
  "Send \`run\' command."
  (geben-dbgp-send-command session "run"))

(defun geben-dbgp-response-run (session cmd msg)
  "A response message handler for \`run\' command."
  nil)

;;; stop

(defun geben-dbgp-command-stop (session)
  "Send \`stop\' command."
  (geben-dbgp-send-command session "stop"))

(defun geben-dbgp-response-stop (session cmd msg)
  "A response message handler for \`stop\' command."
  nil)

;; context

(defun geben-dbgp-command-context-names (session &optional depth)
  (geben-dbgp-send-command session "context_names"
			   (and (numberp depth)
				(cons "-d" depth))))

(defun geben-dbgp-response-context-names (session cmd msg)
  (setq geben-dbgp-context-names-alist
	(mapcar (lambda (context)
		  (let ((name (xml-get-attribute context 'name))
			(id (xml-get-attribute context 'id)))
		    (cons name (string-to-number id))))
		(xml-get-children msg 'context))))

;;; breakpoint_set

(defun geben-dbgp-command-breakpoint-set (session bp)
  "Send \`breakpoint_set\' command."
  (if (not (geben-session-active-p session))
      (geben-dbgp-bp-add bp)
    (let ((obp (geben-dbgp-bp-find bp)))
      (if (and obp
	       (plist-get obp :id))
	  (geben-dbgp-send-command session "breakpoint_update"
				   (cons "-d" (plist-get obp :id))
				   (cons "-h" (or (plist-get bp :hit-value)
						  0))
				   (cons "-o" ">="))
	(let ((params
	       (remove nil
		       (list
			(cons "-t"
			      (substring (symbol-name (plist-get bp :type)) 1))
			(and (plist-get bp :fileuri)
			     (cons "-f" (plist-get bp :fileuri)))
			(and (plist-get bp :lineno)
			     (cons "-n" (plist-get bp :lineno)))
			(and (plist-get bp :class)
			     (geben-session-get session :xdebug-p)
			     (cons "-a" (plist-get bp :class)))
			(and (plist-get bp :function)
			     (if (and (geben-session-get session :xdebug-p)
				      (plist-get bp :method))
				 (cons "-m" (plist-get bp :method))
			       (cons "-m" (plist-get bp :function))))
			(and (plist-get bp :exception)
			     (cons "-x" (plist-get bp :exception)))
			(cons "-h" (or (plist-get bp :hit-value) 0))
			(cons "-o" ">=")
			(cons "-s" (or (plist-get bp :state)
				       "enabled"))
			(cons "-r" (or (plist-get bp :run-once)
				       0))
			(and (plist-get bp :expression)
			     (cons "--"
				   (base64-encode-string
				    (plist-get bp :expression))))))))
	  (when params
	    (apply 'geben-dbgp-send-command session "breakpoint_set" params)))))))

(defun geben-dbgp-response-breakpoint-set (session cmd msg)
  "A response message handler for \`breakpoint_set\' command."
  (let* ((type (intern (concat ":" (geben-dbgp-cmd-param-get cmd "-t"))))
	 (id (xml-get-attribute-or-nil msg 'id))
	 (fileuri (geben-dbgp-cmd-param-get cmd "-f"))
	 (lineno (geben-dbgp-cmd-param-get cmd "-n"))
	 (function (geben-dbgp-cmd-param-get cmd "-m"))
	 (class (geben-dbgp-cmd-param-get cmd "-a"))
	 (method function)
	 (exception (geben-dbgp-cmd-param-get cmd "-x"))
	 (expression (geben-dbgp-cmd-param-get cmd "--"))
	 (hit-value (geben-dbgp-cmd-param-get cmd "-h"))
	 (state (geben-dbgp-cmd-param-get cmd "-s"))
	 (local-path (and fileuri
			  (or (geben-session-local-path-of session fileuri)
			      (geben-temp-path-for-fileuri fileuri))))
	 bp)
    (when expression
      (setq expression (base64-decode-string expression)))
    (geben-dbgp-bp-add
     (setq bp (geben-dbgp-bp-make type
				  :id id
				  :fileuri fileuri
				  :lineno lineno
				  :class class
				  :method method
				  :function function
				  :exception exception
				  :expression expression
				  :hit-value hit-value
				  :local-path local-path
				  :state state))))
  (geben-dbgp-breakpoint-list-refresh))

(defun geben-dbgp-response-breakpoint-update (session cmd msg)
  "A response message handler for `breakpoint_update' command."
  (let* ((id (geben-dbgp-cmd-param-get cmd "-d"))
	 (bp (geben-dbgp-bp-find id)))
    (when bp
      (plist-put bp :hit-value (geben-dbgp-cmd-param-get cmd "-h"))
      (geben-dbgp-breakpoint-list-refresh))))

;;; breakpoint_remove

(defun geben-dbgp-command-breakpoint-remove (session &optional fileuri path lineno)
  "Send `breakpoint_remove' command."
  (setq path (or path
		 (buffer-file-name (current-buffer))))
  (when (stringp path)
    (setq lineno (or lineno
		     (and (get-file-buffer path)
			  (with-current-buffer (get-file-buffer path)
			    (geben-what-line)))))
    (setq fileuri (or fileuri
		      (geben-dbgp-find-fileuri path)
		      (concat "file://" (file-truename path))))
    (when (and fileuri lineno)
      (let* ((bp (geben-dbgp-bp-lineno-find fileuri lineno))
	     (bid (and bp (plist-get bp :id))))
	(when bp
	  (if (geben-session-active-p session)
	      (geben-dbgp-cmd-sequence
	       (geben-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
	       ((bid)
		(lambda (session cmd msg)
		  (when (dbgp-xml-get-error-message msg)
		    ;; remove a stray breakpoint from hash table.
		    (geben-dbgp-bp-remove bid)))))
	    (geben-dbgp-bp-remove bp)))))))

(defun geben-dbgp-response-breakpoint-remove (session cmd msg)
  "A response message handler for \`breakpoint_remove\' command."
  (let* ((id (geben-dbgp-cmd-param-get cmd "-d"))
	 (bp (geben-dbgp-bp-find id)))
    (geben-dbgp-bp-remove id)
    (geben-dbgp-breakpoint-list-refresh)))

(defun geben-dbgp-command-breakpoint-list (session)
  "Send `breakpoint_list' command."
  (geben-dbgp-send-command session "breakpoint_list"))

(defun geben-dbgp-response-breakpoint-list (session cmd msg)
  "A response message handler for \`breakpoint_list\' command."
  t)

;;; stack_get

(defun geben-dbgp-command-stack-get (session)
  "Send \`stack_get\' command."
  (geben-dbgp-send-command session "stack_get"))

(defun geben-dbgp-response-stack-get (session cmd msg)
  "A response message handler for \`stack_get\' command."
  (geben-session-put session :stack (xml-get-children msg 'stack))
  (let* ((stack (car-safe (geben-session-get session :stack)))
	 (fileuri (xml-get-attribute-or-nil stack 'filename))
	 (lineno (xml-get-attribute-or-nil stack'lineno)))
    (and fileuri lineno
	 (geben-dbgp-indicate-current-line fileuri lineno))))

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
	   
;;; source

(defun geben-dbgp-regularize-fileuri (session fileuri)
  ;; for bug of Xdebug 2.0.3 and below:
  (replace-regexp-in-string "%28[0-9]+%29%20:%20runtime-created%20function$" ""
			    fileuri))
  
(defun geben-dbgp-command-source (session fileuri)
  "Send source command.
FILEURI is a uri of the target file of a debuggee site."
  (geben-dbgp-send-command session "source" (cons "-f"
						  (geben-dbgp-regularize-fileuri fileuri))))


(defun geben-dbgp-response-source (session cmd msg)
  "A response message handler for \`source\' command."
  (let* ((fileuri (geben-dbgp-cmd-param-get cmd "-f"))
	 ;; (decode-coding-string (base64-decode-string (third msg)) 'undecided)))))
	 (local-path (geben-temp-path-for-fileuri fileuri)))
    (when local-path
      (geben-temp-store local-path (base64-decode-string (third msg)))
      (puthash fileuri (geben-dbgp-source-make fileuri t local-path) geben-dbgp-source-hash)
      (geben-visit-file local-path))))

(defun geben-dbgp-command-feature-get (session feature)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_get" (cons "-n" feature)))

(defun geben-dbgp-command-feature-set (session feature value)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_set"
			   (cons "-n" feature)
			   (cons "-v" (format "%S" (eval value)))))

;;; redirect

(defun geben-dbgp-command-stdout (session mode)
  "Send `stdout' command."
  (let ((m (plist-get '(nil 0 :disable 0 :redirect 1 :intercept 2) mode)))
    (when (and m)
      (geben-dbgp-send-command session "stdout" (cons "-c" m)))))

(defun geben-dbgp-response-stdout (session cmd msg)
  "A response message handler for `stdout' command."
  (setq geben-dbgp-redirect-stdout-current
	(case (geben-dbgp-cmd-param-get cmd "-c")
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
  (setq geben-dbgp-redirect-stderr-current
	(case (geben-dbgp-cmd-param-get cmd "-c")
	  (0 nil)
	  (1 :redirect)
	  (2 :intercept))))

;; context

(defun geben-dbgp-command-context-get (session context-id &optional depth)
  (geben-dbgp-send-command session "context_get"
			   (cons "-c" context-id)
			   (and depth
				(cons "-d" depth))))

(defun geben-dbgp-response-context-get (session cmd msg)
  t)

;; property

(defun geben-dbgp-command-property-get (session &rest args)
  (apply 'geben-dbgp-send-command session "property_get"
	 (mapcar (lambda (key)
		   (let ((arg (plist-get (car args) key)))
		     (when arg
		       (cons (geben-dbgp-cmd-param-for key) arg))))
		 '(:depth :context-id :name :max-data-size :type :page :key :address))))

(defun geben-dbgp-response-property-get (session cmd msg)
  t)

;;;

(defun geben-dbgp-fetch-source-file (session fileuri)
  "Fetch the content of FILEURI."
  (setq fileuri (geben-dbgp-regularize-fileuri fileuri))
  (unless (geben-session-local-path-of session fileuri)
    (let ((local-path (geben-temp-path-for-fileuri fileuri)))
      (or (file-exists-p local-path)
	  ;; haven't fetched remote source yet; fetch it.
	  (geben-dbgp-command-source fileuri)))))

(defun geben-dbgp-find-fileuri (source local-path)
  "Find fileuri for PATH."
  (block finder
    (maphash (lambda (fileuri source)
	       (when (string= (plist-get source :local-path) local-path)
		 (return-from finder fileuri)))
	     (geben-session-get session :source))))
	     
(defun geben-dbgp-get-fileuri-of (session local-path)
  (or (geben-dbgp-find-fileuri local-path)
      (let* ((temp-dir (geben-session-get session :temp-dir))
	     (temp-len (length temp-dir)))
	(concat "file://"
		(if (and (< temp-len (length local-path))
			 (string= temp-dir (substring local-path 0 temp-len)))
		    (substring local-path
			       (- temp-len
				  (if (string< "" (file-name-nondirectory temp-dir)) 0 1)))
		  local-path)))))

(defun geben-dbgp-find-file (path)
  "Visit debuggee file specified by PATH.
After visited it invokes `geben-after-visit-hook'."
  (let ((buffer (or (find-buffer-visiting path)
		    (and (file-exists-p path)
			 (find-file-noselect path)))))
    (when buffer
      (prog1
	  (geben-dbgp-display-window buffer)
	(run-hook-with-args 'geben-after-visit-hook buffer)))))

(defun geben-dbgp-massage-args (file args)
  args)

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
			       :sessoin-accept 'geben-dbgp-session-accept-p
			       :session-init 'geben-dbgp-session-init
			       :session-filter 'geben-dbgp-session-filter
			       :session-sentinel 'geben-dbgp-session-sentinel))
		   ((geben-proxy-project-p project)
		    (dbgp-proxy-register-exec (geben-proxy-project-proxy-addr project)
					      (geben-proxy-project-proxy-port project)
					      (geben-proxy-project-proxy-idekey project)
					      (geben-proxy-project-proxy-multi-session project)
					      :project project
					      :sessoin-accept 'geben-dbgp-session-accept-p
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
				      (geben-proxy-project-proxy-ip project)
				      (geben-proxy-project-proxy-port project)
				      (geben-proxy-project-proxy-idekey project)))
			    (second error-sexp))))
	   (beep)
	   (read-char msg nil 3)))))
    (if succeeded
	(message "Waiting for debug server to connect."))))

(defun geben-dbgp-session-accept-p (session)
  "Judge whether the SESSION is to be processed or to be terminated."
  (let ((project (geben-session-get session :project)))
    ;; accept session if the project of the session is:
    ;;  a. capable for multi sessions.
    ;;  b. not used yet; it's the first session for the project.
    (or (and (geben-proxy-project-p project)
	     (geben-proxy-project-proxy-multi-session project))
	(not (find-if (lambda (session)
			(and (processp session)
			     (eq 'open (process-status session))
			     (eq project (geben-session-get session :project))))
		      geben-sessions)))))
	
(defun geben-dbgp-session-init (session)
  "Initialize SESSION environment."
  (push session geben-sessions)
  (with-current-buffer (process-buffer session)
    (rename-buffer (geben-session-buffer-name session geben-process-buffer-name) t))
  (geben-session-init session))
  
(defun geben-dbgp-session-filter (session string)
  "Process DBGp response STRING.
Parse STRING, find xml chunks, convert them to xmlized lisp objects
and call `geben-dbgp-entry' with each chunk."
  (let (xml output)
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

(defcustom geben-dbgp-disconnect-hook nil
  "Hook running at when the session connection is disconnected."
  :type 'hook
  :group 'geben)

(defun geben-dbgp-session-sentinel (session string)
  (ignore-errors
    (run-hook-with-args geben-dbgp-disconnect-hook session))
  (when (buffer-live-p (process-buffer session))
    (with-current-buffer (process-buffer session)
      (insert "\nDisconnected.\n\n")
      (geben-dbgp-reset)))
  (setq geben-sessions (remq session geben-sessions)))

;;-------------------------------------------------------------
;;  miscellaneous functions
;;-------------------------------------------------------------

;; temporary directory

(defun geben-temp-path-for-fileuri (session fileuri)
  "Generate path string from FILEURI to store files temporarily."
  (let ((path (geben-make-local-path fileuri)))
    (if path
	(expand-file-name (substring path 1)
			  (geben-session-get session :temp-dir))
      fileuri)))

(defun geben-temp-store (path source)
  "Store temporary file."
  (make-directory (file-name-directory path) t)
  (ignore-errors
    (with-current-buffer (or (find-buffer-visiting path)
			     (create-file-buffer path))
      (widen)
      (erase-buffer)
      (font-lock-mode 0)
      (let ((encoding (detect-coding-string source t)))
	(unless (eq 'undecided encoding)
	  (set-buffer-file-coding-system encoding))
	(insert (decode-coding-string source encoding)))
      (with-temp-message ""
	(write-file path)
	(kill-buffer (current-buffer))))
    t))

;; path

(defun geben-make-local-path (fileuri)
  "Make a path string correspond to FILEURI."
  (when (string-match "^\\(file\\|https?\\):/+" fileuri)
    (let ((path (substring fileuri (1- (match-end 0)))))
      (setq path (or (and (eq system-type 'windows-nt)
			  (require 'url-util)
			  (url-unhex-string path))
		     path))
      (if (string= "" (file-name-nondirectory path))
	  (expand-file-name (geben-generate-default-file-name) path)
	path))))

(defun geben-generate-default-file-name ()
  (case (geben-session-get session :language)
    (:php "index.php")
    (:python "index.py")
    (:perl "index.pl")
    (:ruby "index.rb")
    (t "index.html")))

;; source code file

(defun geben-visit-file (path)
  "Visit to a local source code file."
  (when (file-exists-p path)
    (let ((buf (find-file-noselect path)))
      (geben-dbgp-display-window buf)
      (run-hook-with-args 'geben-after-visit-hook buf)
      buf)))

(defun geben-enter-geben-mode (buf)
  (geben-mode 1))

;;
;; interactive commands
;;

;;;###autoload
(defun geben (&optional quit)
  "Start GEBEN, a PHP source level debugger.
Prefixed with \\[universal-argument], GEBEN quits immediately.

GEBEN communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

The variable `geben-dbgp-command-line' is a command line to
execute a DBGp protocol client command. GEBEN communicates with
script servers through this command.

Once you've done these setup operation correctly, run GEBEN first
and your script on your script server second. After some
negotiation GEBEN will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page."
  (interactive "P")
  (if quit
      (let (buf proc)
	(and (setq buf (get-buffer geben-process-buffer-name))
	     (setq proc (get-buffer-process buf))
	     (dbgp-kill-process proc)))
    (geben-dbgp-start geben-projects)))

;;-------------------------------------------------------------
;;  geben-mode
;;-------------------------------------------------------------

(defvar geben-mode-map nil)
(unless geben-mode-map
  (setq geben-mode-map (make-sparse-keymap "geben"))
  ;; control
  (define-key geben-mode-map " " 'geben-step-again)
  (define-key geben-mode-map "g" 'geben-run)
  ;;(define-key geben-mode-map "G" 'geben-Go-nonstop-mode)
  (define-key geben-mode-map "t" 'geben-set-redirect)
  ;;(define-key geben-mode-map "T" 'geben-Trace-fast-mode)
  ;;(define-key geben-mode-map "c" 'geben-continue-mode)
  ;;(define-key geben-mode-map "C" 'geben-Continue-fast-mode)

  ;;(define-key geben-mode-map "f" 'geben-forward) not implemented
  ;;(define-key geben-mode-map "f" 'geben-forward-sexp)
  ;;(define-key geben-mode-map "h" 'geben-goto-here)

  ;;(define-key geben-mode-map "I" 'geben-instrument-callee)
  (define-key geben-mode-map "i" 'geben-step-into)
  (define-key geben-mode-map "o" 'geben-step-over)
  (define-key geben-mode-map "r" 'geben-step-out)

  ;; quitting and stopping
  (define-key geben-mode-map "q" 'geben-stop)
  ;;(define-key geben-mode-map "Q" 'geben-top-level-nonstop)
  ;;(define-key geben-mode-map "a" 'abort-recursive-edit)
  (define-key geben-mode-map "v" 'geben-display-context)

  ;; breakpoints
  (define-key geben-mode-map "b" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "B" 'geben-breakpoint-menu)
  (define-key geben-mode-map "u" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "\C-cb" 'geben-breakpoint-list)
  ;;(define-key geben-mode-map "B" 'geben-next-breakpoint)
  ;;(define-key geben-mode-map "x" 'geben-set-conditional-breakpoint)
  ;;(define-key geben-mode-map "X" 'geben-set-global-break-condition)

  ;; evaluation
  (define-key geben-mode-map "e" 'geben-eval-expression)
  ;;(define-key geben-mode-map "\C-x\C-e" 'geben-eval-last-sexp)
  ;;(define-key geben-mode-map "E" 'geben-visit-eval-list)

  ;; views
  (define-key geben-mode-map "w" 'geben-where)
  ;;(define-key geben-mode-map "v" 'geben-view-outside) ;; maybe obsolete??
  ;;(define-key geben-mode-map "p" 'geben-bounce-point)
  ;;(define-key geben-mode-map "P" 'geben-view-outside) ;; same as v
  ;;(define-key geben-mode-map "W" 'geben-toggle-save-windows)

  ;; misc
  (define-key geben-mode-map "?" 'geben-mode-help)
  (define-key geben-mode-map "d" 'geben-backtrace)

  ;;(define-key geben-mode-map "-" 'negative-argument)

  ;; statistics
  ;;(define-key geben-mode-map "=" 'geben-temp-display-freq-count)

  ;; GUD bindings
  (define-key geben-mode-map "\C-c\C-s" 'geben-step-into)
  (define-key geben-mode-map "\C-c\C-n" 'geben-step-over)
  (define-key geben-mode-map "\C-c\C-c" 'geben-run)

  (define-key geben-mode-map "\C-x " 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-d" 'geben-unset-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-t" 'geben-set-breakpoint-line)
  (define-key geben-mode-map "\C-c\C-l" 'geben-where))

;;;###autoload
(define-minor-mode geben-mode
  "Minor mode for debugging source code with GEBEN.
The geben-mode buffer commands:
\\{geben-mode-map}"
  nil " *debugging*" geben-mode-map
  (setq buffer-read-only geben-mode)
  (setq left-margin-width (if geben-mode 2 0))
  ;; when the buffer is visible in a window,
  ;; force the window to notice the margin modification
  (let ((win (get-buffer-window (current-buffer))))
    (if win
	(set-window-buffer win (current-buffer)))))
  
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (geben-dbgp-reset)
	    (mapc #'geben-session-delete-temp-dir geben-sessions)))

(defun geben-mode-help ()
  "Display description and key bindings of `geben-mode'."
  (interactive)
  (describe-function 'geben-mode))

(defvar geben-step-type :step-into
  "Step command of what `geben-step-again' acts.
This value remains the last step command type either
`:step-into' or `:step-out'.")

(defun geben-step-again ()
  "Do either `geben-step-into' or `geben-step-over' what the last time called.
Default is `geben-step-into'."
  (interactive)
  (case geben-step-type
    (:step-over (geben-step-over))
    (:step-into (geben-step-into))
    (t (geben-step-into))))
     
(defun geben-step-into ()
  "Step into the definition of the function or method about to be called.
If there is a function call involved it will break on the first
statement in that function"
  (interactive)
  (setq geben-step-type :step-into)
  (geben-dbgp-command-step-into))

(defun geben-step-over ()
  "Step over the definition of the function or method about to be called.
If there is a function call on the line from which the command
is issued then the debugger engine will stop at the statement
after the function call in the same scope as from where the
command was issued"
  (interactive)
  (setq geben-step-type :step-over)
  (geben-dbgp-command-step-over))

(defun geben-step-out ()
  "Step out of the current scope.
It breaks on the statement after returning from the current
function."
  (interactive)
  (geben-dbgp-command-step-out))

(defun geben-run ()
  "Start or resumes the script.
It will break at next breakpoint, or stops at the end of the script."
  (interactive)
  (geben-dbgp-command-run))

(defun geben-stop ()
  "End execution of the script immediately."
  (interactive)
  (geben-dbgp-command-stop))

(defun geben-breakpoint-menu (arg)
  "Set a breakpoint interactively.
Script debugger engine may support a kind of breakpoints, which
will be stored in the variable `geben-dbgp-breakpoint-types'
after a debugging session is started.

This command asks you a breakpoint type and its options.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-breakpoint-menu] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-breakpoint-menu]), \
this command will also ask a
hit-value interactively.
"
  (interactive "P")
  (let ((candidates (remove nil
			    (mapcar
			     (lambda (x)
			       (if (member (car x) geben-dbgp-breakpoint-types) x))
			     '((:line . "l)Line")
			       (:call . "c)Call")
			       (:return . "r)Return")
			       (:exception . "e)Exception")
			       (:conditional . "d)Conditional")
			       (:watch . "w)Watch"))))))
    (when (null candidates)
      (error "No breakpoint type is supported by the debugger engine."))
    (let* ((c (read-char (concat "Breakpoint type: "
				 (mapconcat
				  (lambda (x)
				    (cdr x))
				  candidates " "))))
	   (x (find-if (lambda (x)
			 (eq c (elt (cdr x) 0)))
		       candidates))
	   (fn (and x
		    (intern-soft (concat "geben-set-breakpoint-"
					 (substring (symbol-name (car x)) 1))))))
      (unless x
	(error "Cancelled"))
      (if (fboundp fn)
	  (call-interactively fn)
	(error (concat (symbol-name fn) " is not implemented."))))))

(defun geben-set-breakpoint-common (hit-value cmd)
  (setq hit-value (if (and (not (null hit-value))
			   (listp hit-value))
		      (if (fboundp 'read-number)
			  (read-number "Number of hit to break: ")
			(string-to-number
			(read-string "Number of hit to break: ")))
		    hit-value))
  (plist-put cmd :hit-value (if (and (numberp hit-value)
				     (<= 0 hit-value))
				hit-value
			      0))
  (geben-dbgp-command-breakpoint-set cmd))

(defun geben-set-breakpoint-line (fileuri lineno &optional hit-value)
  "Set a breakpoint at the current line.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-line] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-line]), \
this command will also ask a
hit-value interactively."
  (interactive (list nil nil current-prefix-arg))
  (let ((local-path (if fileuri
  (geben-session-local-path-of session fileuri)
  (buffer-file-name (current-buffer)))))
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make
				  :line
				  :fileuri (or fileuri
				  (geben-dbgp-find-fileuri local-path)
				  (geben-dbgp-find-fileuri (file-truename local-path))
				  (geben-dbgp-get-fileuri-of (file-truename local-path)))
				  :lineno (if (numberp lineno)
				  lineno
				  (geben-what-line))
				  :local-path local-path
				  :overlay t))))

(defvar geben-set-breakpoint-call-history nil)
(defvar geben-set-breakpoint-fileuri-history nil)
(defvar geben-set-breakpoint-exception-history nil)
(defvar geben-set-breakpoint-condition-history nil)

(defun geben-set-breakpoint-call (name &optional fileuri hit-value)
  "Set a breakpoint to break at when entering function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-call] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-call]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Name: " ""
			     'geben-set-breakpoint-call-history)
		(unless (member (geben-session-get session :language) '(:php :ruby))
		  ;; at this present some debugger engines' implementation is buggy:
		  ;; some requires fileuri and some don't accept it.
		  (read-string "fileuri: "
		  (geben-dbgp-get-fileuri-of
		   (file-truename (buffer-file-name (current-buffer))))
		  'geben-set-breakpoint-fileuri-history))
		current-prefix-arg))
  (when (string< "" name)
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make :call
						     :function name
						     :fileuri fileuri))))

(defun geben-set-breakpoint-return (name &optional fileuri hit-value)
  "Set a breakpoint to break after returned from a function/method named NAME.
For a class method, specify NAME like \"MyClass::MyMethod\".
For an instance method, do either like \"MyClass::MyMethod\" or
\"MyClass->MyMethod\".
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-return] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-return]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Name: " ""
			     'geben-set-breakpoint-call-history)
		(unless (member (geben-session-get session :language) '(:php :ruby))
		  ;; at this present some debugger engines' implementations are buggy:
		  ;; some requires fileuri and some don't accept it.
		  (read-string "fileuri: "
		  (geben-dbgp-get-fileuri-of
		   (file-truename (buffer-file-name (current-buffer))))
		  'geben-set-breakpoint-fileuri-history))
		current-prefix-arg))
  (when (string< "" name)
    (geben-set-breakpoint-common hit-value
				 (geben-dbgp-bp-make :return
						     :function name
						     :fileuri fileuri))))

(defun geben-set-breakpoint-exception (name &optional hit-value)
  "Set a breakpoint to break at when an exception named NAME is occurred.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-exception] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-exception]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Exception type: "
			     "Exception"
			     'geben-set-breakpoint-exception-history)
		current-prefix-arg))
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :exception
						   :exception name)))
   
(defun geben-set-breakpoint-conditional (expr fileuri &optional lineno hit-value)
  "Set a breakpoint to break at when the expression EXPR is true in the file FILEURI.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Expression: " ""
			     'geben-set-breakpoint-condition-history)
		(geben-dbgp-get-fileuri-of
		 (file-truename (buffer-file-name (current-buffer))))
		(read-string "Line number to evaluate (blank means entire file): "
			     (number-to-string (geben-what-line)))
		current-prefix-arg))
  
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :conditional
						   :expression expr
						   :fileuri fileuri
						   :lineno (and (stringp lineno)
								(string-match "^[0-9]+$" lineno)
								(string-to-number lineno)))))

(defun geben-set-breakpoint-watch (expr &optional hit-value)
  "Set a breakpoint to break on write of the variable or address.
Optionally, with a numeric argument you can specify `hit-value'
\(number of hits to break); \\[universal-argument] 2 \
\\<geben-mode-map>\\[geben-set-breakpoint-conditional] will set a breakpoint
with 2 hit-value.
With just a prefix arg \(\\[universal-argument] \\[geben-set-breakpoint-conditional]),
this command will also ask a
hit-value interactively."
  (interactive (list
		(read-string "Expression: " ""
			     'geben-set-breakpoint-condition-history)
		current-prefix-arg))
  (geben-set-breakpoint-common hit-value
			       (geben-dbgp-bp-make :watch
						   :expression expr)))

(defun geben-unset-breakpoint-line ()
  "Clear a breakpoint set at the current line."
  (interactive)
  (geben-dbgp-command-breakpoint-remove))

(defun geben-breakpoint-list ()
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-dbgp-breakpoint-list t))

(defvar geben-eval-history nil)

(defun geben-eval-expression (expr)
  "Evaluate a given string EXPR within the current execution context."
  (interactive
   (progn
     (list (read-from-minibuffer "Eval: "
     nil nil nil 'geben-eval-history))))
  (geben-dbgp-command-eval expr))

(defun geben-open-file (fileuri)
  "Open a debugger server side file specified by FILEURI.
FILEURI forms like as \`file:///path/to/file\'."
  (interactive (list (read-string "Open file: " "file://")))
  (geben-dbgp-command-source fileuri))

(defun geben-backtrace ()
  "Display backtrace list.
The backtrace list buffer is under `geben-backtrace-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-dbgp-backtrace))

(defun geben-set-redirect (target &optional arg)
  "Set the debuggee script's output redirection mode.
This command enables you to redirect the debuggee script's output to GEBEN.
You can select redirection target from \`stdout', \`stderr' and both of them.
Prefixed with \\[universal-argument], you can also select redirection mode
from \`redirect', \`intercept' and \`disabled'."
  (interactive (list (case (read-char "Redirect: o)STDOUT e)STRERR b)Both")
		       (?o :stdout)
		       (?e :stderr)
		       (?b :both))
		     current-prefix-arg))
  (unless target
    (error "Cancelled"))
  (let ((mode (if arg
  (case (read-char "Mode: r)Redirect i)Intercept d)Disable")
    (?r :redirect)
    (?i :intercept)
    (?d :disable))
  :redirect)))
    (unless mode
      (error "Cancelled"))
    (when (memq target '(:stdout :both))
      (geben-dbgp-command-stdout mode))
    (when (memq target '(:stderr :both))
      (geben-dbgp-command-stderr mode))))

(defun geben-display-context (&optional depth)
  (interactive (list (cond
		      ((null current-prefix-arg) 0)
		      ((numberp current-prefix-arg)
		      current-prefix-arg)
		      ((listp current-prefix-arg)
		      (if (fboundp 'read-number)
			  (read-number "Depth: " 0)
			(string-to-number (read-string "Depth: " "0"))))
		      (t nil))))
  (geben-dbgp-context-display (or depth 0)))

(provide 'geben)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; geben.el ends here
;;
;;(defmacro geben-defcmdseq nil t)
;;
;;(font-lock-add-keywords
;; 'emacs-lisp-mode
;; '(("\\<geben-defcmdseq\\>" 0 font-lock-keyword-face)))
;;
;;(defmacro geben-with-commands (&rest body)
;;  (declare (indent defun)))
;;
;;(geben-with-commands
;;  (geben-dbgp-seq-init-source)
;;  (geben-dbgp-response-source))
;;
;;
;;(defsubst geben-cmd-bind (cmd exec-status cmd1)
;;  (geben-plist-add cmd :sequence (list exec-status cmd1)))
;;
;;(defun geben-dbgp-seq-init-source (cmd msg)
;;  (let ((fileuri (xml-get-attribute msg 'fileuri)))
;;    (geben-dbgp-command-source fileuri)))
;;
;;(defsubst geben-bp-correct-line (bp)
;;  "Compensate line position of breakpoint BP.
;;User may edit code at any time, so the line position of
;;breakpoint can be mismatch between indicator(overlay) and numeric
;;setting in BP.
;;This function will try to adjust breakpoint line to nearly what
;;user expect."
;;  (let ((overlay (plist-get bp :overlay)))
;;    (if (and (overlayp overlay)
;;	     (eq (overlay-buffer overlay)
;;		 (find-buffer-visiting (or (plist-get bp :local-path) ""))))
;;	(with-current-buffer (overlay-buffer overlay)
;;	  (plist-put bp :lineno (progn
;;				  (goto-char (overlay-start overlay))
;;				  (geben-what-line)))))))
;;
;;(defsubst geben-cmdseq-push (session condition call recv)
;;  (geben-session-push session :cmdseq (list :condition condition
;;						  :call call
;;						  :recv recv)))
;;  
;;(defun geben-dbgp-seq-restore-breakpoints (cmd msg)
;;  "Restore breakpoints against new DBGp session."
;;  (let ((session (plist-get cmd :session)))
;;    (mapc (lambda (bp)
;;	    (plist-put bp :id nil)
;;	    (geben-bp-correct-line bp)
;;	    (geben-cmdseq-push session
;;			       :any
;;			       (lexical-let ((bp bp))
;;				 (lambda ()
;;				   (geben-dbgp-command-breakpoint-set bp)))
;;			       'geben-dbgp-command-breakpoint-set))
;;	    geben-dbgp-breakpoints)))
;;
;;(geben-dbgp-bind-commands
;;  '(init (:any geben-dbgp-seq-init-source
;;	       geben-dbgp-response-source)
;;	 (:any geben-dbgp-seq-restore-breakpoints)
;;	 (:any geben-dbgp-seq-init-features)
;;	 (:any geben-dbgp-seq-init-context-names
;;	       geben-dbgp-response-context-names)
;;	 (:any geben-dbgp-seq-move-to-entry-point)))
