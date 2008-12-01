(require 'geben-dbgp)
(require 'geben-source)

;;--------------------------------------------------------------
;; DBGp connected session initialization
;;--------------------------------------------------------------

(defun geben-dbgp-init-fetch-entry-source (session)
  "Fetch the content of the entry source file."
  (let ((fileuri (xml-get-attribute-or-nil (geben-session-initmsg session) 'fileuri)))
    (when fileuri
      (geben-dbgp-command-source session fileuri))))

(defun geben-dbgp-init-proceed-to-first-line (session)
  ""
  (geben-dbgp-sequence
      (geben-dbgp-send-command session "status")
    (lambda (session cmd msg err)
      (unless err
	(if (equal "break" (xml-get-attribute msg 'status))
	    ;; it is nonconforming to DBGp specs; anyway manage it.
	    nil
	  (geben-dbgp-command-step-into session))))))

;; features

(defcustom geben-dbgp-feature-list
  '((:set max_data 32768)
    (:set max_depth 1)
    (:set max_children 32)
    (:get breakpoint_types geben-dbgp-breakpoint-store-types))
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
  (let ((features (or (geben-session-feature session)
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
	       (condition-case error-sexp
		   (if (and (symbolp param)
			    (fboundp param))
		       (geben-dbgp-sequence
			   (geben-dbgp-command-feature-get session name)
			 param))
		 (error
		  (warn "`geben-dbgp-feature-alist' has invalid entry: %S" entry)))))))))

;; feature

(defun geben-dbgp-command-feature-get (session feature)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_get" (cons "-n" feature)))

(defun geben-dbgp-command-feature-set (session feature value)
  "Send \`feature_get\' command."
  (geben-dbgp-send-command session "feature_set"
			   (cons "-n" feature)
			   (cons "-v" (format "%S" (eval value)))))

(add-hook 'geben-dbgp-init-hook 'geben-dbgp-init-fetch-entry-source t)
(add-hook 'geben-dbgp-init-hook 'geben-dbgp-feature-init t)
(add-hook 'geben-dbgp-init-hook 'geben-dbgp-redirect-init t)
(add-hook 'geben-dbgp-init-hook 'geben-dbgp-command-context-names t)
(add-hook 'geben-dbgp-init-hook 'geben-dbgp-breakpoint-restore t)
(add-hook 'geben-dbgp-init-hook 'geben-dbgp-init-proceed-to-first-line t)

(provide 'geben-dbgp-init)
