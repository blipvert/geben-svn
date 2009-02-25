(require 'cl)
(require 'geben-session)
(require 'geben-dbgp-util)
(require 'geben-cmd)
(require 'geben-dbgp)


;;==============================================================
;; context
;;==============================================================

(defface geben-context-category-face
  '((((class color))
     :background "purple"
     :foreground "white"
     :bold t))
  "Face used to highlight context category name."
  :group 'geben-highlighting-faces)

(defface geben-context-variable-face
  '((t :inherit 'font-lock-variable-name-face))
  "Face used to highlight variable name."
  :group 'geben-highlighting-faces)

(defface geben-context-type-face
  '((t :inherit 'font-lock-type-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)
  
(defface geben-context-class-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight type name."
  :group 'geben-highlighting-faces)
  
(defface geben-context-string-face
  '((t :inherit 'font-lock-string-face))
  "Face used to highlight string value."
  :group 'geben-highlighting-faces)
  
(defface geben-context-constant-face
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight numeric value."
  :group 'geben-highlighting-faces)

(defstruct (geben-context
	    (:constructor nil)
	    (:constructor geben-context-make))
  names	   ; context names alist(KEY: context name, VALUE: context id)
  tid  ; transaction id to which the current context variables belong.
  variables			;
  expanded-variables		; context variables in expanded state.
  (depth 0)
  )

(defvar geben-context-where "")
(defvar geben-context-loading nil)
(defvar geben-context-property-tree-fill-children-hook 'geben-context-tree-children-fill)

(defun geben-session-context-init (session)
  (setf (geben-session-context session) (geben-context-make)))
(add-hook 'geben-session-enter-hook #'geben-session-context-init)

;; context list buffer

(defsubst geben-session-context-buffer (session)
  (let ((buf (geben-session-buffer session geben-context-buffer-name)))
    (with-current-buffer buf
      (geben-context-mode session))
    buf))

(defsubst geben-session-context-buffer-get (session)
  (geben-session-buffer-get session geben-context-buffer-name))

(defsubst geben-session-context-buffer-live-p (session)
  (geben-session-buffer-live-p session geben-context-buffer-name))

(defsubst geben-session-context-buffer-visible-p (session)
  (geben-session-buffer-visible-p session geben-context-buffer-name))

;;

(defsubst geben-session-context-tid (session)
  (geben-context-tid (geben-session-context session)))

(defsubst geben-session-context-names (session)
  (geben-context-names (geben-session-context session)))

(defsubst geben-session-context-depth (session)
  (geben-context-depth (geben-session-context session)))

;; context list accessors

(defsubst geben-session-context-list (session cid)
  "Get context list for the context id CID."
  (assq cid
	(geben-context-variables
	 (geben-session-context session))))

(defsubst geben-session-context-list-old (session cid)
  "Get previous context list for the context id CID."
  (cdr (assq 'old (geben-session-context-list session cid))))

(defsubst geben-session-context-list-new (session cid)
  "Get the current context list for the context id CID."
  (cdr (assq 'new (geben-session-context-list session cid))))

(defsubst geben-session-context-list-update (session cid list)
  "Update the current context list for the context id CID with LIST."
  (let* ((clist (geben-session-context-list session cid))
	 (old (assq 'new clist)))
    (setcdr clist (list (cons 'old (cdr old))
			(cons 'new list)))))

;; context property list accessors

(defsubst geben-context-property-has-children (property)
  "Check whether PROPERTY has any children."
  (equal "1" (xml-get-attribute-or-nil property 'children)))

(defsubst geben-context-property-format-bool (value)
  "Format VALUE in the debuggee language expression."
  (let ((bool (if (equal "0" value) nil t)))
    (if bool "true" "false")))

(defsubst geben-context-property-format-array-name (property)
  "Format array element name in the debuggee language expression."
  (format "%s[%s]"
	  (propertize (xml-get-attribute property 'name)
		      'face 'geben-context-variable-face)
	  (propertize (xml-get-attribute property 'numchildren)
		      'face 'geben-context-constant-face)))

(defsubst geben-context-property-attribute (property sym)
  "Get attribute SYM from PROPERTY."
  ;; DBGp specs specifies property attributes of context_get and
  ;; property_get commands. But some debugger engines have values not
  ;; as attributes but child elements."
  (let ((node (car (xml-get-children property sym))))
    (if (consp node)
	(geben-dbgp-decode-string (xml-node-children node)
				  (xml-get-attribute node 'encoding)
				  'utf-8)
      (xml-get-attribute property sym))))

(defsubst geben-context-property-name (property)
  "Get name attribute value from PROPERTY."
  (geben-context-property-attribute property 'name))
	
(defsubst geben-context-property-fullname (property)
  "Get fullname attribute value from PROPERTY."
  (geben-context-property-attribute property 'fullname))

(defsubst geben-context-property-value (property)
  "Get value from PROPERTY."
  (let ((node (car (xml-get-children property 'value))))
    (if (consp node)
	(geben-dbgp-decode-string (xml-node-children node)
				  (xml-get-attribute node 'encoding)
				  'utf-8)
      (geben-dbgp-decode-string (xml-node-children property)
				(xml-get-attribute property 'encoding)
				'utf-8))))

(defun geben-context-property-typeinfo (property)
  "Get type information of PROPERTY to display it in the context buffer."
  (let ((type (and (xml-get-attribute-or-nil property 'type)
		   (intern (xml-get-attribute-or-nil property 'type))))
	typeinfo)
    (setq typeinfo
	  (cond
	   ((null type) nil)
	   ((member type '(int float))
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face))
	   ((eq type 'bool)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face
		  :value-formatter 'geben-context-property-format-bool))
	   ((eq type 'string)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-string-face))
	   ((member type '(array hash))
	    (list :type type
		  :type-visiblep nil
		  :name-formatter 'geben-context-property-format-array-name
		  :value-face 'default
		  :value-formatter (lambda (value) "")))
	   ((eq type 'null)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-constant-face
		  :value-formatter (lambda (value) "null")))
	   ((eq type 'resource)
	    (list :type type
		  :type-visiblep t
		  :value-face 'geben-context-constant-face))
	   ((eq type 'object)
	    (list :type (if (xml-get-attribute-or-nil property 'classname)
			    (intern (xml-get-attribute-or-nil property 'classname))
			  type)
		  :type-visiblep t
		  :type-face 'geben-context-class-face
		  :value-face 'default))
	   ((eq type 'uninitialized)
	    (list :type 'undef
		  :type-visiblep t
		  :type-face 'geben-context-type-face
		  :value-face 'default))
	   (t
	    (list :type type
		  :type-visiblep t
		  :type-face 'geben-context-type-face
		  :value-face 'default))))
    typeinfo))

;;--------------------------------------------------------------
;; context property tree widget
;;--------------------------------------------------------------

(defun geben-context-property-tree-open (tree)
  "Expand TREE."
  (let ((marker (widget-get tree :from)))
    (when (markerp marker)
      (with-current-buffer (marker-buffer marker)
	(goto-char marker)
	(call-interactively 'widget-button-press)
	(unless (widget-get tree :open)
	  (call-interactively 'widget-button-press))))))

(defun geben-context-property-tree-expand-p (tree)
  "A tree widget callback function to indicate whether TREE is able to expand."
  (or (geben-context-property-tree-has-complete-children tree)
      (and (run-hook-with-args 'geben-context-property-tree-fill-children-hook
			       tree)
	   nil)))

(defun geben-context-property-tree-expand (tree)
  "A tree widget callback function to create child list of TREE."
  (mapcar #'geben-context-property-tree-create-node
	  (xml-get-children (widget-get tree :property) 'property)))

(defun geben-context-property-tree-has-complete-children (tree)
  "Determine whether TREE has complete child nodes.
Child nodes can be short for :property property of TREE."
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property))
	 (numchildren (and children
			   (string-to-number (xml-get-attribute property 'numchildren)))))
    (and children
	 (<= numchildren (length children)))))

(defun geben-context-property-tree-create-node (property)
  "Create nodes which represent PROPERTY."
  (let* ((typeinfo (geben-context-property-typeinfo property))
	 (value (geben-context-property-value property))
	 tag)
    (let ((formatter (plist-get typeinfo :name-formatter)))
      (setq tag 
	    (if formatter
		(funcall formatter property)
	      (propertize (geben-context-property-name property)
			  'face 'geben-context-variable-face))))
    (when (plist-get typeinfo :type-visiblep)
      (setq tag (concat tag
			(format "(%s)" (propertize
					(symbol-name (plist-get typeinfo :type))
					'face (plist-get typeinfo :type-face))))))
    (let ((formatter (plist-get typeinfo :value-formatter)))
      (when (or value formatter)
	(setq tag (format "%-32s %s" tag
			  (propertize (if formatter
					  (funcall formatter value)
					value)
				      'face (plist-get typeinfo :value-face))))))
    (if (geben-context-property-has-children property)
	(list 'tree-widget
	      :tag tag
	      :property property
	      :expander 'geben-context-property-tree-expand
	      :expander-p 'geben-context-property-tree-expand-p)
      (list 'item :tag (concat "   " tag)))))
  
(defun geben-context-property-tree-context-id (tree)
  "Get context id to which TREE belongs."
  (when tree
    (let ((cid (widget-get tree :context-id)))
      (or cid
	  (geben-context-property-tree-context-id (widget-get tree :parent))))))

;;--------------------------------------------------------------
;; context functions
;;--------------------------------------------------------------

(defun geben-context-list-fetch (session callback)
  "Fetch context variables for a SESSION from debuggee server.
After fetching it calls CALLBACK function."
  (let ((context (geben-session-context session)))
    (when (geben-context-names context)
      (unless (geben-context-variables context)
	(setf (geben-context-variables context)
	      (mapcar (lambda (context)
			(list (cdr context)))
		      (geben-context-names context))))
      ;; Remain the current tid.
      ;; It is possible that the current context proceeds by step_in or
      ;; other continuous commands while retrieving variables.
      ;; To avoid mixing variables with multi context, remain something at here,
      ;; tid, and check the value in the retrieving process.
      (setf (geben-context-tid context) (geben-session-tid session))
      (geben-context-list-fetch-loop session
				     (geben-context-tid context)
				     (geben-context-depth context)
				     (mapcar (lambda (context)
					       (cdr context))
					     (geben-context-names context))
				     callback))))

(defun geben-context-list-fetch-loop (session tid-save depth context-id-list callback)
  (let ((buf (geben-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
	(setq geben-context-loading t))
      (geben-dbgp-sequence-bind (tid-save depth context-id-list callback)
	(geben-dbgp-command-context-get session (car context-id-list) depth)
	(lambda (session cmd msg err)
	  (when (and (not err)
		     (eq tid-save (geben-session-context-tid session))
		     (geben-session-context-buffer-live-p session))
	    (geben-session-context-list-update session
					       (geben-cmd-param-get cmd "-c")
					       (xml-get-children msg 'property))
	    (if (cdr context-id-list)
		(geben-context-list-fetch-loop session tid-save depth
					       (cdr context-id-list) callback)
	      (geben-context-fill-buffer session)
	      (with-current-buffer (geben-session-context-buffer-get session)
		(setq geben-context-loading nil))
	      (funcall callback session))))))))

(defun geben-context-fill-buffer (session)
  "Fill the context buffer with locally stored context list."
  (let ((buf (geben-session-context-buffer-get session)))
    (when buf
      (with-current-buffer buf
	(let ((inhibit-read-only t)
	      (inhibit-modification-hooks t))
	  (widen)
	  (erase-buffer)
	  (dolist (context-name (geben-session-context-names session))
	    (let ((new (geben-session-context-list-new session (cdr context-name))))
	      (apply 'widget-create
		     'tree-widget
		     :tag (car context-name)
		     :context-id (cdr context-name)
		     :open t
		     (mapcar #'geben-context-property-tree-create-node new))))
	  (widget-setup))
	(goto-char (point-min))))))

(defun geben-context-tree-children-fill (tree &optional tid-save)
  (geben-with-current-session session
    (let ((tid-save (or tid-save
			(geben-session-context-tid session)))
	  (completed (geben-context-property-tree-has-complete-children tree))
	  (buf (geben-session-context-buffer-get session)))
      (when (and (buffer-live-p buf)
		 (eq tid-save (geben-session-context-tid session)))
	(with-current-buffer buf
	    (setq geben-context-loading (not completed)))
	(if completed
	    (geben-context-property-tree-open tree)
	  (geben-context-tree-children-fill-1 session tree tid-save))))))

(defun geben-context-tree-children-fill-1 (session tree tid-save)
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property)))
    (with-current-buffer (geben-session-context-buffer-get session)
      ;; -- comment on :property-page property --
      ;; debugger engine may lack of PAGESIZE in property message(bug).
      ;; so the following code doesn't rely on PAGESIZE but uses own
      ;; :property-page widget property.
      (let* ((nextpage (if (widget-get tree :property-page)
			   (1+ (widget-get tree :property-page))
			 (if children 1 0)))
	     (args (list :depth (geben-session-context-depth session)
			 :context-id (geben-context-property-tree-context-id tree)
			 :name (geben-context-property-fullname property)
			 :page nextpage)))
	(widget-put tree :property-page nextpage)
	(when (xml-get-attribute-or-nil property 'key)
	  (plist-put args :key (xml-get-attribute-or-nil property 'key)))
	(geben-dbgp-sequence-bind (tree tid-save)
	  (geben-dbgp-command-property-get session args)
	  (lambda (session cmd msg err)
	    (unless err
	      (geben-context-tree-children-append session
						  tid-save
						  tree
						  (car (xml-get-children msg 'property)))
	      (geben-context-tree-children-fill tree
						tid-save))))))))

(defun geben-context-tree-children-append (session tid-save tree property)
  (if (eq tid-save (geben-session-context-tid session))
      (let ((tree-prop (widget-get tree :property)))
	(nconc (or (cddr tree-prop)
		   tree-prop)
	       (cddr property)))))

(defun geben-context-list-refresh (session depth &optional force)
  (when (and (geben-session-active-p session)
	     (or force
		 (geben-session-context-buffer-visible-p session)))
    (geben-context-list-display session depth (not force))))
  
(defun geben-context-list-display (session depth &optional no-select)
  "Display context variables in the context buffer."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (when (or (< depth 0)
	    (< (length (geben-session-stack session)) (1+ depth)))
    (error "GEBEN context display: invalid depth: %S" depth))
  (setf (geben-context-depth (geben-session-context session)) depth)
  (let ((buf (geben-session-context-buffer session)))
    (with-current-buffer buf
	(setq geben-context-where
	      (xml-get-attribute (nth depth (geben-session-stack session))
				 'where)))
    (unless no-select
      (geben-dbgp-display-window buf))
    (geben-context-list-fetch session
			      (geben-lexical-bind (buf no-select)
				(lambda (session)
				  (and (buffer-live-p buf)
				       (not no-select)
				       (geben-dbgp-display-window buf)))))))

;;--------------------------------------------------------------
;; context mode
;;--------------------------------------------------------------

(defcustom geben-context-mode-hook nil
  "*Hook running at when GEBEN's context buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'widget-forward)
    (define-key map "S-\t" 'widget-backward)
    ;;(define-key map "\C-m" 'geben-context-mode-expand)
    ;;(define-key map "e" 'geben-context-mode-edit)
    (define-key map "r" 'geben-context-mode-refresh)
    (define-key map "q" 'geben-quit-window)
    (define-key map "p" 'widget-backward)
    (define-key map "n" 'widget-forward)
    (define-key map "?" 'geben-context-mode-help)
    map)
  "Keymap for `geben-context-mode'")

(defun geben-context-mode (session)
  "Major mode for GEBEN's context output.
The buffer commands are:
\\{geben-context-mode-map}"
  (interactive)
  (unless (eq major-mode 'geben-context-mode)
    (kill-all-local-variables)
    (use-local-map geben-context-mode-map)
    (setq major-mode 'geben-context-mode)
    (setq mode-name "GEBEN context")
    (set (make-local-variable 'revert-buffer-function)
	 (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
	 (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (if (fboundp 'run-mode-hooks)
	(run-mode-hooks 'geben-context-mode-hook)
      (run-hooks 'geben-context-mode-hook))
    (buffer-disable-undo)
    (set (make-local-variable 'geben-context-where) "")
    (set (make-local-variable 'geben-context-loading) nil)
    (set (make-local-variable 'tree-widget-theme) "geben")
    (setq header-line-format
	  (list
	   "Where: "
	   'geben-context-where
	   "   "
	   '(geben-context-loading "(loading...)")
	   ))
    (setq buffer-read-only t))
  (set (make-local-variable 'geben-current-session) session))

(defun geben-context-mode-refresh (&optional force)
  "Refresh the context buffer."
  (interactive)
  (geben-with-current-session session
    (geben-context-list-refresh session
				(geben-session-context-depth session)
				force)))

(defun geben-context-mode-help ()
  "Display description and key bindings of `geben-context-mode'."
  (interactive)
  (describe-function 'geben-context-mode))

;; context

(defun geben-dbgp-command-context-names (session &optional depth)
  (geben-dbgp-send-command session "context_names"
			   (and (numberp depth)
				(cons "-d" depth))))

(defun geben-dbgp-response-context-names (session cmd msg)
  (setf (geben-context-names (geben-session-context session))
	(mapcar (lambda (context)
		  (let ((name (xml-get-attribute context 'name))
			(id (xml-get-attribute context 'id)))
		    (cons name (string-to-number id))))
		(xml-get-children msg 'context))))

;; context

(defun geben-dbgp-command-context-get (session context-id &optional depth)
  (geben-dbgp-send-command session "context_get"
			   (cons "-c" context-id)
			   (and depth
				(cons "-d" depth))))

;; property

(defun geben-dbgp-command-property-get (session &rest args)
  (apply 'geben-dbgp-send-command session "property_get"
	 (mapcar (lambda (key)
		   (let ((arg (plist-get (car args) key)))
		     (when arg
		       (cons (geben-cmd-param-for key) arg))))
		 '(:depth :context-id :name :max-data-size :type :page :key :address))))

(provide 'geben-context)