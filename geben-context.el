;;--------------------------------------------------------------
;; context
;;--------------------------------------------------------------

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

(defstruct geben-dbgp-context
  names			     ; context names alist(KEY: context name, VALUE: context id)
  tid			     ; transaction id to which the current context variables belong.
  variables		     ;
  expanded-variables	     ; context variables in expanded state.
  where
  depth
)

(defvar geben-dbgp-context-loading nil)
(defvar geben-dbgp-prop-tree-fill-children-hook 'geben-dbgp-context-fill-tree-children)

;; context list accessors

(defsubst geben-dbgp-ctx-get-list (session cid)
  "Get context list for the context id CID."
  (assq cid
	(geben-dbgp-context-variables
	 (geben-session-plist-get session :context))))

(defsubst geben-dbgp-ctx-get-old-list (session cid)
  "Get previous context list for the context id CID."
  (cdr (assq 'old (geben-dbgp-ctx-get-list session cid))))

(defsubst geben-dbgp-ctx-get-new-list (session cid)
  "Get the current context list for the context id CID."
  (cdr (assq 'new (geben-dbgp-ctx-get-list session cid))))

(defsubst geben-dbgp-ctx-update-list (session cid list)
  "Update the current context list for the context id CID with LIST."
  (let* ((clist (geben-dbgp-ctx-get-list session cid))
	 (old (assq 'new clist)))
    (setcdr clist (list (cons 'old (cdr old))
			(cons 'new list)))))

;; context property list accessors

(defmacro geben-dbgp-prop-has-children (property)
  "Check whether PROPERTY has any children."
  `(equal "1" (xml-get-attribute ,property 'children)))

(defun geben-dbgp-prop-typeinfo (property)
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
		  :value-formatter 'geben-dbgp-prop-format-bool))
	   ((eq type 'string)
	    (list :type type
		  :type-visiblep nil
		  :value-face 'geben-context-string-face))
	   ((member type '(array hash))
	    (list :type type
		  :type-visiblep nil
		  :name-formatter 'geben-dbgp-prop-format-array-name
		  :value-face 'default))
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

(defun geben-dbgp-prop-format-bool (value)
  "Format VALUE in the debuggee language expression."
  (let ((bool (if (equal "0" value) nil t)))
    (if bool "true" "false")))

(defun geben-dbgp-prop-format-array-name (property)
  "Format array element name in the debuggee language expression."
  (format "%s[%s]"
	  (propertize (xml-get-attribute property 'name)
		      'face 'geben-context-variable-face)
	  (propertize (xml-get-attribute property 'numchildren)
		      'face 'geben-context-constant-face)))

(defun geben-dbgp-prop-get-attribute (property sym)
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

(defmacro geben-dbgp-prop-get-name (property)
  "Get name attribute value from PROPERTY."
  `(geben-dbgp-prop-get-attribute ,property 'name))
	
(defmacro geben-dbgp-prop-get-fullname (property)
  "Get fullname attribute value from PROPERTY."
  `(geben-dbgp-prop-get-attribute ,property 'fullname))

(defun geben-dbgp-prop-get-value (property)
  "Get value from PROPERTY."
  (let ((node (car (xml-get-children property 'value))))
    (if (consp node)
	(geben-dbgp-decode-string (xml-node-children node)
				  (xml-get-attribute node 'encoding)
				  'utf-8)
      (geben-dbgp-decode-string (xml-node-children property)
				(xml-get-attribute property 'encoding)
				'utf-8))))

;; context property tree widget

(defun geben-dbgp-prop-tree-open (tree)
  "Expand TREE."
  (let ((marker (widget-get tree :from)))
    (when (markerp marker)
      (with-current-buffer (marker-buffer marker)
	(goto-char marker)
	(call-interactively 'widget-button-press)
	(unless (widget-get tree :open)
	  (call-interactively 'widget-button-press))))))

(defun geben-dbgp-prop-tree-expand-p (tree)
  "A tree widget callback function to indicate whether TREE is able to expand."
  (or (geben-dbgp-prop-tree-has-complete-children tree)
      (and (run-hook-with-args 'geben-dbgp-prop-tree-fill-children-hook
			       tree)
	   nil)))

(defun geben-dbgp-prop-tree-expand (tree)
  "A tree widget callback function to create child list of TREE."
  (mapcar #'geben-dbgp-prop-tree-create-node
	  (xml-get-children (widget-get tree :property) 'property)))

(defun geben-dbgp-prop-tree-has-complete-children (tree)
  "Determine whether TREE has complete child nodes.
Child nodes can be short for :property property of TREE."
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property))
	 (numchildren (and children
			   (string-to-number (xml-get-attribute property 'numchildren)))))
    (and children
	 (<= numchildren (length children)))))

(defun geben-dbgp-prop-tree-create-node (property)
  "Create nodes which represent PROPERTY."
  (let* ((typeinfo (geben-dbgp-prop-typeinfo property))
	 (value (geben-dbgp-prop-get-value property))
	 tag)
    (let ((formatter (plist-get typeinfo :name-formatter)))
      (setq tag 
	    (if formatter
		(funcall formatter property)
	      (propertize (geben-dbgp-prop-get-name property)
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
    (if (geben-dbgp-prop-has-children property)
	(list 'tree-widget
	      :tag tag
	      :property property
	      :expander 'geben-dbgp-prop-tree-expand
	      :expander-p 'geben-dbgp-prop-tree-expand-p)
      (list 'item :tag (concat "   " tag)))))
  
(defun geben-dbgp-prop-tree-context-id (tree)
  "Get context id to which TREE belongs."
  (when tree
    (let ((cid (widget-get tree :context-id)))
      (or cid
	  (geben-dbgp-prop-tree-context-id (widget-get tree :parent))))))

;; context functions

(defun geben-dbgp-context-update (session depth &optional no-select-p)
  "Update the context buffer with context of a stack DEPTH.
If NO-SELECT-P is nil, the context buffer will be selected
after updating."
  (let ((buf (get-buffer geben-context-buffer-name))
	(context (geben-session-get session :context)))
    (when (and buf
	       (or (null no-select-p)
		   (get-buffer-window buf)) ;; only when the buffer is visible
	       (geben-dbgp-context-variables context))
      (with-current-buffer buf
	(setf (geben-dbgp-context-depth context) depth)
	(setf (geben-dbgp-context-where context) 
	      (xml-get-attribute (nth depth (geben-session-get session :stack)) 'where))
	(setq geben-dbgp-context-loading t))
      ;; Remain the current tid.
      ;; It is possible that the current context proceeds by step_in or
      ;; other continuous commands while retrieving variables.
      ;; To avoid mixing variables with multi context, remain something at here,
      ;; tid, and check the value in the retrieving process.
      (setf (geben-dbgp-context-tid context)
	    (geben-session-get session :tid))
      (geben-dbgp-context-update-loop session
				      (geben-dbgp-context-tid context)
				      depth
				      (mapcar (lambda (context)
						(cdr context))
					      (geben-dbgp-context-names-alist context))
				      no-select-p))))

(defun geben-dbgp-context-update-1 (session cmd msg)
  (when (get-buffer geben-context-buffer-name)
    (geben-dbgp-ctx-update-list session
				(geben-dbgp-cmd-param-get cmd "-c")
				(xml-get-children msg 'property))))

(defun geben-dbgp-context-update-loop (session tid-save depth context-id-list no-select-p)
  (geben-dbgp-cmd-sequence
   (geben-dbgp-command-context-get session (car context-id-list) depth)
   ((tid-save depth context-id-list no-select-p)
    (lambda (session cmd msg)
      (when (and (not err)
		 (eq tid-save geben-dbgp-context-tid))
	(geben-dbgp-context-update-1 session cmd msg)
	(let ((context-id-list (cdr (quote context-id-list))))
	  (if context-id-list
	      (geben-dbgp-context-update-loop session tid-save depth context-id-list no-select-p)
	    (let ((buf (geben-dbgp-context-fill-buffer session)))
	      (when (and buf
			 (not (quote no-select-p)))
		(geben-dbgp-display-window buf))))))))))

(defun geben-dbgp-context-fill-buffer (session)
  "Fill the context buffer with locally stored context list."
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when buf
      (with-current-buffer buf
	(let ((inhibit-read-only t)
	      (inhibit-modification-hooks t))
	  (widen)
	  (erase-buffer)
	  (dolist (context-name geben-dbgp-context-names-alist)
	    (let ((old (geben-dbgp-ctx-get-old-list (cdr context-name)))
		  (new (geben-dbgp-ctx-get-new-list (cdr context-name))))
	      (apply 'widget-create
		     'tree-widget
		     :tag (car context-name)
		     :context-id (cdr context-name)
		     :open t
		     (mapcar #'geben-dbgp-prop-tree-create-node new))))
	  (widget-setup))
	(goto-char (point-min))
	(setq geben-dbgp-context-loading nil)))
    buf))

(defun geben-dbgp-context-fill-tree-children (tree &optional tid-save)
  (let ((tid-save (or tid-save
		      geben-dbgp-context-tid))
	(completed (geben-dbgp-prop-tree-has-complete-children tree)))
    (when (eq geben-dbgp-context-tid tid-save)
      (with-current-buffer (get-buffer geben-context-buffer-name)
	(setq geben-dbgp-context-loading (not completed)))
      (if completed
	  (geben-dbgp-prop-tree-open tree)
	(geben-dbgp-context-fill-tree-children-1 tree tid-save)))))

(defun geben-dbgp-context-fill-tree-children-1 (tree tid-save)
  (let* ((property (widget-get tree :property))
	 (children (xml-get-children property 'property)))
    (with-current-buffer (get-buffer geben-context-buffer-name)
      ;; -- comment on :property-page property --
      ;; debugger engine may lack of PAGESIZE in property message(bug).
      ;; so the following code doesn't rely on PAGESIZE but uses own
      ;; :property-page widget property.
      (let* ((nextpage (if (widget-get tree :property-page)
			   (1+ (widget-get tree :property-page))
			 (if children 1 0)))
	     (args (list :depth geben-dbgp-context-depth
			 :context-id (geben-dbgp-prop-tree-context-id tree)
			 :name (geben-dbgp-prop-get-fullname property)
			 :page nextpage)))
	(widget-put tree :property-page nextpage)
	(when (xml-get-attribute-or-nil property 'key)
	  (plist-put args :key (xml-get-attribute-or-nil property 'key)))
	(geben-dbgp-cmd-sequence
	 (geben-dbgp-command-property-get args)
	 ((tree tid-save)
	  (lambda (session cmd msg)
	    (unless (dbgp-xml-get-error-message msg)
	      (geben-dbgp-context-append-tree-children tid-save
						       tree
						       (car (xml-get-children msg 'property)))
	      (geben-dbgp-context-fill-tree-children tree
						     tid-save)))))))))

(defun geben-dbgp-context-append-tree-children (tid-save tree property)
  (when (eq geben-dbgp-context-tid tid-save)
    (let ((tree-prop (widget-get tree :property)))
      (nconc (or (cddr tree-prop)
		 tree-prop)
	     (cddr property)))))

(defun geben-dbgp-context-display (depth)
  "Display context variables in the context buffer."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (let ((buf (get-buffer geben-context-buffer-name)))
    (when (or (< depth 0)
	      (< (length (geben-session-get session :stack)) (1+ depth)))
      (error "GEBEN context display: invalid depth: %S" depth))
    (unless buf
      (setq buf (get-buffer-create geben-context-buffer-name))
      (with-current-buffer buf
	(geben-context-mode)))
    (unless geben-dbgp-context-variables
      (setq geben-dbgp-context-variables
	    (mapcar (lambda (context)
		      (list (cdr context)))
		    geben-dbgp-context-names-alist)))
    (geben-dbgp-context-update depth)))

;; context mode

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
    (define-key map "r" 'geben-dbgp-context-refresh)
    (define-key map "q" 'geben-quit-window)
    (define-key map "p" 'widget-backward)
    (define-key map "n" 'widget-forward)
    (define-key map "?" 'geben-context-mode-help)
    map)
  "Keymap for `geben-context-mode'")

(defun geben-context-mode ()
  "Major mode for GEBEN's context output.
The buffer commands are:
\\{geben-context-mode-map}"
  (interactive)
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
  (make-local-variable 'geben-dbgp-context-where)
  (make-local-variable 'geben-dbgp-context-depth)
  (make-local-variable 'geben-dbgp-context-loading)
  (set (make-local-variable 'tree-widget-theme) "geben")
  (setq header-line-format
	(list
	 "Where: "
	 'geben-dbgp-context-where
	 "   "
	 '(geben-dbgp-context-loading "(loading...)")
	 ))
  (setq buffer-read-only t))

(defun geben-dbgp-context-refresh ()
  "Refresh the context buffer."
  (interactive)
  (geben-dbgp-with-current-session session
    (let ((buf (get-buffer geben-context-buffer-name)))
      (when (and buf
		 (buffer-live-p buf)
		 (geben-session-active-p session))
	(let (depth)
	  (with-current-buffer buf
	    (setq depth geben-dbgp-context-depth))
	  (geben-dbgp-context-update depth))))))

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
  (setq geben-dbgp-context-names-alist
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

(provide 'geben-context)