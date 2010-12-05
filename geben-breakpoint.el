(require 'cl)
(require 'geben-common)
(require 'geben-util)
(require 'geben-session)
(require 'geben-dbgp-util)
(require 'geben-source)
(require 'geben-cursor)
(require 'geben-dbgp)

;;==============================================================
;; breakpoints
;;==============================================================

(defstruct (geben-breakpoint
	    (:constructor nil)
	    (:constructor geben-breakpoint-make))
  "Configuration of breakpoints for a session.

types:
  Breakpoint types, supported by the current debugger engine.

list:
  A list of break points."
  (types '(:line :call :return :exception :conditional))
  list)

(defface geben-breakpoint-face
  '((((class color))
     :foreground "white"
     :background "red1")
    (t :inverse-video t))
  "Face used to highlight various names.
This includes element and attribute names, processing
instruction targets and the CDATA keyword in a CDATA section.
This is not used directly, but only via inheritance by other faces."
  :group 'geben-highlighting-faces)

(defcustom geben-show-breakpoints-debugging-only t
  "*Specify breakpoint markers visibility.
If the value is nil, GEBEN will always display breakpoint markers.
If non-nil, displays the markers while debugging but hides after
debugging is finished."
  :group 'geben
  :type 'boolean)

;;--------------------------------------------------------------
;; breakpoint object
;;--------------------------------------------------------------

;; breakpoint object manipulators

(defun geben-bp-make (session type &rest params)
  "Create a new line breakpoint object."
  (assert (geben-session-p session))
  (let ((bp (append (list :type type) params)))
    ;; force :lineno and :hit-value value to be integer.
    (mapc (lambda (prop)
	    (when (stringp (plist-get bp prop))
	      (plist-put bp prop (string-to-number (plist-get bp prop)))))
	  '(:lineno :hit-value))
    ;; setup overlay
    (when (and (plist-get params :fileuri)
	       (plist-get params :lineno)
	       (not (plist-get params :overlay)))
      (geben-bp-overlay-setup bp))
    ;; Xdebug issue; generate :class and :method name from :function
    (let ((name (plist-get params :function)))
      (and name
	   (geben-session-xdebug-p session)
	   (string-match "[:->]" name)
	   (plist-put bp :class (replace-regexp-in-string "^\\([^:-]+\\).*" "\\1" name))
	   (plist-put bp :method (replace-regexp-in-string "^.*[:>]+" "" name))))
    ;; make sure bp has :state.
    (unless (plist-get params :state)
      (plist-put bp :state "enabled"))
    bp))

(defsubst geben-bp-finalize (bp)
  "Finalize a breakpoint object."
  (let ((overlay (plist-get bp :overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)))
  bp)

(defsubst geben-bp= (lhs rhs)
  "Return t if two breakpoint object point same thing."
  (and (eq (plist-get lhs :type)
	   (plist-get rhs :type))
       (eq (plist-get lhs :lineno)
	   (plist-get rhs :lineno))
       (equal (plist-get lhs :fileuri)
	      (plist-get rhs :fileuri))
       (equal (plist-get lhs :function)
	      (plist-get rhs :function))
       (equal (plist-get lhs :exception)
	      (plist-get rhs :exception))
       (equal (plist-get lhs :expression)
	      (plist-get rhs :expression))))

;; session storage

(defun geben-session-breakpoint-storage-add (session bp)
  (let* ((storage (geben-session-storage session))
	 (list (plist-get storage :bp)))
    (unless (find bp list :test #'geben-bp=)
      (let ((bp-copy (copy-sequence bp)))
	(plist-put bp-copy :overlay nil)
	(if list
	    (nconc list (list bp-copy))
	  (plist-put storage :bp (list bp-copy)))))))

(defun geben-session-breakpoint-storage-remove (session bp)
  (let* ((storage (geben-session-storage session))
	 (list (plist-get storage :bp)))
    (when (find bp list :test #'geben-bp=)
      (plist-put storage :bp (delete* bp list :test #'geben-bp=)))))

(defun geben-session-breakpoint-storage-restore (session)
  (let ((storage (geben-session-storage session))
	(breakpoint (geben-session-breakpoint session)))
    (setf (geben-breakpoint-list breakpoint)
	  (plist-get storage :bp))))

;; session

(defun geben-session-breakpoint-add (session bp)
  "Add a breakpoint BP to session's breakpoint list."
  (unless (geben-session-breakpoint-find session bp)
    (let* ((breakpoint (geben-session-breakpoint session))
	   (list (geben-breakpoint-list breakpoint)))
      (if list
	  (nconc list (list bp))
	(setf (geben-breakpoint-list breakpoint) (list bp))))
    (geben-session-breakpoint-storage-add session bp)))

(defun geben-session-breakpoint-remove (session id-or-obj)
  "Remove breakpoints having specific breakpoint id or same meaning objects."
  (setf (geben-breakpoint-list (geben-session-breakpoint session))
	(remove-if (if (stringp id-or-obj)
		       (lambda (bp)
			 (when (string= (plist-get bp :id) id-or-obj)
			   (geben-session-breakpoint-storage-remove session bp)
			   (geben-bp-finalize bp)))
		     (lambda (bp)
		       (when (geben-bp= id-or-obj bp)
			 (geben-session-breakpoint-storage-remove session bp)
			 (geben-bp-finalize bp))))
		   (geben-breakpoint-list (geben-session-breakpoint session)))))

(defun geben-session-breakpoint-find (session id-or-obj)
  "Find a breakpoint.
id-or-obj should be either a breakpoint id or a breakpoint object."
  (find-if 
   (if (stringp id-or-obj)
       (lambda (bp)
	 (string= (plist-get bp :id) id-or-obj))
     (lambda (bp)
       (geben-bp= id-or-obj bp)))
   (geben-breakpoint-list (geben-session-breakpoint session))))
  
;; dbgp

(defun geben-dbgp-breakpoint-restore (session)
  "Restore breakpoints against new DBGp session."
  (let ((breakpoints (mapcan (lambda (session)
			       (prog1
				   (geben-breakpoint-list (geben-session-breakpoint session))
				 (setf (geben-breakpoint-list (geben-session-breakpoint session)) nil)))
			     (list session (geben-offline-session))))
	overlay)
    (dolist (bp breakpoints)
      ;; User may edit code since previous debugging session
      ;; so that lineno breakpoints set before may moved.
      ;; The followings try to adjust breakpoint line to
      ;; nearly what user expect.
      (if (and (setq overlay (plist-get bp :overlay))
	       (overlayp overlay)
	       (overlay-livep overlay)
	       (eq (overlay-buffer overlay)
		   (find-buffer-visiting (or (plist-get bp :local-path)
					     ""))))
	  (with-current-buffer (overlay-buffer overlay)
	    (save-excursion
	      (plist-put bp :lineno (progn
				      (goto-char (overlay-start overlay))
				      (geben-what-line))))))
      (geben-dbgp-sequence-bind (bp)
	(geben-dbgp-command-breakpoint-set session bp)
	(lambda (session cmd msg err)
	  (geben-bp-finalize bp))))))

(defun geben-breakpoint-remove (session bp-or-list)
  "Remove specified breakpoints."
  (dolist (bp (if (geben-breakpoint-p bp-or-list)
		  (list bp-or-list)
		bp-or-list))
    (let ((bid (plist-get bp :id)))
      (if (and (geben-session-active-p session)
	       bid)
	  (geben-dbgp-sequence-bind (bid)
	    (geben-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
	    (lambda (session cmd msg err)
	      ;; remove a stray breakpoint from hash table.
	      (when err
		(geben-session-breakpoint-remove session bid))))
	(setf (geben-breakpoint-list (geben-session-breakpoint session))
	      (delete-if (lambda (bp1)
			   (geben-bp= bp bp1))
			 (geben-breakpoint-list (geben-session-breakpoint session))))))))

(defun geben-breakpoint-clear (session)
  "Clear all breakpoints."
  (geben-breakpoint-remove session
			   (geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-breakpoint-find-at-pos (session buf pos)
  (with-current-buffer buf
    (remove-if 'null
	       (mapcar (lambda (overlay)
			 (let ((bp (overlay-get overlay 'bp)))
			   (and (eq :line (plist-get bp :type))
				bp)))
		       (overlays-at pos)))))

;; breakpoint list

(defface geben-breakpoint-fileuri
  '((t (:inherit geben-backtrace-fileuri)))
  "Face used to highlight fileuri in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-lineno
  '((t (:inherit geben-backtrace-lineno)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defface geben-breakpoint-function
  '((t (:inherit font-lock-function-name-face)))
  "Face for displaying line numbers in breakpoint list buffer."
  :group 'geben-highlighting-faces)

(defun geben-breakpoint-sort-pred (a b)
  (if (and (stringp (plist-get a :id))
	   (equal (plist-get a :id)
		  (plist-get b :id)))
      nil
    (let ((type-rank '(:line 1
			     :call 2
			     :return 3
			     :exception 4
			     :conditional 5
			     :watch 6))
	  ax bx cmp)
      (setq cmp (- (plist-get type-rank (plist-get a :type))
		   (plist-get type-rank (plist-get b :type))))
      (if (not (zerop cmp))
	  (< cmp 0)
	(case (plist-get a :type)
	  (:line
	   (setq ax (plist-get a :fileuri))
	   (setq bx (plist-get b :fileuri))
	   (or (string< ax bx)
	       (and (string= ax bx)
		    (< (plist-get a :lineno)
		       (plist-get b :lineno)))))
	  (:call
	   (string< (plist-get a :function)
		    (plist-get b :function)))
	  (:return
	   (string< (plist-get a :function)
		    (plist-get b :function)))
	  (:exception
	   (string< (plist-get a :exception)
		    (plist-get b :exception)))
	  (:conditional
	   (or (string< (plist-get a :fileuri)
			(plist-get b :fileuri))
	       (progn
		 (setq ax (plist-get a :lineno)
		       bx (plist-get b :lineno))
		 (if (null ax)
		     (not (null ax))
		   (if (null ax)
		       nil
		     (< ax bx))))
	       (string< (plist-get a :expression)
			(plist-get b :expression))))
	  (:watch
	   (string< (plist-get a :expression)
		    (plist-get b :expression))))))))

;;--------------------------------------------------------------
;; breakpoint list mode
;;--------------------------------------------------------------

(defcustom geben-breakpoint-list-mode-hook nil
  "*Hook running at when GEBEN's breakpoint list buffer is initialized."
  :group 'geben
  :type 'hook)

(defvar geben-breakpoint-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'geben-breakpoint-list-mode-mouse-goto)
    (define-key map "\C-m" 'geben-breakpoint-list-mode-goto)
    (define-key map "d" 'geben-breakpoint-list-mark-delete)
    (define-key map "u" 'geben-breakpoint-list-unmark)
    (define-key map "x" 'geben-breakpoint-list-execute)
    (define-key map "q" 'geben-quit-window)
    (define-key map "r" 'geben-breakpoint-list-refresh)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "?" 'geben-breakpoint-list-mode-help)
    map)
  "Keymap for `geben-breakpoint-list-mode'")
    
(defun geben-breakpoint-list-mode (session)
  "Major mode for GEBEN's breakpoint list.
The buffer commands are:
\\{geben-breakpoint-list-mode-map}"
  (unless (eq major-mode 'geben-breakpoint-list-mode)
    (kill-all-local-variables)
    (use-local-map geben-breakpoint-list-mode-map)
    (setq major-mode 'geben-breakpoint-list-mode)
    (setq mode-name "GEBEN breakpoints")
    (set (make-local-variable 'revert-buffer-function)
	 (lambda (a b) nil))
    (and (fboundp 'font-lock-defontify)
	 (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (if (fboundp 'run-mode-hooks)
	(run-mode-hooks 'geben-breakpoint-list-mode-hook)
      (run-hooks 'geben-breakpoint-list-mode-hook)))
  (set (make-local-variable 'geben-current-session) session))

(defun geben-breakpoint-list-mark-delete ()
  "Add deletion mark."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun geben-breakpoint-list-unmark ()
  "Remove deletion mark."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert " ")
      (forward-line 1))))

(defun geben-breakpoint-list-execute ()
  "Execute breakpoint deletion."
  (interactive)
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (geben-with-current-session session
      (let (candidates)
	(save-excursion
	  (goto-char (point-min))
	  (let ((buffer-read-only nil))
	    (while (re-search-forward "^D" nil t)
	      (add-to-list 'candidates (get-text-property (point) 'geben-bp)))))
	(geben-breakpoint-remove session candidates)
	(when candidates
	  (geben-breakpoint-list-display session))))))

(defun geben-breakpoint-list-mode-goto (&optional event)
  "Move to the set point of the selected breakpoint."
  (interactive (list last-nonmenu-event))
  (when (eq major-mode 'geben-breakpoint-list-mode)
    (geben-with-current-session session
      (let ((bp
	     (if (or (null event)
		     (not (listp event)))
		 ;; Actually `event-end' works correctly with a nil argument as
		 ;; well, so we could dispense with this test, but let's not
		 ;; rely on this undocumented behavior.
		 (get-text-property (point) 'geben-bp)
	       (with-current-buffer (window-buffer (posn-window (event-end event)))
		 (save-excursion
		   (goto-char (posn-point (event-end event)))
		   (get-text-property (point) 'geben-bp)))))
	    same-window-buffer-names
	    same-window-regexps)
	(let ((fileuri (plist-get bp :fileuri))
	      (lineno (plist-get bp :lineno)))
	  (and fileuri lineno
	       (geben-session-cursor-update session fileuri lineno)))))))

(defun geben-breakpoint-list-mode-help ()
  "Display description and key bindings of `geben-breakpoint-list-mode'."
  (interactive)
  (describe-function 'geben-breakpoint-list-mode))

(defun geben-breakpoint-list-refresh (&optional force)
  "Display breakpoint list.
The breakpoint list buffer is under `geben-breakpoint-list-mode'.
Key mapping and other information is described its help page."
  (interactive)
  (geben-with-current-session session
    (when (and (geben-session-active-p session)
	       (or force
		   (geben-session-buffer-visible-p session
						   geben-breakpoint-list-buffer-name)))
      (geben-dbgp-sequence
	  (geben-dbgp-send-command session "breakpoint_list")
	(lambda (session cmd msg err)
	  (geben-breakpoint-recreate session cmd msg err)
	  (geben-breakpoint-list-display session))))))

(defun geben-breakpoint-recreate (session cmd msg err)
  "Create breakpoint objects according to the result of `breakpoint_list'."
  (unless err
    (dolist (msg-bp (xml-get-children msg 'breakpoint))
      (let* ((id (xml-get-attribute-or-nil msg-bp 'id))
	     (bp (geben-session-breakpoint-find session id)))
	(unless bp
	  (let* ((type (intern-soft (concat ":" (xml-get-attribute msg-bp 'type))))
		 (fileuri (xml-get-attribute-or-nil msg-bp 'filename))
		 (lineno (or (xml-get-attribute-or-nil msg-bp 'lineno)
			     (xml-get-attribute-or-nil msg-bp 'line)))
		 (function (xml-get-attribute-or-nil msg-bp 'function))
		 (class (xml-get-attribute-or-nil msg-bp 'class))
		 (method function)
		 (exception (xml-get-attribute-or-nil msg-bp 'exception))
		 (expression (xml-get-attribute-or-nil msg-bp 'expression))
		 (state (xml-get-attribute-or-nil msg-bp 'state))
		 (local-path (and fileuri
				  (or (geben-session-source-local-path session fileuri)
				      (geben-source-local-path session fileuri)))))
	    (when (stringp lineno)
	      (setq lineno (string-to-number lineno))
	      (when (floatp lineno) ;; debugger engine may return invalid number.
		(setq lineno 1)))
	    (when class
	      (setq function (format "%s::%s" (or function "") class)))
	    (when expression
	      (setq expression (base64-decode-string expression)))
	    (geben-session-breakpoint-add
	     session
	     (setq bp (geben-bp-make session type
				     :id id
				     :fileuri fileuri
				     :lineno lineno
				     :class class
				     :method method
				     :function function
				     :exception exception
				     :expression expression
				     :state state
				     :local-path local-path)))))
	(when bp
	  (plist-put bp :hit-count (string-to-number (xml-get-attribute msg-bp 'hit_count)))
	  (plist-put bp :hit-value (string-to-number (xml-get-attribute msg-bp 'hit_value))))))))

(defun geben-breakpoint-list-display (session)
  (let ((buf (geben-session-buffer session geben-breakpoint-list-buffer-name))
	(breakpoints (geben-breakpoint-list (geben-session-breakpoint session)))
	pos)
    (with-current-buffer buf
      (geben-breakpoint-list-mode session)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(if (or (not (listp breakpoints))
		(zerop (length breakpoints)))
	    (insert "No breakpoints.\n")
	  (setq breakpoints (sort (copy-list breakpoints)
				  #'geben-breakpoint-sort-pred))
	  (mapc (lambda (bp)
		  (insert "  ")
		  (insert (format "%-11s"
				  (or (case (plist-get bp :type)
					(:line "Line")
					(:exception "Exception")
					(:call "Call")
					(:return "Return")
					(:conditional "Conditional")
					(:watch "Watch"))
				      "Unknown")))
		  (if (geben-session-active-p session)
		      (insert (format "%2s/%-2s  "
				      (or (plist-get bp :hit-count) "?")
				      (let ((hit-value (plist-get bp :hit-value)))
					(cond
					 ((null hit-value) "?")
					 ((zerop hit-value) "*")
					 (t hit-value)))))
		    (insert " "))
		  (when (plist-get bp :function)
		    (insert (propertize (plist-get bp :function)
					'face 'geben-breakpoint-function))
		    (insert " "))
		  (when (plist-get bp :exception)
		    (insert (propertize (plist-get bp :exception)
					'face 'geben-breakpoint-function))
		    (insert " "))
		  (when (plist-get bp :expression)
		    (insert (format "\"%s\" " (plist-get bp :expression))))
		  (when (plist-get bp :fileuri)
		    (insert (format "%s:%s"
				    (propertize (plist-get bp :fileuri)
						'face 'geben-breakpoint-fileuri)
				    (propertize (format "%s" (or (plist-get bp :lineno) "*"))
						'face 'geben-breakpoint-lineno))))
		  (insert "\n")
		  (put-text-property (save-excursion (forward-line -1) (point))
				     (point)
				     'geben-bp bp))
		breakpoints))
	(setq header-line-format
	      (concat "  Type        "
		      (if (geben-session-active-p session) "Hits  " "")
		      "Property"))
	(goto-char (point-min))))
    (save-selected-window
      (geben-dbgp-display-window buf))))

;; overlay

(defun geben-bp-overlay-setup (bp)
  "Create an overlay for a breakpoint BP."
  (geben-bp-finalize bp)
  (let* ((local-path (plist-get bp :local-path))
	 (overlay (and (stringp local-path)
		       (find-buffer-visiting local-path)
		       (geben-overlay-make-line (plist-get bp :lineno)
						(find-buffer-visiting local-path)))))
    (when overlay
      (overlay-put overlay 'face 'geben-breakpoint-face)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'bp bp)
      (overlay-put overlay 'modification-hooks '(geben-bp-overlay-modified))
      (overlay-put overlay 'insert-in-front-hooks '(geben-bp-overlay-inserted-in-front))
      (plist-put bp :overlay overlay)))
  bp)

(defun geben-bp-overlay-hide (session)
  "Hide breakpoint overlays."
  (mapc (lambda (bp)
	  (let ((overlay (plist-get bp :overlay)))
	    (and (overlayp overlay)
		 (overlay-livep overlay)
		 (overlay-put overlay 'face nil))))
	(geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-bp-overlay-modified (overlay afterp beg end &optional len)
  "A callback function invoked when inside of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (when afterp
    (save-excursion
      (save-restriction
	(widen)
	(let* ((lineno-from (progn (goto-char (overlay-start overlay))
				   (geben-what-line)))
	       (lineno-to (progn (goto-char (overlay-end overlay))
				 (geben-what-line)))
	       (lineno lineno-from))
	  (goto-line lineno)
	  (while (and (looking-at "[ \t]*$")
		      (< lineno lineno-to))
	    (forward-line)
	    (incf lineno))
	  (if (< lineno-from lineno)
	      (plist-put (overlay-get overlay 'bp) :lineno lineno))
	  (goto-line lineno)
	  (beginning-of-line)
	  (move-overlay overlay (point) (save-excursion
					  (forward-line)
					  (point))))))))

(defun geben-bp-overlay-inserted-in-front (overlay afterp beg end &optional len)
  "A callback function invoked when text in front of an overlay is modified.
With this callback GEBEN tracks displacements of line breakpoints."
  (if afterp
      (save-excursion
	(goto-line (progn (goto-char (overlay-start overlay))
			  (geben-what-line)))
	(move-overlay overlay (point) (save-excursion
					(forward-line)
					(point))))))

(defun geben-bp-overlay-restore (session buf)
  "A callback function invoked when emacs visits a new file.
GEBEN may place overlay markers if there are line breakpoints in
the file."
  (mapc (lambda (bp)
	  (and (plist-get bp :lineno)
	       (eq buf (find-buffer-visiting (or (plist-get bp :local-path)
						 "")))
	       (geben-bp-overlay-setup bp)))
	(geben-breakpoint-list (geben-session-breakpoint session))))

(defun geben-session-breakpoint-init (session)
  (setf (geben-session-breakpoint session) (geben-breakpoint-make))
  (geben-session-breakpoint-storage-restore session))

(add-hook 'geben-session-enter-hook #'geben-session-breakpoint-init)

(defun geben-session-breakpoint-release (session)
  (when geben-show-breakpoints-debugging-only
    (geben-bp-overlay-hide session)))

(add-hook 'geben-session-exit-hook #'geben-session-breakpoint-release)

(defun geben-dbgp-breakpoint-store-types (session cmd msg err)
  (when (equal "1" (xml-get-attribute msg 'supported))
    (let ((types (mapcar
		  (lambda (type)
		    (intern (concat ":" type)))
		  (split-string (or (car (xml-node-children msg))
				    "")
				" "))))
      (if (geben-session-xdebug-p session)
	  ;; Xdebug 2.0.3 supports the following types but they aren't
	  ;; included in the response. Push them in the list manually.
	  (setq types (append types '(:exception :conditional))))
      (unless types
	;; Some debugger engines are buggy;
	;; they don't return breakpoint types correctly.
	;; To them put all of types to the list.
	(setq types '(:line :call :return :exception :conditional :watch)))
      (setf (geben-breakpoint-types (geben-session-breakpoint session)) types))))

(add-hook 'geben-source-visit-hook #'geben-bp-overlay-restore)

;;; breakpoint_set

(defun geben-dbgp-command-breakpoint-set (session bp)
  "Send \`breakpoint_set\' command."
  (if (not (geben-session-active-p session))
      (geben-session-breakpoint-add session bp)
    (let ((obp (geben-session-breakpoint-find session bp)))
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
			     (geben-session-xdebug-p session)
			     (cons "-a" (plist-get bp :class)))
			(and (plist-get bp :function)
			     (if (and (geben-session-xdebug-p session)
				      (plist-get bp :method))
				 (cons "-m" (plist-get bp :method))
			       (cons "-m" (plist-get bp :function))))
			(and (plist-get bp :exception)
			     (cons "-x" (plist-get bp :exception)))
			(cons "-h" (or (plist-get bp :hit-value) 0))
			(cons "-o" ">=")
			(cons "-s" (or (plist-get bp :state)
				       "enabled"))
			(cons "-r" (if (plist-get bp :run-once) 1 0))
			(and (plist-get bp :expression)
			     (cons "--"
				   (base64-encode-string
				    (plist-get bp :expression))))))))
	  (when params
	    (apply 'geben-dbgp-send-command session "breakpoint_set" params)))))))

(defun geben-dbgp-response-breakpoint-set (session cmd msg)
  "A response message handler for \`breakpoint_set\' command."
  (unless (eq (geben-cmd-param-get cmd "-r") 1) ; unless :run-once is set
    (let* ((type (intern (concat ":" (geben-cmd-param-get cmd "-t"))))
	   (id (xml-get-attribute-or-nil msg 'id))
	   (fileuri (geben-cmd-param-get cmd "-f"))
	   (lineno (geben-cmd-param-get cmd "-n"))
	   (function (geben-cmd-param-get cmd "-m"))
	   (class (geben-cmd-param-get cmd "-a"))
	   (method function)
	   (exception (geben-cmd-param-get cmd "-x"))
	   (expression (geben-cmd-param-get cmd "--"))
	   (hit-value (geben-cmd-param-get cmd "-h"))
	   (state (geben-cmd-param-get cmd "-s"))
	   (local-path (and fileuri
			    (or (geben-session-source-local-path session fileuri)
				(geben-source-local-path session fileuri))))
	   bp)
      (when expression
	(setq expression (base64-decode-string expression)))
      (geben-session-breakpoint-add session
				    (setq bp (geben-bp-make session type
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
    (geben-breakpoint-list-refresh)))

(defun geben-dbgp-response-breakpoint-update (session cmd msg)
  "A response message handler for `breakpoint_update' command."
  (let* ((id (geben-cmd-param-get cmd "-d"))
	 (bp (geben-session-breakpoint-find session id)))
    (when bp
      (plist-put bp :hit-value (geben-cmd-param-get cmd "-h"))
      (geben-breakpoint-list-refresh))))

;;; breakpoint_remove

(defun geben-dbgp-command-breakpoint-remove (session bid)
  "Send `breakpoint_remove' command."
  (if (geben-session-active-p session)
      (geben-dbgp-sequence-bind (bid)
	(geben-dbgp-send-command session "breakpoint_remove" (cons "-d" bid))
	(lambda (session cmd msg err)
	  (when (dbgp-xml-get-error-message msg)
	    ;; remove a stray breakpoint from hash table.
	    (geben-session-breakpoint-remove session bid)
	    (geben-breakpoint-list-refresh))))
    (geben-session-breakpoint-remove session bid)))

(defun geben-dbgp-response-breakpoint-remove (session cmd msg)
  "A response message handler for \`breakpoint_remove\' command."
  (let* ((id (geben-cmd-param-get cmd "-d"))
	 (bp (geben-session-breakpoint-find session id)))
    (geben-session-breakpoint-remove session id)
    (geben-breakpoint-list-refresh)))

(defun geben-dbgp-command-breakpoint-list (session)
  "Send `breakpoint_list' command."
  (geben-dbgp-send-command session "breakpoint_list"))

(defun geben-dbgp-response-breakpoint-list (session cmd msg)
  "A response message handler for \`breakpoint_list\' command."
  t)

(defun geben-dbgp-breakpoint-list-refresh (session)
  (geben-breakpoint-list-refresh))
  
(provide 'geben-breakpoint)