;;--------------------------------------------------------------
;; stack
;;--------------------------------------------------------------

;; backtrace

(defface geben-backtrace-fileuri
  '((((class color))
     (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face used to highlight fileuri in backtrace buffer."
  :group 'geben-highlighting-faces)

(defface geben-backtrace-lineno
  '((t :inherit font-lock-variable-name-face))
  "Face for displaying line numbers in backtrace buffer."
  :group 'geben-highlighting-faces)

(defcustom geben-backtrace-mode-hook nil
  "*Hook running at when GEBEN's backtrace buffer is initialized."
  :group 'geben
  :type 'hook)

(defun geben-backtrace (session)
  "Display backtrace."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (let ((buf (get-buffer-create geben-backtrace-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)
      (dotimes (i (length (geben-session-get session :stack)))
	(let* ((stack (nth i (geben-session-get session :stack)))
	       (fileuri (geben-dbgp-regularize-fileuri (xml-get-attribute stack 'filename)))
	       (lineno (xml-get-attribute stack 'lineno))
	       (where (xml-get-attribute stack 'where))
	       (level (xml-get-attribute stack 'level)))
	  (insert (format "%s:%s %s\n"
			  (propertize fileuri 'face "geben-backtrace-fileuri")
			  (propertize lineno 'face "geben-backtrace-lineno")
			  where))
	  (put-text-property (save-excursion (forward-line -1) (point))
			     (point)
			     'geben-stack-frame
			     (list :fileuri fileuri
				   :lineno lineno
				   :level (string-to-number level)))))
      (setq buffer-read-only t)
      (geben-backtrace-mode)
      (goto-char (point-min)))
    (geben-dbgp-display-window buf)))

(defvar geben-backtrace-mode-map nil
  "Keymap for `geben-backtrace-mode'")
(unless geben-backtrace-mode-map
  (setq geben-backtrace-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-2] 'geben-backtrace-mode-mouse-goto)
	  (define-key map "\C-m" 'geben-backtrace-mode-goto)
	  (define-key map "q" 'geben-backtrace-mode-quit)
	  (define-key map "p" 'previous-line)
	  (define-key map "n" 'next-line)
	  (define-key map "v" 'geben-backtrace-mode-context)
	  (define-key map "?" 'geben-backtrace-mode-help)
	  map)))

(defun geben-backtrace-mode ()
  "Major mode for GEBEN's backtrace output.
The buffer commands are:
\\{geben-backtrace-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map geben-backtrace-mode-map)
  (setq major-mode 'geben-backtrace-mode)
  (setq mode-name "GEBEN backtrace")
  (set (make-local-variable 'revert-buffer-function)
       (lambda (a b) nil))
  (and (fboundp 'font-lock-defontify)
       (add-hook 'change-major-mode-hook 'font-lock-defontify nil t))
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'geben-backtrace-mode-hook)
    (run-hooks 'geben-backtrace-mode-hook)))

(defalias 'geben-backtrace-mode-mouse-goto 'geben-backtrace-mode-goto)
(defun geben-backtrace-mode-goto (&optional event)
  (interactive (list last-nonmenu-event))
  (let ((stack-frame
	 (if (or (null event)
		 (not (listp event)))
	     ;; Actually `event-end' works correctly with a nil argument as
	     ;; well, so we could dispense with this test, but let's not
	     ;; rely on this undocumented behavior.
	     (get-text-property (point) 'geben-stack-frame)
	   (with-current-buffer (window-buffer (posn-window (event-end event)))
	     (save-excursion
	       (goto-char (posn-point (event-end event)))
	       (get-text-property (point) 'geben-stack-frame)))))
	same-window-buffer-names
	same-window-regexps)
    (when stack-frame
      (geben-dbgp-indicate-current-line (plist-get stack-frame :fileuri)
					(plist-get stack-frame :lineno)
					t))))

(defun geben-backtrace-mode-quit ()
  "Quit and bury the backtrace mode buffer."
  (interactive)
  (quit-window)
  (geben-where))

(defun geben-backtrace-mode-help ()
  "Display description and key bindings of `geben-backtrace-mode'."
  (interactive)
  (describe-function 'geben-backtrace-mode))

(defun geben-backtrace-mode-context ()
  (interactive)
  (let ((stack (get-text-property (point) 'geben-stack-frame)))
    (when stack
      (geben-display-context (plist-get stack :level)))))

