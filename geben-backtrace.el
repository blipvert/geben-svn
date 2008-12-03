(require 'cl)
(require 'geben-common)
(require 'geben-session)
(require 'geben-cursor)
(require 'geben-source)
(require 'geben-dbgp)

;;==============================================================
;; stack
;;==============================================================

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

(defun geben-backtrace-buffer (session)
  (let ((buf (get-buffer-create (geben-session-buffer-get session geben-backtrace-buffer-name))))
    (unless (eq 'geben-backtrace-mode major-mode)
      (with-current-buffer buf
	(geben-backtrace-mode)))
    buf))

(defun geben-backtrace (session)
  "Display backtrace."
  (unless (geben-session-active-p session)
    (error "GEBEN is out of debugging session."))
  (with-current-buffer (geben-backtrace-buffer session)
    (let ((inhibit-read-only t)
	  (stack (geben-session-stack session)))
      (erase-buffer)
      (dotimes (i (length session))
	(let* ((stack (nth i stack))
	       (fileuri (geben-source-fileuri-regularize (xml-get-attribute stack 'filename)))
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
      (goto-char (point-min)))
    (geben-dbgp-display-window (geben-backtrace-buffer session))))

(defvar geben-backtrace-mode-map nil
  "Keymap for `geben-backtrace-mode'")
(unless geben-backtrace-mode-map
  (setq geben-backtrace-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-2] 'geben-backtrace-mode-mouse-goto)
	  (define-key map "\C-m" 'geben-backtrace-mode-goto)
	  (define-key map "q" 'geben-quit-window)
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
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'geben-backtrace-mode-hook)
    (run-hooks 'geben-backtrace-mode-hook)))

(defalias 'geben-backtrace-mode-mouse-goto 'geben-backtrace-mode-goto)
(defun geben-backtrace-mode-goto (&optional event)
  (interactive (list last-nonmenu-event))
  (geben-with-current-session session
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
	(geben-session-cursor-update session
				     (plist-get stack-frame :fileuri)
				     (plist-get stack-frame :lineno))))))

(defun geben-backtrace-mode-help ()
  "Display description and key bindings of `geben-backtrace-mode'."
  (interactive)
  (describe-function 'geben-backtrace-mode))

(defvar geben-dbgp-stack-update-hook nil)

(defun geben-backtrace-mode-context ()
  (interactive)
  (geben-with-current-session session
    (let ((stack (get-text-property (point) 'geben-stack-frame)))
      (when stack
	(run-hook-with-args 'geben-dbgp-stack-update-hook
			    session (plist-get stack :level))))))

;;; stack_get

(defun geben-dbgp-command-stack-get (session)
  "Send \`stack_get\' command."
  (geben-dbgp-send-command session "stack_get"))

(defun geben-dbgp-stack-update (session)
  (geben-dbgp-sequence
    (geben-dbgp-command-stack-get session)
    (lambda (session cmd msg err)
      (unless err
	(setf (geben-session-stack session) (xml-get-children msg 'stack))
	(let* ((stack (car (xml-get-children msg 'stack)))
	       (fileuri (xml-get-attribute-or-nil stack 'filename))
	       (lineno (xml-get-attribute-or-nil stack'lineno)))
	  (and fileuri lineno
	       (geben-session-cursor-update session fileuri lineno)))
	(run-hook-with-args 'geben-dbgp-stack-update-hook
			    session 0)))))

(add-hook 'geben-dbgp-continuous-command-hook
	  'geben-dbgp-stack-update)

(provide 'geben-backtrace)