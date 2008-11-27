(defface geben-cursor-arrow-face
  '((((class color))
     :foreground "cyan"))
  "Face to displaying arrow indicator."
  :group 'geben-highlighting-faces)
 
(defun geben-cursor-indicate (fileuri lineno &optional display-bufferp)
  "Display indication marker at the current breaking point.
if DISPLAY-BUFFERP is non-nil, the buffer contains the breaking point
will be displayed in a window."
  (let ((local-path (geben-session-source-local-path session
						 (geben-dbgp-regularize-fileuri fileuri) t)))
    (if local-path
	(prog1
	    (geben-dbgp-indicate-current-line-1 local-path lineno)
	  ;;(and display-bufferp
	  (and t
	       (geben-dbgp-indicate-now)))
      (geben-dbgp-cmd-sequence
       (geben-dbgp-command-source fileuri)
       (fileuri lineno)
       (lambda (session cmd msg)
	 (when (not err)
	   (geben-dbgp-indicate-current-line-1
	    (geben-session-local-path-of session fileuri) lineno)
	   (geben-dbgp-indicate-now))))
      nil)))

(defun geben-dbgp-indicate-current-line-1 (local-path lineno)
  "Display current debugging position marker."
  (let ((lineno-1 (cond
		   ((numberp lineno)
		    lineno)
		   ((stringp lineno)
		    (string-to-number lineno)))))
    (when lineno-1
      (when (floatp lineno-1)
	(setq lineno-1 1)) ;; restrict to integer
      (setq geben-dbgp-indicator-position (cons local-path lineno-1))
      (message "stopped: %s(%S)"
	       (file-name-nondirectory local-path) lineno-1))))

(defun geben-dbgp-indicate-now ()
  (if (null geben-dbgp-indicator-position)
      (when (overlayp geben-dbgp-overlay-arrow)
	(delete-overlay geben-dbgp-overlay-arrow)
	(setq geben-dbgp-overlay-arrow nil))
    (let ((buf (geben-dbgp-find-file (car geben-dbgp-indicator-position)))
	  pos)
      (when buf
	(ignore-errors
	  (save-restriction
	    (widen)
	    (goto-line (cdr geben-dbgp-indicator-position))
	    (setq pos (point))
	    (if geben-dbgp-overlay-arrow
		(move-overlay geben-dbgp-overlay-arrow pos pos)
	      (setq geben-dbgp-overlay-arrow (make-overlay pos pos buf))
	      (overlay-put geben-dbgp-overlay-arrow
			   'before-string
			   (propertize "x"
				       'display
				       (list
					'(margin left-margin)
					(propertize "=>"
						    'face 'geben-dbgp-arrow-face))))))
	  (set-window-point (get-buffer-window buf) pos))))))

