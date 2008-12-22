(require 'cl)
(require 'geben-common)
(require 'geben-session)
(require 'geben-dbgp-util)
(require 'geben-source)

;;==============================================================
;; cursor
;;==============================================================

(defface geben-cursor-arrow-face
  '((((class color))
     :inherit 'default
     :foreground "cyan"))
  "Face to displaying arrow indicator."
  :group 'geben-highlighting-faces)

(defun geben-session-cursor-update (session fileuri lineno)
  (let ((lineno (cond
		 ((numberp lineno)
		  lineno)
		 ((stringp lineno)
		  (string-to-number lineno))))
	(fileuri (geben-source-fileuri-regularize fileuri)))
    (and lineno
	 (floatp lineno)
	 (setq lineno 1))		; restrict to integer
    (plist-put (geben-session-cursor session) :position (cons fileuri lineno)))
  (geben-session-cursor-indicate session))

(defun geben-session-cursor-indicate (session)
  "Display indication marker at the current breaking point.
if DISPLAY-BUFFERP is non-nil, the buffer contains the breaking point
will be displayed in a window."
  (let* ((cursor (geben-session-cursor session))
	 (position (plist-get cursor :position))
	 (fileuri (car position))
	 (lineno (cdr position))
	 (local-path (geben-session-source-local-path session fileuri)))
    (if local-path
	(geben-session-cursor-overlay-update session)
      (geben-dbgp-sequence
	  (geben-dbgp-command-source session fileuri)
	(lambda (session cmd msg err)
	  (unless err
	    (geben-session-cursor-overlay-update session)))))))

(defun geben-session-cursor-overlay-update (session)
  (let* ((cursor (geben-session-cursor session))
	 (overlay (plist-get cursor :overlay))
	 (position (plist-get cursor :position))
	 (fileuri (car position))
	 (lineno (cdr position))
	 (local-path (and fileuri
			  (geben-session-source-local-path session fileuri))))
    (if (null position)
	(when (overlayp overlay)
	  (delete-overlay overlay)
	  (plist-put cursor :overlay nil))
      (let ((buf (geben-source-visit local-path))
	    pos)
	(when buf
	  (with-current-buffer buf
	    (ignore-errors
	      (save-restriction
		(widen)
		(goto-line lineno)
		(setq pos (point))
		(if (overlayp overlay)
		    (move-overlay overlay pos pos buf)
		  (plist-put cursor :overlay
			     (setq overlay (make-overlay pos pos buf)))
		  (overlay-put overlay
			       'before-string
			       (propertize "x"
					   'display
					   (list
					    '(margin left-margin)
					    (propertize "=>"
							'face 'geben-cursor-arrow-face))))))
	      (set-window-point (get-buffer-window buf) pos))))))))

(defun geben-session-cursor-file-visit-handler (session buf)
  (let ((cursor (geben-session-cursor session))
	(fileuri (geben-session-source-fileuri session (buffer-file-name buf))))
    (and fileuri
	 (equal fileuri (car (plist-get cursor :position)))
	 (geben-session-cursor-overlay-update session))))

(add-hook 'geben-source-visit-hook #'geben-session-cursor-file-visit-handler)

(provide 'geben-cursor)