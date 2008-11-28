;;==============================================================
;; DBGp
;;==============================================================

;;;
;;; command/response handlers
;;;

;;-------------------------------------------------------------
;;  miscellaneous functions
;;-------------------------------------------------------------

;;
;; interactive commands
;;

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
