(require 'cl)
(require 'geben-util)

;;==============================================================
;; DBGp related utilities
;;==============================================================

(defmacro* geben-dbgp-sequence (cmd &rest callback)
  (declare (indent 1))
  (list 'progn
	(list 'geben-plist-append cmd
	      :callback (car callback))))

(defmacro* geben-dbgp-sequence-bind (bindings cmd &rest callback)
  (declare (indent 1))
  (cl-macroexpand-all
   (list 'progn
	 (list 'geben-plist-append cmd
	       :callback (if bindings
			     (list 'geben-lexical-bind bindings (car callback))
			   (car callback))))))

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

(provide 'geben-dbgp-util)