(require 'cl)
(require 'geben-util)

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

(provide 'geben-dbgp-util)