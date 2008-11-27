;;--------------------------------------------------------------
;; cmd hash
;;--------------------------------------------------------------

(require 'cl)
(require 'geben-util)
(require 'geben-session)

(defmacro geben-cmd-param-for (key)
  `(plist-get '(:depth "-d"
		       :context-id "-c"
		       :max-data-size "-m"
		       :type "-t"
		       :page "-p"
		       :key "k"
		       :address "-a"
		       :name "-n"
		       :fileuri "-f"
		       :lineno "-n"
		       :class "-a"
		       :function "-m"
		       :state "-s"
		       :exception "-x"
		       :hit-value "-h"
		       :hit-condition "-o"
		       :run-once "-r"
		       :expression "--")
	      ,key))

(defsubst geben-cmd-make (session operand params)
  "Create a new command object."
  (list :session session
	:tid (geben-session-next-tid session)
	:operand operand
	:param params))

(defun geben-cmd-remove (session tid)
  "Get a command object from the command hash table specified by TID."
  (let ((cmds (geben-session-cmd session)))
    (if (eq tid (plist-get (car cmds) :tid))
	(prog1
	    (car cmds)
	  (setf (geben-session-cmd session) (cdr cmds)))
      (let (match-cmd)
	(setf (geben-session-cmd session)
	      (remove-if (lambda (cmd)
			   (and (eq tid (plist-get cmd :tid))
				(setq match-cmd cmd)))
			 cmds))
	match-cmd))))

(defsubst geben-cmd-param-get (cmd flag)
  "Get FLAG's parameter used in CMD.
For a DBGp command \`stack_get -i 1 -d 2\',
`(geben-cmd-param-get cmd \"-d\")\' gets \"2\"."
  (cdr-safe (assoc flag (plist-get cmd :param))))

(defun geben-cmd-expand (cmd)
  "Build a send command string for DBGp protocol."
  (mapconcat #'(lambda (x)
		 (cond ((stringp x) x)
		       ((integerp x) (int-to-string x))
		       ((atom (format "%S" x)))
		       ((null x) "")
		       (t x)))
	     (geben-flatten (list (plist-get cmd :operand)
				  "-i"
				  (plist-get cmd :tid)
				  (plist-get cmd :param)))
	     " "))
  
(provide 'geben-cmd)
