(require 'geben-dbgp-init)
(require 'geben-mode)

(defcustom geben-dbgp-default-port 9000
  "Default port number to listen a new DBGp connection."
  :group 'geben
  :type 'integer)

(defcustom geben-dbgp-default-proxy '("127.0.0.1" 9001 "default" nil)
  "Default setting for a new DBGp proxy connection."
  :group 'geben)

;;;###autoload
(defun geben (&optional args)
  "Start GEBEN, a PHP source level debugger.
Prefixed with \\[universal-argument], GEBEN quits immediately.

GEBEN communicates with script servers, located anywhere local or
remote, in DBGp protocol (e.g. PHP with Xdebug extension)
to help you debugging your script with some valuable features:
 - continuation commands like \`step in\', \`step out\', ...
 - a kind of breakpoints like \`line no\', \`function call\' and
   \`function return\'.
 - evaluation
 - stack dump
 - etc.

The script servers should be DBGp protocol enabled.
Ask to your script server administrator about this setting up
issue.

The variable `geben-dbgp-command-line' is a command line to
execute a DBGp protocol client command. GEBEN communicates with
script servers through this command.

Once you've done these setup operation correctly, run GEBEN first
and your script on your script server second. After some
negotiation GEBEN will display your script's entry source code.
The debugging session is started.

In the debugging session the source code buffers are under the
minor mode  `geben-mode'. Key mapping and other information is
described its help page."
  (interactive "p")
  (case args
    (1
     (geben-dbgp-start geben-dbgp-default-port))
    (4
     (let ((default (or (car dbgp-listener-port-history)
			geben-dbgp-default-port
			(default-value 'geben-dbgp-default-port))))
       (geben-dbgp-start (dbgp-read-integer (format "Listen port(default %s): " default)
					    default 'dbgp-listener-port-history))))
    (16
     (call-interactively 'geben-proxy))
    (t
     (call-interactively 'geben-end))))

(defun geben-proxy (ip-or-addr port idekey ;;multi-session-p
			       )
  (interactive (list
		(let ((default (or (car dbgp-proxy-address-history)
				   (nth 0 geben-dbgp-default-proxy)
				   (nth 0 (default-value 'geben-dbgp-default-proxy)))))
		  (dbgp-read-string (format "Proxy address (default %s): " default)
				    nil 'dbgp-proxy-address-history default))
		(let ((default (or (car dbgp-proxy-port-history)
				   (nth 1 geben-dbgp-default-proxy)
				   (nth 1 (default-value 'geben-dbgp-default-proxy)))))
		  (dbgp-read-integer (format "Proxy port (default %d): " default)
				     default 'dbgp-proxy-port-history))
		(let ((default (or (car dbgp-proxy-idekey-history)
				   (nth 2 geben-dbgp-default-proxy)
				   (nth 2 (default-value 'geben-dbgp-default-proxy)))))
		  (dbgp-read-string "IDE key: " nil 'dbgp-proxy-idekey-history))
		;;(not (memq (read-char "Multi session(Y/n): ") '(?N ?n)))
		))
  (geben-dbgp-start-proxy ip-or-addr port idekey ;;multi-session-p
			  ))
  
(defun geben-end (port)
  "Stop the DBGp listener listening to PORT."
  (interactive
   (let ((ports (remq nil
		      (mapcar (lambda (listener)
				(and (or current-prefix-arg
					 (not (dbgp-proxy-p listener)))
				     (number-to-string (second (process-contact listener)))))
			      dbgp-listeners))))
     (list
      ;; ask user for the target idekey.
      (read (completing-read "Listener port to kill: " ports nil t
			     (and (eq 1 (length ports))
				  (car ports)))))))
  (let ((listener (dbgp-listener-find port)))
    (dbgp-listener-kill port)
    (and (interactive-p)
	 (message (if listener
		      "The DBGp listener for port %d is terminated." 
		    "DBGp listener for port %d does not exist.")
		  port))
    (and listener t)))

(provide 'geben)