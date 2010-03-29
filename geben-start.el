(require 'geben-dbgp-init)
(require 'geben-mode)

(defcustom geben-dbgp-default-port 9000
  "Default port number to listen a new DBGp connection."
  :group 'geben
  :type 'integer)

(defcustom geben-dbgp-default-proxy '("127.0.0.1" 9001 "default" nil t)
  "Default setting for a new DBGp proxy connection.

The first and second elements are address and port where the DBGp proxy listening on.
The third element is IDE key.
The forth element is a flag but currently not used yet.
The fifth element is port to be used in debugging sessions. If a non-integer value is
set, then any free port will be allocated.
"
  :group 'geben)

;;;###autoload
(defun geben (&optional args)
  "Start GEBEN, a DBGp protocol frontend - a script debugger.
Variations are described below.

By default, starts GEBEN listening to port `geben-dbgp-default-port'.
Prefixed with one \\[universal-argument], asks listening port number interactively and
starts GEBEN on the port.
Prefixed with two \\[universal-argument]'s, starts a GEBEN proxy listener.
Prefixed with three \\[universal-argument]'s, kills a GEBEN listener.
Prefixed with four \\[universal-argument]'s, kills a GEBEN proxy listener.

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
    (64
     (call-interactively 'geben-end))
    (t
     (call-interactively 'geben-proxy-end))))

(defun geben-end (port)
  "Stop the DBGp listener on PORT."
  (interactive
   (let ((ports (remq nil
		      (mapcar (lambda (listener)
				(and (not (dbgp-proxy-p listener))
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

(defun geben-proxy (ip-or-addr port idekey ;;multi-session-p
			       &optional session-port)
  "Start a new DBGp proxy listener.
The DBGp proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY."
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
		(let ((default (or (car dbgp-proxy-session-port-history)
				   (nth 4 geben-dbgp-default-proxy)
				   (nth 4 (default-value 'geben-dbgp-default-proxy)))))
		  (unless (numberp default)
		    (setq default 0))
		  (dbgp-read-integer (format "Port for debug session (%s): "
					     (if (< 0 default)
						 (format "default %d, 0 to use any free port" default)
					       (format "leave empty to use any free port")))
				     default 'dbgp-proxy-session-port-history))))
  (geben-dbgp-start-proxy ip-or-addr port idekey ;;multi-session-p
			  session-port))

(defalias 'geben-proxy-end #'dbgp-proxy-unregister)

(provide 'geben)