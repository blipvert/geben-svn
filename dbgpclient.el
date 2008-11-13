;;; dbgp.el --- DBGp protocol interface
;; $Id: $
;; 
;; Filename: dbgp.el
;; Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: reedom <fujinaka.tohru@gmail.com>
;; Version: 0.20
;; URL: http://code.google.com/p/geben-on-emacs/
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo
;; Compatibility: Emacs 22.1
;;
;; This file is not part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(eval-when-compile
  (when (or (not (boundp 'emacs-version))
	    (string< emacs-version "22.1"))
    (error (concat "geben.el: This package requires Emacs 22.1 or later."))))

(eval-and-compile
  (require 'cl)
  (require 'comint)
  (require 'xml))

;;--------------------------------------------------------------
;; customization
;;--------------------------------------------------------------

;; For compatibility between versions of custom
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	   ;; Some XEmacsen w/ custom don't have :set keyword.
	   ;; This protects them against custom.
	   (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (if (boundp 'defgroup)
	nil
      (defmacro defgroup (&rest args)
	nil))
    (if (boundp 'defcustom)
	nil
      (defmacro defcustom (var value doc &rest args)
	`(defvar (,var) (,value) (,doc))))))

;; customize group

(defgroup dbgp nil
  "DBGp protocol interface."
  :group 'debug)

(defgroup dbgp-highlighting-faces nil
  "Faces for DBGp process buffer."
  :group 'dbgp
  :group 'font-lock-highlighting-faces)

(defcustom dbgp-default-port 9000
  "DBGp listener's default port number."
  :type 'integer
  :group 'dbgp)

(defcustom dbgp-local-address "127.0.0.1"
  "Local host address. It is used for DBGp proxy.
This value is passed to DBGp proxy at connection negotiation.
When the proxy receive a new debugging session, the proxy tries
to connect to DBGp listener of this address."
  :type 'string
  :group 'dbgp)

;;--------------------------------------------------------------
;; macro
;;--------------------------------------------------------------

(defmacro dbgp-xml-get-error-message (xml)
  `(car
    (xml-node-children
     (car
      (xml-node-children
       (car
	(xml-get-children ,xml 'error)))))))

(defmacro dbgp-find-listener (port)
  `(find-if (lambda (listener)
	      (eq ,port (second (process-contact listener))))
	    dbgp-listeners))

(defmacro dbgp-make-listner-name (port)
  `(format "DBGp listener<%d>" ,port))

(defmacro dbgp-kill-process (proc)
  "Kill DBGp process PROC."
  `(if (memq (process-status ,proc) '(listen open))
       (delete-process ,proc))
  ;;  (ignore-errors
  ;;    (with-temp-buffer
  ;;      (set-process-buffer proc (current-buffer)))))
  )
  
(defmacro dbgp-proxy-p (listener)
  `(process-get listener :proxy))

;;--------------------------------------------------------------
;; DBGp
;;--------------------------------------------------------------

(defcustom dbgp-command-prompt "(cmd) "
  "DBGp client process buffer's command line prompt to display."
  :type 'string
  :group 'dbgp)

;;--------------------------------------------------------------
;; DBGp listener process
;;--------------------------------------------------------------

;; -- What is DBGp listener process --
;;
;; DBGp listener process is a network connection, as an entry point
;; for DBGp protocol connection.
;; The process listens at a specific network address to a specific
;; port for a new session connection(from debugger engine) coming.
;; When a new connection has accepted, the DBGp listener creates
;; a new DBGp session process. Then the new process takes over
;; the connection and the DBGp listener process starts listening
;; for another connection.
;;
;; -- DBGp listener custom properties --
;;
;; :session-init	default function for a new DBGp session 
;;			process to initialize a new session.
;; :session-filter	default function for a new DBGp session 
;;			process to filter protocol messages.
;; :session-sentinel	default function for a new DBGp session 
;;			called when the session is disconnected.

(defvar dbgp-listeners nil
  "List of DBGp listener processes.

DBGp listener process is a network connection, as an entry point
for DBGp protocol connection.
The process listens at a specific network address to a specific
port for a new session connection(from debugger engine) coming.
When a new connection has accepted, the DBGp listener creates
a new DBGp session process. Then the new process takes over
the connection and the DBGp listener process starts listening
for another connection.

-- DBGp listener process custom properties --

:session-init		default function for a new DBGp session 
			process to initialize a new session.
:session-filter		default function for a new DBGp session 
			process to filter protocol messages.
:session-sentinel	default function for a new DBGp session 
			called when the session is disconnected.
:proxy			if the listener is created for a proxy
			connection, this value has a plist of
			(:addr :port :idekey :multi-session).
			Otherwise the value is nil.
")

(defvar dbgp-sessions nil
  "List of DBGp session processes.

DBGp session process is a network connection, talks with a DBGp
debugger engine.

A DBGp session process is created by a DBGp listener process
after a DBGp session connection from a DBGp debugger engine
is accepted.
The session process is alive until the session is disconnected.

-- DBGp session process custom properties --

:listener		The listener process which creates this
			session process.
")

(defvar dbgp-proxy-address-history nil)
(defvar dbgp-listener-port-history nil)
(defvar dbgp-proxy-idekey-history nil)

;;--------------------------------------------------------------
;; interactive read functions
;;--------------------------------------------------------------

(defun dbgp-read-string (prompt &optional initial-input history default-value)
  "Read a string from the terminal, not allowing blanks.
Prompt with PROMPT.  Whitespace terminates the input.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  This argument has been superseded by DEFAULT-VALUE and should normally
  be nil in new code.  It behaves as in `read-from-minibuffer'.  See the
  documentation string of that function for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
"
  (let (str
	(temp-history (and history
			   (copy-list (symbol-value history)))))
     (while
	 (progn
	   (setq str (read-string prompt initial-input 'temp-history default-value))
	   (if (zerop (length str))
	       (setq str (or default-value ""))
	     (setq str (replace-regexp-in-string "^[ \t\r\n]+" "" str))
	     (setq str (replace-regexp-in-string "[ \t\r\n]+$" "" str)))
	   (zerop (length str))))
     (and history
	  (set history (cons str (remove str (symbol-value history)))))
     str))
      
(defun dbgp-read-integer (prompt &optional default history)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of HISTORY argument."
  (let (n
	(temp-history (and history
			   (copy-list (symbol-value history)))))
    (while
	(let ((str (read-string prompt nil 'temp-history default)))
	  (ignore-errors
	    (setq n (cond
		     ((numberp str) str)
		     ((zerop (length str)) default)
		     ((stringp str) (read str)))))
	  (unless (integerp n)
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    (and history
	 (set history (cons n (remq n (symbol-value history)))))
    n))
      
;;--------------------------------------------------------------
;; DBGp listener start/stop
;;--------------------------------------------------------------

;;;###autoload
(defun dbgp-start (port)
  "Start a new DBGp listener listening to PORT."
  (interactive (let (;;(addrs (mapcar (lambda (intf)
		     ;;		      (format-network-address (cdar (network-interface-list)) t))
		     ;;		    (network-interface-list)))
		     ;;(addr-default (or (car dbgp-listener-address-history)
		     ;;		       (and (member "127.0.0.1" addrs)
		     ;;			    "127.0.0.1")
		     ;;		       (car addrs)))
		     (port-default (or (car dbgp-listener-port-history)
				    9000)))
;;		 (or addrs
;;		     (error "This machine has no network interface to bind."))
		 (list
;;		  (completing-read (format "Listener address to bind(default %s): " default)
;;				   addrs nil t
;;				   'dbgp-listener-address-history default)
		  (dbgp-read-integer (format "Listen port(default %s): " port-default)
				     port-default 'dbgp-listener-port-history))))
  (let ((result (dbgp-exec port
			   nil
			   'dbgp-default-chunk-filter
			   'dbgp-default-chunk-sentinel)))
    (when (interactive-p)
      (message (cdr result)))
    result))

;;;###autoload
(defun dbgp-exec (port init filter sentinel)
  "Start a new DBGp listener listening to PORT."
  (if (dbgp-listener-alive-p port)
      (cons (dbgp-find-listener port)
	    (format "The DBGp listener for %d has already been started." port))
    (let ((listener (make-network-process :name (dbgp-make-listner-name port)
					  :server 1
					  :service port
					  :host "localhost"
					  :nowait t
					  :noquery t
					  :filter 'dbgp-setup-comint
					  :sentinel 'dbgp-listener-sentinel
					  :log 'dbgp-listener-log)))
	(unless listener
	  (error "Failed to create DBGp listener for port %d" port))
	(process-put listener :session-init init)
	(process-put listener :session-filter filter)
	(process-put listener :session-sentinel sentinel)
	(setq dbgp-listeners (cons listener
				   (remq (dbgp-find-listener port) dbgp-listeners)))
	(cons listener
	      (format "The DBGp listener for %d is started." port)))))

(defun dbgp-stop (port &optional include-proxy)
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
      (read (completing-read "Listener port: " ports nil t
			     (and (eq 1 (length ports))
				  (car ports))))
      current-prefix-arg)))
  (let ((listener (dbgp-find-listener port)))
    (dbgp-kill-listener port)
    (and (interactive-p)
	 (message (if listener
		      "The DBGp listener for port %d is terminated." 
		    "DBGp listener for port %d does not exist.")
		  port))
    (and listener t)))

(defun dbgp-kill-listener (port)
  (let ((listener (dbgp-find-listener port)))
    (when listener
      (delete-process listener))))

;;--------------------------------------------------------------
;; DBGp proxy listener register/unregister
;;--------------------------------------------------------------

;;;###autoload
(defun dbgp-register-proxy (proxy-ip-or-addr proxy-port idekey multi-session-p)
  "Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at PROXY-IP-OR-ADDR / PROXY-PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY."
  (interactive (list
		(let ((default (or (car dbgp-proxy-address-history) "localhost")))
		  (dbgp-read-string (format "Proxy address (default %s): " default)
				    nil 'dbgp-proxy-address-history default))
		(let ((default (or (car dbgp-listener-port-history) 9001)))
		  (dbgp-read-integer (format "Proxy port (default %d): " default)
				     default 'dbgp-listener-port-history))
		(dbgp-read-string "IDE key: " nil 'dbgp-proxy-idekey-history)
		(not (memq (read-char "Multi session(Y/n): ") '(?N ?n)))))
  (let* ((result (dbgp-proxy-register proxy-ip-or-addr proxy-port idekey multi-session-p
				      nil
				      'dbgp-default-chunk-filter
				      'dbgp-default-chunk-sentinel))
	 (status (cons result
		       (cond
			((processp result)
			 (format "New DBGp proxy listener is registered. idekey: `%s'" idekey))
			((stringp result)
			 (format "DBGp proxy returns an error: %s" result))
			((eq :proxy-not-found result)
			 (format "Cannot connect to DBGp proxy \"%s:%s\"." proxy-ip-or-addr proxy-port))
			((eq :no-response result)
			 "DBGp proxy responds no message.")
			((eq :invalid-xml result)
			 "DBGp proxy responds with invalid XML.")))))
    (and (interactive-p)
	 (cdr status)
	 (message (cdr status)))
    status))

;;;###autoload
(defun dbgp-proxy-register (ip-or-addr port idekey multi-session-p
				     session-init session-filter session-sentinel)
  "Register a new DBGp listener to an external DBGp proxy.
The proxy should be found at IP-OR-ADDR / PORT.
This create a new DBGp listener and register it to the proxy
associating with the IDEKEY."
  (let* ((listener-proc (make-network-process :name "DBGp proxy listener"
					      :server t
					      :service t
					      :family 'ipv4
					      :noquery t
					      :filter 'dbgp-setup-comint
					      :sentinel 'dbgp-listener-sentinel))
	 (listener-port (second (process-contact listener-proc)))
	 (result (dbgp-proxy-send-command ip-or-addr port
					  (format "proxyinit -a %s:%s -k %s -m %d"
						  dbgp-local-address listener-port idekey
						  (if multi-session-p 1 0)))))
    (if (and (consp result)
	     (not (equal "1" (xml-get-attribute result 'success))))
	(setq result (dbgp-proxy-send-command ip-or-addr port
					      (format "proxyinit -p %s -k %s -m %d"
						      listener-port idekey
						      (if multi-session-p 1 0)))))
    (if (not (and (consp result)
		  (equal "1" (xml-get-attribute result 'success))))
	(progn
	  (dbgp-kill-process listener-proc)
	  (if (not (consp result))
	      result
	    (dbgp-xml-get-error-message result)))
      (process-put listener-proc :proxy (list :addr ip-or-addr
					      :port port
					      :idekey idekey
					      :multi-session multi-session-p))
      (process-put listener-proc :session-init session-init)
      (process-put listener-proc :session-filter session-filter)
      (process-put listener-proc :session-sentinel session-sentinel)
      (process-put listener-proc :listener listener-proc)
      (setq dbgp-listeners (cons listener-proc dbgp-listeners))
      listener-proc)))

;;;###autoload
(defun dbgp-unregister-proxy (idekey &optional proxy-ip-or-addr proxy-port)
  "Unregister the DBGp listener associated with IDEKEY from a DBGp proxy.
After unregistration, it kills the listener instance."
  (interactive
   (let (proxies idekeys idekey)
     ;; collect idekeys.
     (mapc (lambda (listener)
	     (let ((proxy (process-get listener :proxy)))
	       (and proxy
		    (setq proxies (cons listener proxies))
		    (add-to-list 'idekeys (plist-get proxy :idekey)))))
	   dbgp-listeners)
     (or proxies
	 (error "No DBGp proxy listener exists."))
     ;; ask user for the target idekey.
     (setq idekey (completing-read "IDE key: " idekeys nil t
				   (and (eq 1 (length idekeys))
					(car idekeys))))
     ;; filter proxies and leave ones having the selected ideky.
     (setq proxies (remove-if (lambda (proxy)
				(not (equal idekey (plist-get (process-get proxy :proxy) :idekey))))
			      proxies))
     (let ((proxy (if (= 1 (length proxies))
		      ;; solo proxy.
		      (car proxies)
		    ;; two or more proxies has the same ideky.
		    ;; ask user to select a proxy unregister from.
		    (let* ((addrs (mapcar (lambda (proxy)
					    (let ((prop (process-get proxy :proxy)))
					      (format "%s:%s" (plist-get prop :addr) (plist-get prop :port))))
					  proxies))
			   (addr (completing-read "Proxy candidates: " addrs nil t (car addrs)))
			   (pos (position addr addrs)))
		      (and pos
			   (nth pos proxies))))))
       (list idekey
	     (plist-get (process-get proxy :proxy) :addr)
	     (plist-get (process-get proxy :proxy) :port)))))

  (let* ((proxies
	  (remq nil
		(mapcar (lambda (listener)
			  (let ((prop (process-get listener :proxy)))
			    (and prop
				 (equal idekey (plist-get prop :idekey))
				 (or (not proxy-ip-or-addr)
				     (equal proxy-ip-or-addr (plist-get prop :addr)))
				 (or (not proxy-port)
				     (equal proxy-port (plist-get prop :port)))
				 listener)))
			dbgp-listeners)))
	 (proxy (if (< 1 (length proxies))
		    (error "Multiple proxies are found. Needs more parameters to determine for unregistration.")
		  (car proxies)))
	 (result (and proxy
		      (dbgp-proxy-unregister proxy)))
	 (status (cons result
		       (cond
			((processp result)
			 (format "The DBGp proxy listener of `%s' is unregistered." idekey))
			((null result)
			 (format "DBGp proxy listener of `%s' is not registered." idekey))
			((stringp result)
			 (format "DBGp proxy returns an error: %s" result))
			((eq :proxy-not-found result)
			 (format "Cannot connect to DBGp proxy \"%s:%s\"." proxy-ip-or-addr proxy-port))
			((eq :no-response result)
			 "DBGp proxy responds no message.")
			((eq :invalid-xml result)
			 "DBGp proxy responds with invalid XML.")))))
    (and (interactive-p)
	 (cdr status)
	 (message (cdr status)))
    status))
  
;;;###autoload
(defun dbgp-proxy-unregister (proxy)
  "Unregister PROXY from a DBGp proxy.
After unregistration, it kills the listener instance."
  (with-temp-buffer
    (let* ((prop (process-get proxy :proxy))
	   (result (dbgp-proxy-send-command (plist-get prop :addr)
					    (plist-get prop :port)
					    (format "proxystop -k %s" (plist-get prop :idekey)))))
      ;; no matter of the result, remove proxy listener from the dbgp-listeners list.
      (dbgp-kill-process proxy)
      (if (consp result)
	  (or (equal "1" (xml-get-attribute result 'success))
	      (dbgp-xml-get-error-message result))
	result))))
		    
(defun dbgp-kill-all-sessions ()
  (interactive)
  (mapc 'delete-process dbgp-sessions)
  (setq dbgp-sessions nil))

;;--------------------------------------------------------------
;; DBGp listener functions
;;--------------------------------------------------------------

(defun dbgp-proxy-send-command (addr port string)
  "Send DBGp proxy command string to an external DBGp proxy.
ADDR and PORT is the address of the target proxy.
This function returns an xml list if the command succeeds,
or a symbol: `:proxy-not-found', `:no-response', or `:invalid-xml'."
  (with-temp-buffer
    (let ((proc (ignore-errors
		  (make-network-process :name "DBGp proxy negotiator"
					:buffer (current-buffer)
					:host addr
					:service port
					:sentinel (lambda (proc string) ""))))
	  xml)
      (if (null proc)
	  :proxy-not-found
	(process-send-string proc string)
	(dotimes (x 50)
	  (if (= (point-min) (point-max))
	      (sit-for 0.1 t)))
	(if (= (point-min) (point-max))
	    :no-response
	  (or (ignore-errors
		(setq xml (car (xml-parse-region (point-min) (point-max)))))
	      :invalid-xml))))))

(defun dbgp-listener-alive-p (port)
  "Return t if any listener for POST is alive."
  (let ((listener (dbgp-find-listener port)))
    (and listener
	 (eq 'listen (process-status listener)))))
  
;;--------------------------------------------------------------
;; DBGp listener process log and sentinel
;;--------------------------------------------------------------

(defun dbgp-listener-sentinel (proc string)
  (with-current-buffer (get-buffer-create "*DBGp Listener*")
    (insert (format "[SNT] %S %s\n" proc string)))
  (setq dbgp-listeners (remq proc dbgp-listeners)))

(defun dbgp-listener-log (proc string)
  (with-current-buffer (get-buffer-create "*DBGp Listener*")
    (insert (format "[LOG] %S %s\n" proc string))))

;;--------------------------------------------------------------
;; DBGp session process filter and sentinel
;;--------------------------------------------------------------

(defvar dbgp-filter-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")
(defvar dbgp-filter-defer-faced nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar dbgp-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar debugclient-delete-prompt-marker nil)

(defvar dbgp-buffer-process nil
  "")
(put 'dbgp-buffer-process 'permanent-local t)

(defadvice open-network-stream (around debugclient-pass-process-to-comint)
  "[comint hack] Pass the spawned DBGp client process to comint."
  (let* ((buffer (ad-get-arg 1))
	 (proc (buffer-local-value 'dbgp-buffer-process buffer)))
    (set-process-buffer proc buffer)
    (setq ad-return-value proc)))

(defun dbgp-setup-comint (proc string)
  "Setup a new comint buffer for a newly created session process PROC.
This is the first filter function for a new session process created by a
listener process. After the setup is done, `dbgp-session-filter' function
takes over the filter."
  (if (or (and (process-get proc :proxy)
	       (not (plist-get (process-get proc :proxy) :multi-session)))
	  (and (not (process-get proc :proxy))
	       (not (process-get proc :multi-session))))
      ;; multi session is disabled
      (progn
	(process-send-eof proc)
	(dbgp-kill-process proc))
    
    (setq dbgp-sessions (cons proc dbgp-sessions))
    ;; initialize sub process
    (set-process-query-on-exit-flag proc nil)

    (let* ((listener (process-get proc :listener))
	   (buffer-name (format "DBGp <%s:%s>"
				(first (process-contact proc))
			       (second (process-contact listener))))
	   (buf (or (find-if (lambda (buf)
			       ;; find reusable buffer
			       (let ((proc (get-buffer-process buf)))
				 (and (buffer-local-value 'dbgp-buffer-process buf)
				      (not (and proc
						(eq 'open (process-status proc)))))))
			     (buffer-list))
		    (get-buffer-create buffer-name))))
      (with-current-buffer buf
	(rename-buffer buffer-name)
	;; store PROC to `dbgp-buffer-process'.
	;; later the adviced `open-network-stream' will pass it
	;; comint.
	(set (make-local-variable 'dbgp-buffer-process) proc)
	(set (make-local-variable 'dbgp-filter-defer-flag) nil)
	(set (make-local-variable 'dbgp-filter-defer-faced) nil)
	(set (make-local-variable 'dbgp-filter-pending-text) nil))
      ;; setup comint buffer
      (ad-activate 'open-network-stream)
      (unwind-protect
	  (make-comint-in-buffer "DBGp-Client" buf (cons t t))
	(ad-deactivate 'open-network-stream))
      ;; update PROC properties
      (set-process-filter proc #'dbgp-session-filter)
      (set-process-sentinel proc #'dbgp-session-sentinel)
      (with-current-buffer buf
	(set (make-local-variable 'debugclient-delete-prompt-marker)
	     (make-marker))
	;;(set (make-local-variable 'comint-use-prompt-regexp) t)
	;;(setq comint-prompt-regexp (concat "^" dbgp-command-prompt))
	(setq comint-input-sender 'dbgp-session-send-string)
	;; call initializer function
	(funcall (or (process-get listener :session-init)
		     'null)
		 proc))
      (dbgp-session-filter proc string))))

(defun dbgp-session-send-string (proc string)
  "Send a DBGp protocol STRING to PROC."
  (comint-send-string proc (concat string "\0")))

(defun dbgp-session-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let ((buf (process-buffer proc))
	(listener (process-get proc :listener))
	(session-filter (process-get proc :session-filter))
	output process-window chunks)
    (block dbgp-session-filter
      (unless (buffer-live-p buf)
	(return-from dbgp-session-filter))

      (with-current-buffer buf
	(when dbgp-filter-defer-flag
	  ;; If we can't process any text now,
	  ;; save it for later.
	  (setq dbgp-filter-defer-faced t
		dbgp-filter-pending-text (if dbgp-filter-pending-text
					     (concat dbgp-filter-pending-text string)
					   string))
	  (return-from dbgp-session-filter))
	
	;; If we have to ask a question during the processing,
	;; defer any additional text that comes from the debugger
	;; during that time.
	(setq dbgp-filter-defer-flag t)
	(setq dbgp-filter-defer-faced nil)

	(ignore-errors
	  ;; Process now any text we previously saved up.
	  (setq dbgp-filter-pending-text (if dbgp-filter-pending-text
					     (concat dbgp-filter-pending-text string)
					   string)
		chunks (dbgp-session-response-to-chunk))

	  ;; If we have been so requested, delete the debugger prompt.
	  (if (marker-buffer debugclient-delete-prompt-marker)
	      (save-restriction
		(widen)
		(let ((inhibit-read-only t))
		  (delete-region (process-mark proc)
				 debugclient-delete-prompt-marker)
		  (comint-update-fence)
		  (set-marker debugclient-delete-prompt-marker nil))))
		
	  ;; Save the process output, checking for source file markers.
	  (setq output
		(if (functionp session-filter)
		    (mapcar (lambda (chunk)
			      (funcall session-filter proc chunk))
			    chunks)
		  (apply 'concat chunks)))))
      ;; Let the comint filter do the actual insertion.
      ;; That lets us inherit various comint features.
      (ignore-errors
	(mapc (lambda (s)
		(comint-output-filter proc (concat s "\n")))
	      output)
	(comint-output-filter proc dbgp-command-prompt)))
    
    (if (with-current-buffer buf
	  (setq dbgp-filter-defer-flag nil)
	  dbgp-filter-defer-faced)
	(dbgp-session-filter proc ""))))

(defun dbgp-session-response-to-chunk ()
  (let* ((string dbgp-filter-pending-text)
	 (beg 0)
	 (end (length string))
	 (i 0)
	 len chunks)
    (while (< i end)
      (if (< 0 (elt string i))
	  (setq i (1+ i))
	(setq len (string-to-number (substring string beg i)))
	(setq i (1+ i))
	(when (< (+ i len) end)
	  (setq chunks (cons (substring string i (+ i len))
			     chunks))
	  ;; Remove chunk from `dbgp-filter-pending-text'.
	  ;; Avoid to use `end' because `dbgp-filter-pending-text' may
	  ;; be expanded during processing this loop. (Still I'm not sure..)
	  (setq dbgp-filter-pending-text
		(if (< (+ i len 1) (length dbgp-filter-pending-text))
		    (substring dbgp-filter-pending-text (+ i len 1))
		  nil))
	  (setq i (+ i len 1))
	  (setq beg i))))
    (nreverse chunks)))

(defun dbgp-session-sentinel (proc string)
  (let ((sentinel (process-get proc :session-sentinel)))
    (ignore-errors
      (and (functionp sentinel)
	   (funcall sentinel proc string))))
  (setq dbgp-sessions (remq proc dbgp-sessions)))

;;--------------------------------------------------------------
;; default session chunk filter and sentinel
;;--------------------------------------------------------------
(defun dbgp-default-chunk-filter (proc string)
  (with-temp-buffer
    (insert string)
    (let ((xml (xml-parse-region (point-min) (point-max))))
      (erase-buffer)
      (when (string-match "^.*?\\?>" string)
	(insert (match-string 0 string))
	(insert "\n"))
      (xml-print xml)
      (buffer-string))))

(defun dbgp-default-chunk-sentinel (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert "\nDisconnected.\n\n"))))

(provide 'dbgp)