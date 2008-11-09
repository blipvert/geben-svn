;;; dbgpclient.el --- DBGp protocol client
;; $Id: $
;; 
;; Filename: dbgpclient.el
;; Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: reedom <fujinaka.tohru@gmail.com>
;; Version: 0.19
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Requirements:
;;
;; [Server side]
;; - PHP with Xdebug 2.0.3
;;    http://xdebug.org/
;; - Perl, Python, Ruby, Tcl with Komodo Debugger Extension
;;    http://aspn.activestate.com/ASPN/Downloads/Komodo/RemoteDebugging
;;
;; [Client side]
;; - Emacs 22.1 and later
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(eval-when-compile
  (require 'cl)
  (require 'xml))

;;--------------------------------------------------------------
;; dbgpclient-process structure
;;--------------------------------------------------------------
(defstruct (dbgpclient-listener
	    (:conc-name dbgp-listener-))
  port
  process
  filter
  sentinel)

(defun dbgp-listener= (lhs rhs)
  "Return t if two `dbgpclient-process' structure is same object."
  (eq (dbgp-listener-process lhs)
      (dbgp-listener-process rhs)))

;;--------------------------------------------------------------
;; dbgpclient
;;--------------------------------------------------------------

(defconst dbgpclient-command-prompt "(cmd) ")

(defvar dbgpclient-buffer nil)
(defvar dbgpclient-processes nil)
(defvar dbgpclient-listener-table (make-hash-table :size 4)
  "Hash table of DBGp listeners.
KEY is listen port.
VALUE is `dbgpclient-process' struct.")

(defvar dbgpclient-buffer-process nil
  "")
(put 'dbgpclient-buffer-process 'permanent-local t)

(defadvice open-network-stream (around debugclient-pass-process-to-comint)
  "[comint hack] Pass the spawned DBGp client process to comint."
  (let* ((buffer (ad-get-arg 1))
	 (proc (buffer-local-value 'dbgpclient-buffer-process buffer)))
    (set-process-buffer proc buffer)
    (setq ad-return-value proc)))

(defmacro dbgpclient-listner-name (port)
  `(format "DBGp-Client(%d)" ,port))

(defun dbgpclient-kill-process (proc)
  (ignore-errors
    (with-temp-buffer
      (set-process-buffer proc (current-buffer)))))
  
(defun dbgpclient-kill-listener (port)
  (let ((dbgp-listener (gethash port dbgpclient-listener-table)))
    (when dbgp-listener
      (dbgpclient-kill-process (dbgp-listener-process dbgp-listener))
      (remhash port dbgpclient-listener-table)
      t)))

(defun dbgpclient-kill-all-processes ()
  (interactive)
  (mapc (lambda (proc)
	  (ignore-errors
	    (if (process-buffer proc)
		(kill-buffer (process-buffer proc))
	      (dbgpclient-kill-process proc))))
	dbgpclient-processes)
  (setq dbgpclient-processes nil))

(defun dbgpclient (port &optional terminate-p)
  (interactive (list
		(if current-prefix-arg
		  (read-number "Listener port to kill: "
			       (if (< 0 (hash-table-count dbgpclient-listener-table))
				   (let (first-found-port)
				     (maphash (lambda (port table)
						(if (null first-found-port)
						    (setq first-found-port port)))
					      dbgpclient-listener-table)
				     first-found-port)
				 9000))
		  (read-number "Listen port: " 9000))
		(and current-prefix-arg t)))
  (let ((result (if terminate-p
		    (dbgpclient-kill-listener port)
		  (and (not (dbgpclient-listener-alive-p port))
		       (dbgpclient-exec port
					'dbgpclient-default-chunk-filter
					'dbgpclient-default-chunk-sentinel)))))
    (when (interactive-p)
      (message (if terminate-p
		   (if result
		       "The DBGp client for port %d is terminated."
		     "DBGp client for port %d is not exists.")
		 (if result
		     "The DBGp client for %d is started."
		   "The DBGp client for %d has already been started."))
	       port))
    result))

(defun dbgpclient-exec (port filter sentinel)
  (let ((listener (gethash port dbgpclient-listener-table)))
    (unless (and listener
		 (eq 'listen (process-status (dbgp-listener-process listener))))
      (let ((proc (make-network-process :name (dbgpclient-listner-name port)
					:server 1
					:service port
					:family 'ipv4
					:nowait t
					:noquery t
					:filter 'dbgpclient-setup-comint
					:sentinel 'dbgpclient-listener-sentinel
					:log 'dbgpclient-listener-log)))
	(unless proc
	  (error "Failed to create DBGp client listener for port %d" port))
	(setq listener (make-dbgpclient-listener :port port
						 :process proc
						 :filter filter
						 :sentinel sentinel))
	(set-process-plist proc (list :port port :listener listener))
	(puthash port listener dbgpclient-listener-table))
      listener)))

(defun dbgpclient-listener-alive-p (port)
  (let ((listener (gethash port dbgpclient-listener-table)))
    (and listener
	 (eq 'listen (process-status (dbgp-listener-process listener))))))
  
(defun dbgpclient-listener-sentinel (proc string)
  (with-current-buffer (get-buffer-create "*DBGp Sentinel*")
    (insert (format "%S %s\n" proc string))))

(defun dbgpclient-listener-log (&rest args)
  (with-current-buffer (get-buffer-create "*DBGp Log*")
    (insert (format "%S\n" args))))

(defun dbgpclient-default-chunk-filter (proc string)
  (with-temp-buffer
    (insert string)
    (let ((xml (xml-parse-region (point-min) (point-max))))
      (erase-buffer)
      (when (string-match "^.*?\\?>" string)
	(insert (match-string 0 string))
	(insert "\n"))
      (xml-print xml)
      (buffer-string))))

(defun dbgpclient-default-chunk-sentinel (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert "\nDisconnected.\n\n")
      (setq dbgpclient-processes (delq proc dbgpclient-processes)))))

(defvar dbgpclient-filter-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")
(defvar dbgpclient-filter-defer-faced nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar dbgpclient-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `gud-filter'.")
(defvar debugclient-delete-prompt-marker nil)

(defun dbgpclient-send-string (proc string)
  (comint-send-string proc (concat string "\0")))

(defun dbgpclient-setup-comint (proc string)
  (if dbgpclient-processes
      (progn
	(process-send-eof proc)
	(dbgpclient-kill-process proc))
    (setq dbgpclient-processes (cons proc dbgpclient-processes))
    ;; initialize sub process
    (set-process-query-on-exit-flag proc nil)

    (let ((buf (if (buffer-live-p dbgpclient-buffer)
		   dbgpclient-buffer
		 (setq dbgpclient-buffer 
		       (generate-new-buffer (process-name proc)))))
	  (listener (plist-get (process-plist proc) :listener)))
      (with-current-buffer buf
	(rename-buffer (process-name proc) t)
	;; store PROC to `dbgpclient-buffer-process'.
	;; later the adviced `open-network-stream' will pass it
	;; comint.
	(set (make-local-variable 'dbgpclient-buffer-process) proc)
	(set (make-local-variable 'dbgpclient-filter-defer-flag) nil)
	(set (make-local-variable 'dbgpclient-filter-defer-faced) nil)
	(set (make-local-variable 'dbgpclient-filter-pending-text) nil))
      ;; setup comint buffer
      (ad-activate 'open-network-stream)
      (unwind-protect
	  (make-comint-in-buffer "DBGp-Client" buf (cons t t))
	(ad-deactivate 'open-network-stream))
      ;; update PROC properties
      (set-process-filter proc #'dbgpclient-process-filter)
      (set-process-sentinel proc (dbgp-listener-sentinel listener))
      (with-current-buffer buf
	(set (make-local-variable 'debugclient-delete-prompt-marker)
	     (make-marker))
	;;(set (make-local-variable 'comint-use-prompt-regexp) t)
	;;(setq comint-prompt-regexp (concat "^" dbgpclient-command-prompt))
	(setq comint-input-sender 'dbgpclient-send-string))
      (pop-to-buffer buf)
      (dbgpclient-process-filter proc string))))

(defun dbgpclient-split-dbgp-string ()
  (let* ((string dbgpclient-filter-pending-text)
	 (end (length string))
	 (i 0)
	 chunks)
    (while (< i end)
      (if (< 0 (elt string i))
	  (setq i (1+ i))
	(setq len (string-to-number (substring string 0 i)))
	(setq i (1+ i))
	(when (< (+ i len) end)
	  (setq chunks (cons (substring string i (+ i len))
			     chunks))
	  ;; Remove chunk from `dbgpclient-filter-pending-text'.
	  ;; Avoid to use `end' because `dbgpclient-filter-pending-text' may
	  ;; be expanded during processing this loop. (Still I'm not sure..)
	  (setq dbgpclient-filter-pending-text
		(if (< (+ i len 1) (length dbgpclient-filter-pending-text))
		    (substring dbgpclient-filter-pending-text (+ i len 1))
		  nil))
	  (setq i (+ i len 1)))))
    (nreverse chunks)))

(defun dbgpclient-process-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let ((buf (process-buffer proc))
	(listener (plist-get (process-plist proc) :listener))
	output process-window chunks)
    (block dbgpclient-process-filter
      (unless (buffer-live-p buf)
	(return-from dbgpclient-process-filter))

      (with-current-buffer buf
	(when dbgpclient-filter-defer-flag
	  ;; If we can't process any text now,
	  ;; save it for later.
	  (setq dbgpclient-filter-defer-faced t
		dbgpclient-filter-pending-text
		(if dbgpclient-filter-pending-text
		    (concat dbgpclient-filter-pending-text string)
		  string))
	  (return-from dbgpclient-process-filter))
	
	;; If we have to ask a question during the processing,
	;; defer any additional text that comes from the debugger
	;; during that time.
	(setq dbgpclient-filter-defer-flag t)
	(setq dbgpclient-filter-defer-faced nil)
	(ignore-errors
	  ;; Process now any text we previously saved up.
	  (setq dbgpclient-filter-pending-text
		(if dbgpclient-filter-pending-text
		    (concat dbgpclient-filter-pending-text string)
		  string))
	  (setq chunks (dbgpclient-split-dbgp-string))

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
	  (setq output (mapcar (lambda (chunk)
				 (funcall (dbgp-listener-filter listener) proc chunk))
			       chunks))))
      ;; Let the comint filter do the actual insertion.
      ;; That lets us inherit various comint features.
      (ignore-errors
	(mapc (lambda (s)
		(comint-output-filter proc (concat s "\n")))
	      output)
	(comint-output-filter proc dbgpclient-command-prompt)))
    
    (if (with-current-buffer buf
	  (setq dbgpclient-filter-defer-flag nil)
	  dbgpclient-filter-defer-faced)
	(dbgpclient-process-filter proc ""))))

