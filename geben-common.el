;;; geben.el --- DBGp protocol frontend, a script debugger
;; $Id$
;; 
;; Filename: geben.el
;; Author: reedom <fujinaka.tohru@gmail.com>
;; Maintainer: reedom <fujinaka.tohru@gmail.com>
;; Version: 0.26
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
;; GEBEN is a software package that interfaces Emacs to DBGp protocol
;; with which you can debug running scripts interactive. At this present
;; DBGp protocol are supported in several script languages with help of
;; custom extensions.
;;
;;; Usage
;;
;; 1. Insert autoload hooks into your .Emacs file.
;;    -> (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)
;; 2. Start GEBEN. By default, M-x geben will start it.
;;    GEBEN starts to listening to DBGp protocol session connection.
;; 3. Run debuggee script.
;;    When the connection is established, GEBEN loads the entry script
;;    file in geben-mode.
;; 4. Start debugging. To see geben-mode key bindings, type ?.
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
  (when (or (not (boundp 'emacs-version))
	    (string< emacs-version "22.1"))
    (error (concat "geben.el: This package requires Emacs 22.1 or later."))))

(eval-and-compile
  (require 'cl)
  (require 'xml)
  (require 'tree-widget)
  (require 'dbgp))

(defvar geben-version "0.24")

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

(defgroup geben nil
  "A PHP Debugging environment."
  :group 'debug)

(defgroup geben-highlighting-faces nil
  "Faces for GEBEN."
  :group 'geben
  :group 'font-lock-highlighting-faces)

;; display window behavior

(defvar geben-dynamic-property-buffer-p nil)

(defcustom geben-display-window-function 'pop-to-buffer
  "*Function to display a debuggee script's content.
Typically `pop-to-buffer' or `switch-to-buffer'."
  :group 'geben
  :type 'function)

(defsubst geben-dbgp-dynamic-property-bufferp (buf)
  (with-current-buffer buf
    (symbol-value 'geben-dynamic-property-buffer-p)))

(defun geben-dbgp-display-window (buf)
  "Display a buffer anywhere in a window, depends on the circumstance."
  (cond
   ((get-buffer-window buf)
    (select-window (get-buffer-window buf))
    (switch-to-buffer buf))
   ((or (eq 1 (count-windows))
	(not (geben-dbgp-dynamic-property-buffer-visiblep)))
    (funcall geben-display-window-function buf))
   (t
    (let ((candidates (make-vector 3 nil))
	  (dynamic-p (geben-dbgp-dynamic-property-bufferp buf)))
      (block finder
	     (walk-windows (lambda (window)
			     (if (geben-dbgp-dynamic-property-bufferp (window-buffer window))
				 (if dynamic-p
				     (unless (aref candidates 1)
				       (aset candidates 1 window)))
			       (if (eq (selected-window) window)
				   (aset candidates 2 window)
				 (aset candidates 0 window)
				 (return-from finder))))))
      (select-window (or (aref candidates 0)
			 (aref candidates 1)
			 (aref candidates 2)
			 (selected-window)))
      (switch-to-buffer buf))))
  buf)

;;  (when (buffer-live-p buf)
;;    (or (eq buf (get-buffer geben-context-buffer-name))
;;	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name session :stdout)))
;;	(eq buf (get-buffer (geben-dbgp-redirect-buffer-name session :stderr))))))

(defun geben-dbgp-dynamic-property-buffer-visiblep ()
  "Check whether any window displays any property buffer."
  (block walk-loop
	 (walk-windows (lambda (window)
			 (if (geben-dbgp-dynamic-property-bufferp (window-buffer window))
			   (return-from walk-loop t))))))

(provide 'geben-common)