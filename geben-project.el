(require 'cl)
(require 'geben-common)

(defcustom geben-listener-connection-points
  '((:proxy nil :port 9000 :multi-session t)
    (:proxy t :addr "localhost" :port 9001 :idekey "hoge" :multi-session t))
  "DBGp listeners and DBGp proxy listeners list."
  :group 'geben)

(defvar geben-persist-sessions nil)
;;==============================================================
;; structures
;;==============================================================

(defstruct (geben-abstract-project
	    (:constructor nil))
  accept-language			; symbol or nil
  fileurl
  (content-coding-system 'utf-8)
  feature
  breakpoint
  redirect)

(defstruct (geben-project
	    (:constructor nil)
	    (:constructor geben-project-make)
	    (:include geben-abstract-project))
  listen-port				; integer
  accept-idekey				; string or nil
  accept-ip)				; string or nil

(defstruct (geben-proxy-project
	    (:constructor nil)
	    (:constructor geben-proxy-project-make)
	    (:include geben-abstract-project))
  addr
  port
  idekey
  multi-session)

(defvar geben-projects
  (list (geben-project-make :listen-port 9000)
	(geben-proxy-project-make :addr "localhost"
				  :port 9001
				  :idekey "hoge"
				  :multi-session t)))

(defvar geben-running-projects nil
  "")

(defsubst geben-dbgp-project-listen-addresses (projects)
  (delete-dups (mapc (lambda (project)
		       (if (geben-proxy-projext-p project)
			   (list :proxy
				 (geben-proxy-project-addr project)
				 (geben-proxy-project-port project))
			 (list :project
			       (geben-project-listen-port project))))
		     projects)))

(defun geben-project-find (session)
  (find-if (lambda (project)
	     )
	   geben-projects)
  
(provide 'geben-project)
