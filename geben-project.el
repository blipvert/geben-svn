(require 'cl)
(require 'geben-common)

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

(provide 'geben-project)
