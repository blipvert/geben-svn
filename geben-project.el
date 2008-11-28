;;--------------------------------------------------------------
;; structures
;;--------------------------------------------------------------

(require 'cl)
(require 'geben-common)

(defstruct (geben-abstract-project)
  accept-language			; symbol or nil 
  feature
  breakpoint
  redirect
  fileurl)

(defstruct (geben-project
	    (:include geben-abstract-project))
  listen-port				; integer
  accept-idekey				; string or nil
  accept-ip)				; string or nil

(defstruct (geben-proxy-project
	    (:include geben-abstract-project))
  proxy-addr
  proxy-port
  proxy-idekey
  proxy-multi-session)

(defvar geben-projects
  (list (make-geben-project :listen-port 9000)
	(make-geben-proxy-project :proxy-addr "localhost"
				  :proxy-port 9001
				  :proxy-idekey "hoge"
				  :proxy-multi-session t)))

(defvar geben-running-projects nil
  "")

(provide 'geben-project)
