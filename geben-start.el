(require 'geben-mode)

;;;###autoload
(defun geben (&optional quit)
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
  (interactive "P")
  (if quit
      (let (buf proc)
	(and (setq buf (get-buffer geben-process-buffer-name))
	     (setq proc (get-buffer-process buf))
	     (dbgp-stop proc)))
    (geben-dbgp-start geben-projects)))

(provide 'geben)