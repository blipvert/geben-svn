(require 'cl)

(defcustom geben-temporary-file-directory (expand-file-name "geben" "~/emacs.d")
  "*Base directory path where GEBEN creates temporary files and directories."
  :group 'geben
  :type 'directory)

(defvar geben-storages nil)
(defvar geben-storage-loaded nil)

(defun geben-storage-load ()
  (let ((storage-path (expand-file-name ".storage"
					geben-temporary-file-directory)))
    (when (file-exists-p storage-path)
      (ignore-errors
	(with-temp-buffer
	  (insert-file-contents storage-path)
	  (setq geben-storages (read (buffer-string))))))))

(defun geben-storage-save ()
  (let ((storage-path (expand-file-name ".storage"
					geben-temporary-file-directory)))
    (with-temp-buffer
      (pp geben-storages (current-buffer))
      (with-temp-message ""
	(write-region (point-min) (point-max) storage-path)))))

(provide 'geben-storage)