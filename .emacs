;;; .emacs --- init-file of emacs.
;;; Commentary:
;;; Code:

 ;; Load all configuration and packages.

(message "Start loading configurations...\n")
(push (expand-file-name "~/.emacs.d/rc") load-path)

;; Add customized paths to the front of load-path
(dolist (path '("~/.emacs.d/site-lisp/"
                "/usr/share/emacs/site-lisp"
                ))
  (when (file-exists-p path)
    (let ((default-directory path))
      (normal-top-level-add-subdirs-to-load-path))))

;; Never load the `central-custome-file'.
(setq custom-file "~/.emacs.d/rc/10-emacs-custome.el")
(if (file-exists-p custom-file)
    (delete-file custom-file nil))

(dolist (fn (directory-files "~/.emacs.d/rc/" t "^[0-9]+.*?\.el$"))
  (condition-case load-message
      (load fn)
    ('error  (message "Skipped file %s -- %s" fn load-message)))
  )

;; set color immediately.
(if (fboundp 'yc/setup-display)
    (yc/setup-display))

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)


;;; .emacs ends here
