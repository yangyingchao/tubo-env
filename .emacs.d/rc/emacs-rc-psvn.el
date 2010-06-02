;;; emacs-rc-psvn.el begins ---
(require 'psvn)
(global-set-key "\C-xvv" 'svn-status-commit)
(global-set-key "\C-xvl" 'svn-status-show-svn-log)
(global-set-key "\C-xvu" 'svn-status-update-cmd)
(global-set-key "\C-xvs" 'svn-status-curdir)
(global-set-key "\C-xvS" 'svn-status)

(defun svn-status-curdir()
  (interactive)
  (svn-status (file-name-directory (buffer-file-name))))

(provide 'emacs-rc-psvn)
;;; emacs-rc-psvn.el ends here
