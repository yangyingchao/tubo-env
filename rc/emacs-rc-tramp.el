;;; emacs-rc-tramp.el begins ---
(require 'tramp)
(setq tramp-default-user "yyc")

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

(nconc  (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods))) '("/bin/sh" "-i"))
(setcdr       (assq 'tramp-remote-sh  (assoc "ssh" tramp-methods))  '("/bin/sh -i"))
(setq tramp-completion-without-shell-p t)
(setq tramp-shell-prompt-pattern "^[ $]+")
(setq tramp-auto-save-directory "~/.emacs.d/auto-save-list")

(tramp-set-completion-function "ssh"
			       '((tramp-parse-sconfig "/etc/ssh_config")
				 (tramp-parse-sconfig "~/.ssh/config")))

(provide 'emacs-rc-tramp)
;;; emacs-rc-tramp.el ends here