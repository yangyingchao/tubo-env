;;; emacs-rc-recentf.el begins ---
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 99)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(provide 'emacs-rc-recentf)
;;; emacs-rc-recentf.el ends here
