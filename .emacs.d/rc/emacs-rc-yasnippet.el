;;; emacs-rc-yasnippet.el begins ---
(require 'yasnippet)
(yas/initialize)
(setq my-yasnippet-dir "~/.emacs.d/templates/yas-snippets")
(yas/load-directory my-yasnippet-dir)
;; hook for automatic reloading of changed snippets
(defun update-yasnippets-on-save ()
  (when (string-match "yas-snippets" buffer-file-name)
    (yas/load-directory my-yasnippet-dir)))
(add-hook 'after-save-hook 'update-yasnippets-on-save)

(provide 'emacs-rc-yasnippet)
;;; emacs-rc-yasnippet.el ends here