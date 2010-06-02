;;; emacs-rc-wiki.el begins ---
;;; wikipedia-mode
(autoload 'wikipedia-mode "wikipedia-mode"nil t)
(add-to-list 'auto-mode-alist
             '("\\.wiki\\'" . wikipedia-mode))
(add-to-list 'auto-mode-alist
             '("en\\.wikipedia\\.org" . wikipedia-mode))
(add-to-list 'auto-mode-alist
			 '("itsalltext.*\\.txt$" . wikipedia-mode))
(add-hook 'wikipedia-mode-hook 'turn-on-flyspell)
(provide 'emacs-rc-wiki)
;;; emacs-rc-wiki.el ends here
