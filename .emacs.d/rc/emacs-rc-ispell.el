;;; emacs-rc-ispell.el ---

(require 'ispell)

(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))
(set-default 'ispell-skip-html t)
(setq ispell-dictionary "english")
(setq ispell-local-dictionary "english")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defun turn-on-flyspell ()
    "Force flyspell-mode on using a positive arg.  For use in hooks."
    (interactive)
    (flyspell-mode 1))
(defun turn-off-flyspell ()
    "Force flyspell-mode on using a positive arg.  For use in hooks."
    (interactive)
    (flyspell-mode nil))



(add-hook 'log-edit-mode-hook 'turn-on-flyspell)

(global-set-key [f11] 'ispell-buffer)
(global-set-key (kbd "<C-f11>") 'flyspell-mode)
(global-set-key (kbd "<C-S-f11>") 'flyspell-prog-mode)
(global-set-key (kbd "<C-M-f11>") 'ispell-word)

(provide 'emacs-rc-ispell)

;;; emacs-rc-ispell.el ends here
