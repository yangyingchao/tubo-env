;;; emacs-rc-browse-kill-ring.el begins ---
(require 'browse-kill-ring)

(browse-kill-ring-default-keybindings)

(global-set-key (kbd "C-c k") 'browse-kill-ring)

(setq kill-ring-max 100)

;;; browse-kill-ring に関する設定
(when (locate-library "browse-kill-ring")
  ;; elisp の呼び出しと key-bind
  (autoload 'browse-kill-ring "browse-kill-ring" "interactively insert items from kill-ring" t)
  (define-key ctl-x-map "\C-y" 'browse-kill-ring)
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    (interactive "p")
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop)
  ;; 各種動作
  (setq browse-kill-ring-quit-action 'kill-and-delete-window)
  ;; 見た目の調整
  (if (not window-system)
      (setq browse-kill-ring-display-style 'one-line
            browse-kill-ring-resize-window nil)
    (defface separator '((t (:foreground "slate gray" :bold t))) nil)
    (setq browse-kill-ring-separator "\n--separator------------------------------"
          browse-kill-ring-separator-face 'separator
          browse-kill-ring-highlight-current-entry t
          browse-kill-ring-highlight-inserted-item t
          browse-kill-ring-resize-window
          )))


(provide 'emacs-rc-browse-kill-ring)
;;; emacs-rc-browse-kill-ring.el ends here
