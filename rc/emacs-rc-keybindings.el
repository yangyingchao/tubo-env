;;; emacs-rc-keybindings.el begins ---
;; (global-set-key [(control f1)] 'open-mylist)
;; (global-set-key [(f1)] (lambda()        ;;�趨F1Ϊwoman��ݼ�
;;           (interactive)
;;           (let ((woman-topic-at-point t))
;;             (woman))))
(global-set-key [f2] 'zl-newline-up)
(require 'artist)
(global-set-key (kbd "<C-S-f2>") 'artist-mode)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-remove-all)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)
;; (global-set-key [(control meta f3)] 'highlight-symbol-query-replace)
(global-set-key [f4] 'goto-line)
(global-set-key (kbd "<C-f4>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<S-f4>") ' kmacro-end-or-call-macro)
(global-set-key (kbd "<C-S-f4>") 'kmacro-end-and-call-macro)
(global-set-key [f5] 'eshell)
(setq compile-command "make -f makefile")
;(global-set-key [f6] 'make)
;(global-set-key [f7] 'sr-speedbar-toggle);f7��/�ر�speedbar

(global-set-key (kbd "<C-f9>") 'bookmark-jump)
(global-set-key [f9] 'bookmark-set)
; f10 show menu.
(global-set-key (kbd "<C-f10>") 'bookmark-bmenu-list)
(autoload 'smerge-mode "smerge-mode" nil t)
(global-set-key [f12] 'smerge-ediff)

;;;; ���õļ����ȼ�
(global-set-key (kbd "C-,") 'backward-page); �ļ�ͷ
(global-set-key (kbd "C-.") 'forward-page); �ļ�β
(global-set-key (kbd "C-w") 'kill-region); For Windows OS.
(global-set-key [(meta ?/)] 'hippie-expand)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\M-b" 'ibuffer-update)
(global-set-key "\C-\M-^" 'my-adjust-window)
(global-set-key "\C-xT" 'tabify)
(global-set-key "\C-xt" 'untabify)
(global-set-key "\C-x\M-l" 'fill-region)
(require 'rect-mark)
(global-set-key (kbd "<C-S-SPC>") 'rm-set-mark)

(require 'ediff)
(global-set-key "\C-xv=" 'my-ediff-revision)
(global-set-key "\C-xv+" 'ediff-revision)

(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(require 'home-end)
(global-set-key [end]  'home-end-end)
(global-set-key [home] 'home-end-home)

(provide 'emacs-rc-keybindings)
;;; emacs-rc-keybindings.el ends here