;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-misc.el, 08-27-2010

(require 'icomplete)
(icomplete-mode t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'make-mode)
(autoload 'makefile-mode "makefile-mode" nil t)
(add-to-list 'auto-mode-alist
             '("Makefile.*" . makefile-mode))
(add-to-list 'auto-mode-alist
             '("makefile.*" . makefile-mode))

(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 99)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)


;;;; Speedbar

(setq speedbar-mode-hook '(lambda ()
                            (interactive)
                            (other-frame 0)))
(require 'sb-texinfo)
(eval-after-load "speedbar" '(load-library "sb-texinfo"))
(add-hook 'texinfo-mode-hook (lambda () (require 'sb-texinfo)))

(require 'sr-speedbar)
(global-set-key [f7] 'sr-speedbar-toggle);f7打开/关闭speedbar

 ;;; Woman
(require 'woman)
(set-face-foreground 'woman-italic "#73d2e6")
(set-face-foreground 'woman-bold "#a40000")

(if (eq system-type 'windows-nt)
    (setq woman-manpath (quote ("d:/gnu/home/yyc/mandb")))
  (setq woman-manpath (quote ("/usr/share/man"  "/usr/local/share/man")))
  )

(defun see-woman()
  (interactive)
  (let ((woman-topic-at-point t))
    (woman)))

(global-set-key [(f1)] (lambda()        ;;设定F1为woman快捷键
                         (interactive)
                         (let ((woman-topic-at-point t))
                           (woman))))
(setq woman-use-own-frame nil);; 不打开新的 frame

 ;;; ispell
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
(global-set-key (kbd "<S-f11>") 'ispell-word)

 ;;Dired
(require 'dired)
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; ************************** highlight utils ****************************
(require 'highlight-utility)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-remove-all)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

;; ************************** SVN Settings *****************************
(require 'psvn)
(global-set-key "\C-xvv" 'svn-status-commit)
(global-set-key "\C-xvl" 'svn-status-show-svn-log)
(global-set-key "\C-xvu" 'svn-status-update-cmd)
(global-set-key "\C-xvs" 'svn-status-curdir)
(global-set-key "\C-xvS" 'svn-status)

(defun svn-status-curdir()
  (interactive)
  (svn-status (file-name-directory (buffer-file-name))))


 ;; ********************** Browse Killing Ring *************************

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



(provide 'emacs-rc-misc)
;;;;; emacs-rc-misc.el ends here
