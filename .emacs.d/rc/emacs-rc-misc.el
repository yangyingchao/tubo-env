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

(provide 'emacs-rc-misc)
;;;;; emacs-rc-misc.el ends here
