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


(provide 'emacs-rc-misc)
;;;;; emacs-rc-misc.el ends here
