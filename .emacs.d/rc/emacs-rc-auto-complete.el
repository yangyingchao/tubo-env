;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;; emacs-rc-auto-complete.el ---
(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default) ; 调用默认设置, defined in auto-complete-config.el。
(add-to-list 'ac-dictionary-directories "~/.emacs.d/templates/dic")

;;; Autofill Keybinding.
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (set-face-background 'ac-selection-face "steelblue")
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-underline-p 'ac-candidate-face "darkgray")
  (define-key ac-complete-mode-map (kbd "<C-tab>") 'ac-expand)
  (define-key ac-complete-mode-map "\M-\r" 'ac-complete)
  (define-key ac-complete-mode-map [(tab)] 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (setq ac-auto-start 3)
  (setq ac-dwim t)
  (setq ac-override-local-map nil)  ;don't override local map
  (setq ac-modes '(
                   ada-mode
                   asm-mode c++-mode c-mode cc-mode cperl-mode css-mode
                   ecmascript-mode emacs-lisp-mode emms-tag-editor-mode f90-mode
                   fortran-mode haskell-mode java-mode javascript-mode
                   latex-mode lisp-interaction-mode lisp-mode
                   literate-haskell-mode makefile-mode org-mode perl-mode
                   php-mode python-mode ruby-mode scheme-mode sgml-mode sh-mode
                   text-mode xml-mode  eshell-mode
                   ))
  )


;; The sources for common all mode.
(setq-default ac-sources
      '(
        ac-source-yasnippet
        ac-source-words-in-buffer
        ac-source-filename
        ))

;;; Lisp mode
(defun yyc/ac-source-lisp ()
  "Sources for lisp mode"
  (setq ac-sources (append '(ac-source-symbols) ac-sources))
  )

(defun yyc/ac-source-python ()
  "sources for python mode"
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  )

(add-hook 'emacs-lisp-mode-hook 'yyc/ac-source-lisp)
(add-hook 'python-mode-hook 'yyc/ac-source-python)
(global-auto-complete-mode t) ;enable global-mode

(provide 'emacs-rc-auto-complete)
;;; emacs-rc-auto-complete.el ends here