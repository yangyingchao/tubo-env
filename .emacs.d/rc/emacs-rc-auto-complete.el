;;; emacs-rc-auto-complete.el ---

;; Generic setup.
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(global-auto-complete-mode t)           ;enable global-mode

(setq ac-auto-start t)
(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-modes '(ada-mode asm-mode c++-mode c-mode cc-mode cperl-mode css-mode
                          ecmascript-mode emacs-lisp-mode emms-tag-editor-mode f90-mode
                          fortran-mode haskell-mode java-mode javascript-mode
                          latex-mode lisp-interaction-mode lisp-mode
                          literate-haskell-mode makefile-mode org-mode perl-mode
                          php-mode python-mode ruby-mode scheme-mode sgml-mode sh-mode
                          text-mode xml-mode
                          ))

;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ac-source-yasnippet
     ac-source-semantic
     ac-source-imenu
     ac-source-abbrev
     ac-source-words-in-buffer
     ac-source-files-in-current-dir
     ac-source-filename
     )))

;;; Lisp mode
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode
               ))
  (add-hook hook '(lambda ()
                    (add-to-list 'ac-sources 'ac-source-symbols))))
;;;; start completion when entered 3 characters
(setq ac-auto-start 3)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

;;; Autofill Keybinding.
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (set-face-background 'ac-selection-face "steelblue")
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-underline-p 'ac-candidate-face "darkgray")
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\M-\r" 'ac-complete)
  (define-key ac-complete-mode-map [(tab)] 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (setq ac-auto-start 2)
  (setq ac-dwim t)
  (setq ac-modes
        (append ac-modes
                '(eshell-mode))
        )
  )

(provide 'emacs-rc-auto-complete)
;;; emacs-rc-auto-complete.el ends here