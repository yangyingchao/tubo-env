;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: 04-rc-complete.el, 08-27-2010


 ;; ************** Autoinsert templates *****************
(require 'autoinsert)
(setq auto-insert-mode t)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/.emacs.d/templates/auto-insert/")
(setq auto-insert 'other)
(setq auto-insert-query nil)

;; auto-insert stuff
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-alist
      '(
        ("\\.cpp$" . ["insert.cpp" auto-update-c-source-file])
        ("\\.h$"   . ["header.h" auto-update-header-file])
        ("\\.c$" . ["insert.c" auto-update-c-source-file])
        ("\\.org$" . ["insert.org" auto-update-defaults])
        ("\\.sh$" . ["insert.sh" auto-update-defaults])
        ("\\.lisp$" . ["insert.lisp" auto-update-defaults])
        ("\\.el$" . ["insert.el" auto-update-defaults])
        ("\\.erl$" . ["insert.err" auto-update-defaults])
        ("\\.py$" . ["insert.py" auto-update-defaults])
        ("\\.tex$" . ["insert.tex" auto-update-defaults])
        ("\\.html$" . ["insert.html" auto-update-defaults])
        ("\\.devhelp2$" . ["insert.devhelp2" auto-update-defaults])
        ("\\.ebuild$" . ["insert.ebuild" auto-update-defaults])
        ("Doxyfile$" . ["insert.doxyfile" auto-update-defaults])
        ))

;; function replaces the string '@@@' by the current file
;; name. You could use a similar approach to insert name and date into
;; your file.
(defun auto-update-header-file ()
  (save-excursion
    (while (search-forward "@@@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (upcase (file-name-nondirectory buffer-file-name)))
        (subst-char-in-region (point-min) (point-max) ?. ?_)
        ))
    )
  )

(defun insert-today ()
  "Insert today's date into buffer"
  (interactive)
  (insert (format-time-string "%m-%e-%Y" (current-time))))

(defun auto-update-c-source-file ()
  (save-excursion
    ;; Replace HHHH with file name sans suffix
    (while (search-forward "HHHH" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h") t
                       )
        ))
    )
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "@@@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name))
        ))
    )
  (save-excursion
    ;; replace DDDD with today's date
    (while (search-forward "DDDD" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match "")
        (insert-today)
        ))
    )
  )

(defun auto-replace-file-name ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name) t)
        ))
    )
  )

(defun auto-update-defaults ()
  (auto-replace-file-name)
  (auto-replace-file-name-no-ext)
  (auto-replace-date-time)
  )

(defun auto-replace-file-name-no-ext ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE_NO_EXT<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-sans-extension (file-name-nondirectory buffer-file-name)) t)
        ))
    )
  )

(defun auto-replace-date-time ()
  (save-excursion
    (while (search-forward "(>>DATE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match "" t)
        (insert-today)
        ))))

;; ******************** Yasnippet ****************************

(require 'yasnippet)
(defvar my-yasnippet-dir nil "nil")
;; (setq yas/indent-line 'fixed)
(yas/initialize)
(setq my-yasnippet-dir "~/.emacs.d/templates/yas-snippets")
(yas/load-directory my-yasnippet-dir)

;; hook for automatic reloading of changed snippets
(defun update-yasnippets-on-save ()
  (when (string-match "yas-snippets" buffer-file-name)
    (yas/load-directory my-yasnippet-dir)))
(add-hook 'after-save-hook 'update-yasnippets-on-save)

(global-set-key (kbd "<C-tab>") 'yas/expand)

;; ************************ Company Mode ***********************

(autoload 'company-mode "company" nil t)
(setq company-idle-delay nil)

(setq company-backends
      '(company-elisp company-nxml company-dabbrev company-css
                     company-eclim company-semantic
                     company-gtags company-dabbrev-code
                     company-keywords
                     company-files )
  )

(add-hook 'c-mode-hook '(lambda () (company-mode)))
(add-hook 'c++-mode-hook '(lambda () (company-mode)))
(add-hook 'python-mode-hook '(lambda () (company-mode)))
(global-set-key (kbd "<S-iso-lefttab>") 'company-complete-common)

;;  *********************** Autocomplete ***********************

(require 'auto-complete)
(require 'ac-company)
(require 'auto-complete-config)

(ac-config-default) ;; Defined in ac-complete-config

(setq ac-auto-start 3)
(setq ac-dwim t)
(setq ac-override-local-map nil)  ;don't override local map
(setq ac-ignore-case t)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/templates/ac-dict")

(mapc
 (lambda(mode)
   (add-to-list 'ac-modes mode))
 '(asm-mode emms-tag-editor-mode haskell-mode latex-mode
            lisp-mode literate-haskell-mode org-mode text-mode
            eshell-mode graphviz-dot-mode powershell-mode))

(set-face-background 'ac-selection-face "steelblue")
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline-p 'ac-candidate-face "darkgray")

(setq-default ac-sources
              '(
                ac-source-semantic
                ac-source-yasnippet
                ac-source-words-in-buffer
                ac-source-filename
                ac-source-semantic
                ac-source-dictionary
                ))

;;;; Extend Auto-Complete with company backends.
;; C mode
(require 'semantic-ia)
;; (ac-company-define-source ac-source-company-abbr company-dabbrev)
;; (ac-company-define-source ac-source-company-abbr-code company-dabbrev-code)
;; (ac-company-define-source ac-source-company-keywords company-keywords)

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; (push 'ac-source-company-abbr ac-sources)
            ;; (push 'ac-source-company-abbr-code ac-sources)
            ;; (push 'ac-source-company-keywords ac-sources)

            (add-to-list 'ac-omni-completion-sources
                         (cons "\\." '(ac-source-semantic)))
            (add-to-list 'ac-omni-completion-sources
                         (cons "->" '(ac-source-semantic)))
            ))

;; Lisp mode
(ac-company-define-source ac-source-company-elisp company-elisp)
(add-hook 'emacs-lisp-mode-hook
       (lambda ()
         (progn
           (add-to-list 'ac-sources 'ac-source-company-elisp)
           (add-to-list 'ac-sources 'ac-source-symbols)
           )))



;; Autofill Keybinding.
(define-key ac-complete-mode-map (kbd "<C-tab>") 'ac-expand)
(define-key ac-complete-mode-map "\M-\r" 'ac-complete)
(define-key ac-complete-mode-map [(tab)] 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)


;; ********************** Common Settings **************************

;;;;  缩进或者补齐
;;; hippie-try-expand settings
(setq hippie-expand-try-functions-list
      '(
        yas/hippie-try-expand
        semantic-ia-complete-symbol
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        ))



(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)
    ))

(provide '04-rc-complete)
;;;;; 04-rc-complete.el ends here
