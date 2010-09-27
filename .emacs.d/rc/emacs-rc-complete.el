;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-complete.el, 08-27-2010


 ;; *********************** Company Mode ******************************

(require 'company)

(setq company-idle-delay nil)
(setq company-minimum-prefix-length 3)
(setq company-begin-commands '(self-insert-command))

(add-hook 'c-mode-hook '(lambda () (company-mode)))
(add-hook 'c++-mode-hook '(lambda () (company-mode)))
(add-hook 'python-mode-hook '(lambda () (company-mode)))
(global-set-key (kbd "<S-iso-lefttab>") 'company-complete-common)

 ;; ************** Autoinsert templates *****************
(require 'autoinsert)
(auto-insert-mode)  ;;; Adds hook to find-files-hook
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
        ("Makefile$" . ["insert.makefile" auto-update-defaults])
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
(yas/initialize)
(setq my-yasnippet-dir "~/.emacs.d/templates/yas-snippets")
(yas/load-directory my-yasnippet-dir)
;; hook for automatic reloading of changed snippets
(defun update-yasnippets-on-save ()
  (when (string-match "yas-snippets" buffer-file-name)
    (yas/load-directory my-yasnippet-dir)))
(add-hook 'after-save-hook 'update-yasnippets-on-save)


;;  *********************** Autocomplete ***********************

(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default) ; 调用默认设置, defined in auto-complete-config.el。
(add-to-list 'ac-dictionary-directories "~/.emacs.d/templates/ac-dict")


;; The sources for common all mode.
(setq-default ac-sources
              '(
                ac-source-semantic
                ac-source-yasnippet
                ac-source-words-in-buffer
                ac-source-filename
                ))

(defmacro ac-company-define-source (name backend &rest overrides)
  "Define auto-complete source NAME from company BACKEND.
When OVERRIDES is specified, OVERRIDES is prepend to original source."
  `(defvar ,name
     '(,@overrides
       (candidates . (ac-company-candidates ',backend))
       (prefix . (ac-company-prefix ',backend))
       (document . (lambda (item) (ac-company-document ',backend item))))))

(defun ac-company-prefix (backend)
  (require backend nil t)
  (when (fboundp backend)
    (let ((prefix (funcall backend 'prefix)))
      (when (stringp prefix)
        (- (point) (length prefix))))))

(defun ac-company-candidates (backend)
  (funcall backend 'candidates ac-prefix))

(defun ac-company-meta-as-document (backend item)
  (funcall backend 'meta item))

(defun ac-company-doc-buffer-as-document (backend item)
  (with-current-buffer (funcall backend 'doc-buffer item)
    (buffer-string)))

(defun ac-company-document (backend item)
  (or (ac-company-doc-buffer-as-document backend item)
      (ac-company-meta-as-document backend item)))

;;; Lisp mode
(ac-company-define-source ac-source-company-elisp company-elisp)
(add-hook 'emacs-lisp-mode-hook
       (lambda ()
         (progn
           (add-to-list 'ac-sources 'ac-source-company-elisp)
           (add-to-list 'ac-sources 'ac-source-symbols)
           )))

(ac-company-define-source ac-source-company-c-lang company-clang)
(add-hook 'c-mode-common-hook
          (lambda ()
            (progn
              (add-to-list 'ac-sources 'ac-source-company-c-lang)
              )))

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
                   ecmascript-mode emacs-lisp-mode emms-tag-editor-mode
                   f90-mode fortran-mode haskell-mode java-mode
                   javascript-mode latex-mode lisp-interaction-mode lisp-mode
                   literate-haskell-mode makefile-mode org-mode perl-mode
                   php-mode python-mode ruby-mode scheme-mode sgml-mode
                   sh-mode text-mode xml-mode  eshell-mode
                   )))

(global-auto-complete-mode t) ;enable global-mode


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


(provide 'emacs-rc-complete)
;;;;; emacs-rc-complete.el ends here
