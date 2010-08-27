;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-modes.el, 08-27-2010

 ;; *********************** auctex for LaTeX Mode ************************

(load "auctex.el" nil t t)

(require 'tex)

(setq TeX-auto-save t)
(setq TeX-global-PDF-mode t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)
                                        ;(setq-default TeX-master nil)

(defun yyc/insert-tex-paper ()
  "Insert latex template for writing paper"
  (interactive)
  (insert-file "~/.emacs.d/templates/auto-insert/article.tex")
  )

(defun yyc/insert-tex-beamer ()
  "Insert latex template for writing paper"
  (interactive)
  (insert-file "~/.emacs.d/templates/auto-insert/beamer.tex")
  )

(defun tex-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "（") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "“") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "$") 'skeleton-pair-insert-maybe)
  )

(defvar LaTeX-environment-list nil)
(push '(("lstlisting" LaTeX-env-args
         [TeX-arg-key-val LaTeX-listings-key-val-options])) LaTeX-environment-list)


;;; Redefine following keys to make them in accord with  other modes.
;;; 重新定义下列命令，使之与其他模式保持一致。
(define-key TeX-mode-map "\C-c;"    'TeX-comment-region)
(define-key TeX-mode-map "\C-c:"    'TeX-uncomment-region)

(defun yyc/latex-mode-hook ()
  "Hooks for latex mode."
  (interactive)
  (auto-fill-mode nil)
  (yyc/show-prog-keywords)
  (tex-mode-auto-pair)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (flyspell-mode)
  )

(add-hook 'LaTeX-mode-hook 'yyc/latex-mode-hook)

 ;; ****************************** HTML Mode ******************************

(defun yyc/html-newline ()
  "New line, add <br> into the end of line."
  (interactive)
  (insert "<br>")
  (newline-and-indent)
  )

(defun yyc/html-ws ()
  "White Space."
  (interactive)
  (insert "&nbsp; ")
  )

(defun yyc/remove-hrefs ()
  "Funtion to remove hyperlinks quickly"
  (interactive)
  (while (re-search-forward "<a href=.*?\">\\(.*?>\\)</a>" nil t)
    (replace-match (match-string 1) nil nil))
  )

(defun my-html-mode-hooks ()
  "description"
  (local-set-key (kbd "<C-return>") 'yyc/html-newline)
  (local-set-key (kbd "C-x <SPC>") 'yyc/html-ws)
  )

(add-to-list 'auto-mode-alist
             '("blog.*\\.txt" . html-mode))

(add-hook 'html-mode-hook 'my-html-mode-hooks)

 ;; *************************** Org Mode ********************************

(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|/TODO\\)$" . org-mode))
(add-to-list 'file-coding-system-alist (cons "\\.\\(org\\|org_archive\\|/TODO\\)$"  'utf-8))
(global-font-lock-mode 1)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-ce" 'org-show-entry)
(global-set-key "\C-cl" 'org-store-link)
(setq org-log-done t)
(custom-set-variables
 '(org-startup-folded nil)
 '(org-log-done t)
 '(org-hide-leading-stars t)
 '(org-agenda-include-diary t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-include-all-todo t)
 '(org-use-property-inheritance t)
                                        ; '(org-enforce-todo-dependencies t)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-enforce-todo-checkbox-dependencies t)
 '(diary-file "~/Work/Orgs/diary")
 '(mark-diary-entries-in-calendar t)
 '(org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)"
                                 "DOING(G)" "DONE(d)" "CANCELED(c)")))
 '(org-agenda-files (quote (
                            "~/Work/Orgs/activity_inventory.org"
                            ))))


(setq org-todo-keywords
      '((sequence "TODO" "DOING" "HANGUP" "|" "DONE" "CANCEL")))
(setq org-export-author-info nil)
(setq org-export-creator-info nil)
(setq org-export-time-stamp-file nil)

(defun process-underline ()
  "Process Underline, replace them with \_"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "_" nil t) (replace-match "\_" nil t))
  (goto-char (point-min))
  (while (search-forward "\\_" nil t) (replace-match "\_" nil t))
  )

;;; Key bingdings

(global-set-key [(control f1)] 'open-mylist)

(defun insert-instead ()
  "description"
  (interactive)
  (insert "\\_")
  )

(defun yyc/show-pomodoro-keywords ()
  "Pomodoro Keywords, used by pomodoro technique "
  (interactive)
  ;; highlight additional keywords
  (font-lock-add-keywords nil '(("\\<\\(TODO \\)" 1 font-lock-comment-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE \\):" 1 font-lock-builtin-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DOING \\):" 1 font-lock-function-name-face t)))
  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1
                                 font-lock-warning-face t))))

(defun yyc/org-mode-hooks ()
  "Functions will run when entering org-mode"
  (interactive)
  (org-defkey org-mode-map "\C-cl" 'org-store-link)
  (org-defkey org-mode-map "\C-ca" 'org-agenda)
  (org-defkey org-mode-map "\C-cb" 'org-iswitchb)
  (org-defkey org-mode-map [(control ?,)]     'backward-page)
  (base-auto-pair)
  (yyc/show-pomodoro-keywords)
  )

(add-hook 'org-mode-hook 'yyc/org-mode-hooks)

 ;; *************************** Wiki Mode ******************************
(autoload 'wikipedia-mode "wikipedia-mode"nil t)
(add-to-list 'auto-mode-alist
             '("\\.wiki\\'" . wikipedia-mode))
(add-to-list 'auto-mode-alist
             '("en\\.wikipedia\\.org" . wikipedia-mode))
(add-to-list 'auto-mode-alist
			 '("itsalltext.*\\.txt$" . wikipedia-mode))
(add-hook 'wikipedia-mode-hook 'turn-on-flyspell)

 ;; *************************** nxml mode for XML *******************


(defun my-nxml-mode-hook ()
  (local-set-key "\C-c/" 'nxml-finish-element)
  (auto-fill-mode)
  (rng-validate-mode)
  (unify-8859-on-decoding-mode)
  (setq ispell-skip-html t)
  (hs-minor-mode 1)
  (base-auto-pair)
  )

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(add-to-list
 'auto-mode-alist
 (cons (concat "\\."
               (regexp-opt
                '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "rdf") t) "\\'")
       'nxml-mode))

(push '("<\\?xml" . nxml-mode) magic-mode-alist)
(setq nxml-attribute-indent 2)
(setq nxml-auto-insert-xml-declaration-flag t)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
               ""
               nil))


(provide 'emacs-rc-modes)
;;;;; emacs-rc-modes.el ends here
