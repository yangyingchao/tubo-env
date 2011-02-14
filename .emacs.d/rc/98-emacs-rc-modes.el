;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-modes.el, 08-27-2010

 ;; *********************** auctex for LaTeX Mode ************************

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(require 'tex-site)
(require 'tex-buf)
(require 'tex)


(setq TeX-auto-save t)
(setq TeX-global-PDF-mode t)
(setq TeX-master nil)  ;; 编辑多文档，在子文档中调用主文档
(setq TeX-output-view-style (quote (("^pdf$" "." "evince %o %(outpage)"))))
(setq TeX-parse-self t)
(setq outline-minor-mode-prefix [(control o)])
(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)

(defun yyc/insert-new-item ()
  "description"
  (interactive)
  (move-end-of-line 1)
  (LaTeX-insert-item)
  )
(add-hook 'LaTeX-mode-hook (lambda()
                             (TeX-PDF-mode t)
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "LaTeX")
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation nil)
                             (setq fill-column 78)
                             (LaTeX-math-mode)
                             (turn-on-reftex)
                             (local-set-key (kbd "<C-return>")
                                            'yyc/insert-new-item)
                             ))

;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-plug-into-AUCTeX t)

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
  (insert "<br />")
  (newline-and-indent)
  )

(defun yyc/html-newph ()
  "New line, add <br> into the end of line."
  (interactive)
  (newline-and-indent)
  (insert "<p>\n\n</p>")
  (backward-char 4)
  (indent-or-complete)
  (previous-line)
  (indent-or-complete)
  )

(defvar fmt nil "nil")
(defvar fmt_len nil "nil")

(defun yyc/html-txt-format (fmt)
  "Format a text string (from start to end )into some format definded as fmt."
  (setq fmt_len (length fmt))
  (message "%d" fmt_len)
  (if (= fmt_len 0)
      (error "Unknown format!"))
  (insert (format "<%s></%s>" fmt fmt))
  (backward-char (+ fmt_len 3))
  (yank)
  )

(defun yyc/html-txt-bd (start end)
  "<strong></strong>"
  (interactive "rp")
  (kill-region start end)
  (yyc/html-txt-format "strong")
  )

(defun yyc/html-txt-pre (start end)
  "<pre></pre>"
  (interactive "rp")
  (kill-region start end)
  (yyc/html-txt-format "pre")
  )
(defun yyc/html-txt-tt (start end)
  "<pre></pre>"
  (interactive "rp")
  (kill-region start end)
  (yyc/html-txt-format "tt")
  )

(defun yyc/html-txt-pha (start end)
  "<p></p>"
  (interactive "rp")
  (kill-region start end)
  (yyc/html-txt-format "p")
  )

(defun yyc/html-txt-col (start end)
  "Add colour."
  (interactive "rp")
  (kill-region start end)
  (insert "   <font color=\"#a40000\">

</font>"
          )
  (previous-line)
  (yank)
  )

(defun yyc/small-font (start end)
  "Add colour."
  (interactive "rp")
  (kill-region start end)
  (insert "   <font size=\"1\">

</font>"
          )
  (previous-line)
  (yank)
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

(defvar fn_ext_name nil "Extension name of file.")
(defvar b64_cmd nil "Command to execute")
(defvar b64_content nil "nil")

(defun yyc/insert-b64-img (fn)
  "description"
  (interactive "fImage to insert:\n")
  (setq fn_ext_name (file-name-extension fn))
  (setq b64_cmd (format "base64 %s | tr -d '\n'" fn))
  (setq b64_content (shell-command-to-string b64_cmd))
  (insert (format "<img src= \"data:image/%s;base64, %s\"/>"
                  fn_ext_name b64_content))
  )

(defun my-html-mode-hooks ()
  "html mode hooks."
  (local-set-key (kbd "<C-return>") 'yyc/html-newline)
  (local-set-key (kbd "<C-M-return>") 'yyc/html-newph)
  (local-set-key (kbd "C-c <SPC>") 'yyc/html-ws)
  (local-set-key (kbd "C-c b") 'yyc/html-txt-bd)
  (local-set-key (kbd "C-c p") 'yyc/html-txt-pre)
  (local-set-key (kbd "C-c P") 'yyc/html-txt-pha)
  (local-set-key (kbd "C-c t") 'yyc/html-txt-tt)
  (local-set-key (kbd "C-c c") 'yyc/html-txt-col)
  (local-set-key (kbd "C-c i") 'yyc/insert-b64-img)
  (local-set-key (kbd "C-c s") 'yyc/small-font)
  (auto-complete-mode)
  (setq fill-column 120)
  )

(add-to-list 'auto-mode-alist
             '("/itsalltext/" . html-mode))

(add-hook 'html-mode-hook 'my-html-mode-hooks)

 ;; *************************** Org Mode ********************************

(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|/TODO\\)$" . org-mode))
(add-to-list 'file-coding-system-alist (cons "\\.\\(org\\|org_archive\\|/TODO\\)$"  'utf-8))
(global-font-lock-mode 1)
(global-set-key (kbd "<C-S-f1>") 'org-agenda)
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

;;; Key bingdings

(defun yyc/show-pomodoro-keywords ()
  "Pomodoro Keywords, used by pomodoro technique "
  (interactive)
  ;; highlight additional keywords
  (font-lock-add-keywords nil '(("\\<\\(TODO \\)"
                                 1 font-lock-comment-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE \\):"
                                 1 font-lock-builtin-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DOING \\):"
                                 1 font-lock-function-name-face t)))
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
  (setq fill-column 120)
  )

(add-hook 'org-mode-hook 'yyc/org-mode-hooks)

 ;; *************************** Wiki Mode ******************************
(autoload 'wikipedia-mode "wikipedia-mode"nil t)
(add-to-list 'auto-mode-alist
             '("\\.wiki\\'" . wikipedia-mode))
(add-to-list 'auto-mode-alist
             '("en\\.wikipedia\\.org" . wikipedia-mode))
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

 ;; **************************** Text Mode ***************************

(defun yyc/txt-mode-hook ()
  "My hooks for txt-mode."
  (interactive)
  (define-key text-mode-map "\C-c\C-e" 'yyc/txt-to-png)
  (setq fill-column 120)
  )

(add-hook 'text-mode-hook 'yyc/txt-mode-hook)
(add-hook  'artist-mode-init 'yyc/txt-mode-hook)


 ;; ************************** ChangeLog *****************************

(add-hook 'change-log-mode-hook
          (lambda()
            (local-set-key (kbd "<C-return>")
                           'add-change-log-entry-other-window)
            ))


(add-to-list 'auto-mode-alist '("svn-commit.*" . log-edit-mode))

 ;; ****************************** Over ********************************


(if (string-match "ITC-208024" system-name)
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start))
  nil
  )


(provide '98-emacs-rc-modes)
;;;;; emacs-rc-modes.el ends here
