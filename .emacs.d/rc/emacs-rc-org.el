;;; emacs-rc-org.el begins ---
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

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
