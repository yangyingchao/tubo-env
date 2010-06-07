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
                                 "|" "DONE(d)" "CANCELED(c)")))
 '(org-agenda-files (quote (
                            "~/Work/Orgs/work.org"
                            "~/Work/Orgs/mics.org"))))

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "HANGUP" "|" "DONE" "CANCEL")))

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

(defun yyc/org-mode-hooks ()
  "Functions will run when entering org-mode"
  (interactive)
  (text-mode-auto-pair)
  (org-defkey org-mode-map "\C-cl" 'org-store-link)
  (org-defkey org-mode-map "\C-ca" 'org-agenda)
  (org-defkey org-mode-map "\C-cb" 'org-iswitchb)
  (org-defkey org-mode-map [(control ?,)]     'backward-page)
  (org-defkey org-mode-map (kbd "_") 'insert-instead)
  )

(add-hook 'org-mode-hook 'yyc/org-mode-hooks)

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
