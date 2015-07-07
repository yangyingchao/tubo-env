;;; t-report.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(defcustom yc/wp-path "~/Documents/Email/WeeklyReports/"
  "Path to store weekly reports."
  :type 'string
  :group 'user)

(defcustom yc/wp-hook
  nil
  "Hooks to run before saving weekly reports."
  :type 'hook
  :group 'user)

(defcustom yc/wp-user-name user-full-name
  "Descriptions."
  :type 'string
  :group 'user)

(defcustom yc/wp-user-mail user-mail-address
  "Descriptions."
  :type 'string
  :group 'user)

(defun yc/run-wp-hook ()
  "Description."
  (interactive)
  (run-hooks 'yc/wp-hook))

;;;###autoload
(defun yc/new-wp ()
  "Create new weekly-report."
  (interactive)
  (unless (file-directory-p yc/wp-path) (make-directory yc/wp-path t))

  (let* ((range (let* ((current (current-time))
                       (weekday (nth 6 (decode-time current)))
                       (start (time-subtract current (days-to-time (1- weekday))))
                       (end   (time-add start (days-to-time 4))))

                  (concat (format-time-string "%Y%m%d" start)
                          "~"
                          (format-time-string "%Y%m%d" end))))

         (fn (expand-file-name (format "%s/周报-%s.org" yc/wp-path range)))
         (template (expand-file-name (format "%s/template.org" yc/wp-path )))
         (user-full-name yc/wp-user-name)
         (user-mail-address yc/wp-user-mail))

    (add-hook 'org-mode-hook
      (lambda ()
        (add-hook 'before-save-hook 'yc/run-wp-hook nil t)))

    (when (and (not (file-exists-p fn))
               (file-exists-p template))

      (with-temp-file fn
        (insert-file-contents template)
        (yc/auto-update-template "RANGE" range)))

    (find-file fn)
    (goto-char (point-max))))


(defun escape-title (title)
  "Replace characters in `TITLE'."
  (let ((maps '((" " . "-")
                ))
        (result (s-trim title)))
    (s-replace-all maps result)))

(defun yc/new-mail ()
  "Create new mail."
  (interactive)

  (let* ((mail-dir (expand-file-name (format "%s/../Mails/" yc/wp-path)))
         (fn (expand-file-name
                (format "%s/%s-%s.org"
                        mail-dir
                        (format-time-string  "%Y-%m-%d" (current-time))
                        (escape-title (completing-read "Title: " '("Unmaed"))))))
         (user-full-name yc/wp-user-name)
         (user-mail-address yc/wp-user-mail)
         (tpl (expand-file-name "~/.emacs.d/templates/yasnippets-private/org-mode/addr")))

         (unless (file-directory-p mail-dir) (make-directory mail-dir t))

    (find-file fn)
    (goto-char (point-max))
    (when (file-exists-p tpl)
      (insert (shell-command-to-string (concat "sed '1,4d' " tpl)))
      (goto-char (point-max)))))

(provide 't-report)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; t-report.el ends here
