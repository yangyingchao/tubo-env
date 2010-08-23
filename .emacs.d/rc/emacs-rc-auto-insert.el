;;; emacs-rc-auto-insert.el ---
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
        ("\\.org$" . ["insert.org"])
        ("\\.sh$" . ["insert.sh" auto-update-defaults])
        ("\\.lisp$" . ["insert.lisp" auto-update-defaults])
        ("\\.el$" . ["insert.el" auto-update-defaults])
        ("\\.erl$" . ["insert.err" auto-update-defaults])
        ("\\.py$" . ["insert.py" auto-update-defaults])
        ("\\.tex$" . ["insert.tex" auto-update-defaults])
        ("\\.html$" . ["insert.html" auto-update-defaults])
        ("\\.devhelp2$" . ["insert.devhelp2" auto-update-defaults])
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

(provide 'emacs-rc-auto-insert)
;;; emacs-rc-auto-insert.el ends here