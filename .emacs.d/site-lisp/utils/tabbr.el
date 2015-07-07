;;; tabbr.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:
(require 'popup)

(defcustom tabbr-db-path (expand-file-name "~/.cache/emacs/tabbr.db")
  "Path of tabbr database."
  :type 'string
  :group 'tabbr)

(defvar tabbr--abbr-ht nil "Hash table of abbreviations.")
(defvar-local tabbr--current nil "")

(defun tabbr--init ()
  "Initialize abbr list."
  (setq tabbr--abbr-ht (make-hash-table :test 'equal))
  (when (file-exists-p tabbr-db-path)
    (dolist (item (with-temp-buffer
                    (insert-file-contents tabbr-db-path)
                    (goto-char (point-min))
                    (read (current-buffer))))
      (puthash (downcase (car item)) (cdr item) tabbr--abbr-ht))))


(defun tabbr--region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word t)))

(defun tabbr--view-result (word)
  "Run dictionary app to look up for WORD."
  (popup-tip (format "%s: %s" word
                     (aif (gethash (downcase word) tabbr--abbr-ht)
                         it "not defined."))))

(defun tabbr--define-abbr (abbr define)
  "Define abbr and save."
  (puthash (downcase abbr) define tabbr--abbr-ht)
  (with-temp-file tabbr-db-path
    (insert "(")
    (maphash (lambda (key value)
               (pp (cons key value) (current-buffer)))
             tabbr--abbr-ht)
    (insert ")\n"))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun tabbr-edit ()
  "Edit or create a abbreviation."
  (interactive)
  (let* ((default (tabbr--region-or-word))
         (abbr (completing-read
                (if default
                    (format "Edit/Create abbr for (default %s): " default)
                  "Edit/Create abbr for: ")
                nil nil 1
                nil
                nil
                default)))
    (when (and abbr (> (length abbr) 0))
      (with-current-buffer (get-buffer-create (format "Tabbr define: %s" abbr))
        (setq-local tabbr--current abbr)
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (tabbr--define-abbr tabbr--current (buffer-string))))

        (local-set-key (kbd "C-c C-k")
                       (lambda ()
                         (interactive)(kill-buffer (current-buffer))))
        (aif (gethash (downcase abbr) tabbr--abbr-ht)
            (insert it))
        (switch-to-buffer (current-buffer))))))

;;;###autoload
(defun tabbr-search ()
  "Description."
  (interactive)
  (unless tabbr--abbr-ht
    (tabbr--init))

  (let* ((default (tabbr--region-or-word))
         (prompt  (if default (format "Abbr (%s): " default)
                    "Abbr: "))
         (word (read-string prompt nil nil default)))
    (tabbr--view-result word)))


(provide 'tabbr)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; tabbr.el ends here
