;;; emr-others.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'use-package)

(use-package sgml-mode
  :commands (sgml-pretty-print))

(defun emr-xml-format-region (start end)
  "Fill region (START/END)."
  (interactive "rp")
  (sgml-pretty-print start end))

(defun emr-xml-format ()
  "Fill region (START/END)."
  (interactive)
  (let ((it (and (buffer-file-name)
                 (executable-find "xmllint"))))

    (if it
        (let ((formated (shell-command-to-string (format "%s --format %s" it (buffer-file-name)))))
          (save-excursion
            (erase-buffer)
            (insert formated)
            (save-buffer)))
      (emr-xml-format-region (point-min) (point-max)))    )
  )


(emr-declare-command 'emr-xml-format-region
  :title "format region"
  :description ""
  :modes 'nxml-mode
  :predicate (lambda ()
               mark-active
               ))

(emr-declare-command 'emr-xml-format
  :title "format file"
  :description ""
  :modes 'nxml-mode
  :predicate (lambda ()
               (not mark-active)))


(provide 'emr-others)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-others.el ends here
