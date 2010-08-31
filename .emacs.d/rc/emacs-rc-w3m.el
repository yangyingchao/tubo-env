;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: emacs-rc-w3m.el, 星期四, 四月 22 2010

(require 'w3m-load)
(require 'w3m-e21)
(provide 'w3m-e23)
(require 'w3m)

(defvar w3m-arrived-file-coding-system nil "nil")


(setq  w3m-arrived-file-coding-system 'utf-8
       w3m-async-exec t
       w3m-coding-system 'utf-8
       w3m-default-save-directory "~/tmp/down/"
       w3m-delete-duplicated-empty-lines t
       w3m-file-coding-system 'utf-8
       w3m-file-name-coding-system 'utf-8
       w3m-fill-column 99
       w3m-input-coding-system 'utf-8
       w3m-key-binding 'info
       w3m-mailto-url-function 'compose-mail
       w3m-output-coding-system 'utf-8
       w3m-search-default-coding-system 'utf-8
       w3m-terminal-coding-system 'utf-8
       w3m-use-cookies t
       w3m-use-filter t
       w3m-use-form t
       w3m-use-mule-ucs t
       w3m-use-favicon nil
       w3m-default-display-inline-images t
       w3m-use-filter t
       w3m-command-arguments '("-cookie" "-F" )
       w3m-home-page "http://www.google.com"
       )

(setq w3m-command-arguments-alist
      '(;; Don't use the proxy server to visit local web pages.
        ("^http://\\([^/]*\\.\\)*your-company\\.com\\(/\\|$\\)"
         "-no-proxy")
        ;; Use the proxy server to visit any foreign urls.
        (""
         "-o" "http_proxy=http://127.0.0.1:5866/")))

(standard-display-ascii ?\212 "-")
(standard-display-ascii ?\226 "-")
(standard-display-ascii ?\227 [?-])
(standard-display-ascii ?\222 [?'])
(standard-display-ascii ?\225 [?+])

;; send all pages through one filter
(setq w3m-filter-rules `(("\\`.+" w3m-filter-all)))

(defun w3m-filter-all (url)
  (let ((list '(
                ;; add more as you see fit!
                ("&#187;" "&gt;&gt;")
                ("&laquo;" "&lt;")
                ("&raquo;" "&gt;")
                ("&ouml;" "o")
                ("&#8230;" "...")
                ("&#8216;" "'")
                ("&#8217;" "'")
                ("&rsquo;" "'")
                ("&lsquo;" "'")
                ("\u2019" "\'")
                ("\u2018" "\'")
                ("\u201c" "\"")
                ("\u201d" "\"")
                ("&rdquo;" "\"")
                ("&ldquo;" "\"")
                ("&#8220;" "\"")
                ("&#8221;" "\"")
                ("\u2013" "-")
                ("\u2014" "-")
                ("&#8211;" "-")
                ("&#8212;" "-")
                ("&ndash;" "-")
                ("&mdash;" "-")
                )))
    (while list
      (let ((pat (car (car list)))
            (rep (car (cdr (car list)))))
        (goto-char (point-min))
        (while (search-forward pat nil t)
          (replace-match rep))
        (setq list (cdr list))))))

(defun cc-w3m-beautify-tables()
  (goto-char (point-min))
  (while (re-search-forward
          "\200\\|\203\\|\206\\|\211\\|\214\\|\202\\|\204\\|\210\\|\201" nil t)
    (replace-match "+"))
  (goto-char (point-min))
  (while (re-search-forward "\205" nil t)
    (replace-match "|"))
  (goto-char (point-min))
  (while (re-search-forward "\212" nil t)
    (replace-match "-")))

(add-hook 'w3m-fontify-after-hook 'cc-w3m-beautify-tables)

;;Removing trailing whitespaces (better display):
(defun cc-w3m-del-trailing-ws()
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "")))

(add-hook 'w3m-fontify-after-hook 'cc-w3m-del-trailing-ws)

(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*" (or w3m-current-title
                                     w3m-current-url)) t)))

(defun w3m-open-current-page-in-firefox ()
  "Opens the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-firefox w3m-current-url))

(defun w3m-open-link-or-image-in-firefox ()
  "Opens the current link or image in Firefox."
  (interactive)
  (browse-url-firefox (or (w3m-anchor)
                          (w3m-image))))
(define-key w3m-mode-map "\C-co" 'w3m-open-current-page-in-firefox)
(define-key w3m-mode-map  "\C-c\C-o" 'w3m-open-current-page-in-firefox)
(define-key w3m-mode-map (kbd "j") 'next-line)
(define-key w3m-mode-map (kbd "k") 'previous-line)

(provide 'emacs-rc-w3m)
;;; emacs-rc-w3m.el ends here
