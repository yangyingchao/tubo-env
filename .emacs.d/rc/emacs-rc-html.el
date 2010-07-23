;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-html.el, 星期五, 五月 28 2010
(defun yyc/html-newline ()
  "New line, add <br> into the end of line."
  (interactive)
  (insert "<br>")
  (newline-and-indent)
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
  )

(add-hook 'html-mode-hook 'my-html-mode-hooks)
(provide 'emacs-rc-html)
;;;;; emacs-rc-html.el ends here
