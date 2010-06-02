;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-html.el, 星期五, 五月 28 2010
(defun my-newline ()
  "New line, add <br> into the end of line."
  (interactive)
  (insert "<br>")
  (newline-and-indent)
  )

(defun my-html-mode-hooks ()
  "description"
  (local-set-key (kbd "<C-return>") 'my-newline)
  )

(add-hook 'html-mode-hook 'my-html-mode-hooks)
(provide 'emacs-rc-html)
;;;;; emacs-rc-html.el ends here
