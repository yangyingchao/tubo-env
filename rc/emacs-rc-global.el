;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-global.el, 星期一, 五月 31 2010
(require 'xgtags)

(defun yp-xgtags-append ()
  (interactive)
  (if xgtags-mode
      (progn
        (message "start to global -uv")
        (start-process "yp-xgtags-append" "*scratch*" "global" "-u"))))

  ;;;; "Setup key-binding for xgtags"
(defun yyc/xgtags-hook-func ()
  "My keybingdings for xgtag mode"
  ;;;; Commonly keybingdings, etags style.
  (define-key xgtags-mode-map "\M-;" 'xgtags-select-next-tag)
  (define-key xgtags-mode-map "\M-." 'xgtags-find-tag)
  (global-set-key (kbd "<C-M-.>") 'xgtags-find-with-grep)
  (global-set-key (kbd "<C-M-;>") 'xgtags-find-rtag)
  ;;;; Cscope style
  (local-set-key "\C-cws" 'xgtags-find-symbol)
  (local-set-key "\C-cwd" 'xgtags-find-tag)
  (local-set-key "\C-cwc" 'xgtags-find-rtag)
  (local-set-key "\C-cwf" 'xgtags-find-file)
  (local-set-key "\C-cwi" 'xgtags-find-with-idutils)
  (local-set-key "\C-cwn" 'xgtags-select-next-tag)
  (local-set-key "\C-cwp" 'xgtags-select-prev-tag)
  (local-set-key "\C-cwu" 'xgtags-pop-stack)
  (local-set-key "\C-cwU" 'yp-xgtags-append)
  )

(add-hook 'xgtags-mode-hook 'yyc/xgtags-hook-func)

(provide 'emacs-rc-global)
;;;;; emacs-rc-global.el ends here
