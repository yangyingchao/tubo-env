;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: emacs-rc-speedbar.el, 星期一, 五月 10 2010

(setq speedbar-mode-hook '(lambda ()
                            (interactive)
                            (other-frame 0)))
(require 'sb-texinfo)
(eval-after-load "speedbar" '(load-library "sb-texinfo"))
(add-hook 'texinfo-mode-hook (lambda () (require 'sb-texinfo)))

(require 'sr-speedbar)
(global-set-key [f7] 'sr-speedbar-toggle);f7打开/关闭speedbar

(provide 'emacs-rc-speedbar)
;;; emacs-rc-speedbar.el ends here
