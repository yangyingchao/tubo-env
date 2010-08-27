;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-platform.el, 星期二, 六月  8 2010

(defvar shellpath nil
  "Path of which shell will be used by emacs.")
(defvar platform-rfc-dir nil "rfc dir for rfc-view")
(setup-font)
(setq shellpath "/bin/bash")
(setq shell-file-name shellpath)
(setq-default explicit-shell-file-name shellpath)
(setenv "SHELL" shell-file-name)

(provide 'emacs-rc-platform)
;;;;; emacs-rc-platform.el ends here
