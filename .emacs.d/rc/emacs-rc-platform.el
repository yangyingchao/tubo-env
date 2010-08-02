;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-platform.el, 星期二, 六月  8 2010

(defvar shellpath nil
  "Path of which shell will be used by emacs.")

(defvar platform-rfc-dir nil "rfc dir for rfc-view")

(defun setup-font ()
  (if (string-equal system-name "ITC-208024")
      (set-frame-font
       "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
    (set-frame-font
     "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1"))
  )

(setup-font)
(setq shellpath "/bin/bash")

(setq  platform-rfc-dir "~/Documents/rfcs/")
(setq shell-file-name shellpath)
(setq-default explicit-shell-file-name shellpath)
(setenv "SHELL" shell-file-name)

(provide 'emacs-rc-platform)
;;;;; emacs-rc-platform.el ends here
