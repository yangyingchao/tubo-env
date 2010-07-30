;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-platform.el, 星期二, 六月  8 2010

(defvar shellpath nil
  "Path of which shell will be used by emacs.")
(defvar cedet-sys-include-dirs nil "system include dir for cedet" )

(defvar platform-rfc-dir nil "rfc dir for rfc-view")

(defvar cedet-c-dependency-system-include-path nil )

(defun setup-font ()
  (if (string-equal system-name "ITC-208024")
      (set-frame-font
       "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
    (set-frame-font
     "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1"))
  )

(setup-font)
(setq shellpath "/bin/bash")
(setq cedet-c-dependency-system-include-path "/usr/include/")
(setq cedet-sys-include-dirs (list
                              "/usr/include"
                              "/usr/include/bits"
                              "/usr/include/glib-2.0"
                              "/usr/include/gnu"
                              "/usr/include/gtk-2.0"
                              "/usr/include/gtk-2.0/gdk-pixbuf"
                              "/usr/include/gtk-2.0/gtk"
                              "/usr/local/include"
                              "/usr/local/include"))
(setq  platform-rfc-dir "~/Documents/rfcs/")
(setq shell-file-name shellpath)
(setq-default explicit-shell-file-name shellpath)
(setenv "SHELL" shell-file-name)

(provide 'emacs-rc-platform)
;;;;; emacs-rc-platform.el ends here
