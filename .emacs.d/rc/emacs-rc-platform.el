;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-platform.el, 星期二, 六月  8 2010

(defvar shellpath nil
  "Path of which shell will be used by emacs.")
(defvar cedet-sys-include-dirs nil "system include dir for cedet" )

(defvar platform-rfc-dir nil "rfc dir for rfc-view")

(defvar cedet-c-dependency-system-include-path nil )


(defun setup-windows ()
  "Setup variables for windows."
  (set-frame-font
   " -outline-Monaco-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")
  (setq shellpath "d:/gnu/bin/bash.exe")
  (setq cedet-c-dependency-system-include-path "d:/gnu/include/")
  (setq cedet-sys-include-dirs (list
                                "d:/gnu/lib/gcc/mingw32/3.4.5/include"
                                "d:/gnu/include/c++/4.1.1"
                                "d:/gnu/include"
                                "d:/gnu/include/gtk-2.0"
                                "d:/gnu/include/gnu"
                                "d:/gnu/include/gtk-2.0/gtk"
                                "d:/gnu/include/gtk-2.0/gdk-pixbuf"
                                "d:/gnu/include/gtk-2.0/gtk"
                                "c:/Program Files/Microsoft Visual Studio 9.0/VC/include"))
  (setq  platform-rfc-dir "e:/Techbooks/rfcs/"))


(defun setup-linux ()
  "Setup variables for linux"
  (set-frame-font
  "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1")
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
  (setq  platform-rfc-dir "~/Documents/rfcs/"))

(if (eq system-type 'windows-nt)
    (setup-windows)
  (setup-linux)
  )

(setq shell-file-name shellpath)
(setq-default explicit-shell-file-name shellpath)
(setenv "SHELL" shell-file-name)

(provide 'emacs-rc-platform)
;;;;; emacs-rc-platform.el ends here
