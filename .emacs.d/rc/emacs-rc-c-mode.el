;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: emacs-rc-c-mode.el, 星期一, 五月 10 2010

(if (eq system-type 'windows-nt)
    (setq inc_list (list
                    "d:/gnu/lib/gcc/mingw32/3.4.5/include"
                    "d:/gnu/include/c++/4.1.1"
                    "d:/gnu/include/sys"
                    "d:/gnu/include"
                    "d:/gnu/include/bits"
                    "d:/gnu/include/glib-2.0"
                    "d:/gnu/include/gtk-2.0"
                    "d:/gnu/include/gnu"
                    "d:/gnu/include/gtk-2.0/gtk"
                    "d:/gnu/include/gtk-2.0/gdk-pixbuf"
                    "d:/gnu/include/gtk-2.0/gtk"
                    ))
  (setq inc_list (list "/usr/include"
                       "/usr/local/include"
                       "/usr/include/bits"
                       "/usr/include/glib-2.0"
                       "/usr/include/gtk-2.0"
                       "/usr/include/gnu"
                       "/usr/include/gtk-2.0/gtk"
                       "/usr/include/gtk-2.0/gdk-pixbuf"
                       "/usr/include/gtk-2.0/gtk"
                       )))

(mapc
 (lambda (mode)
   (define-abbrev-table mode '(
                               ("inc" "" skeleton-include 1)
                               )))
 '(c-mode-abbrev-table c++-mode-abbrev-table))

;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.
(define-skeleton skeleton-include
  "generate include<>" ""
  > "#include <"
  (completing-read "Include File:"
                   (mapcar #'(lambda (f) (list f ))
                           (apply 'append
                                  (mapcar
                                   #'(lambda (dir)
                                       (directory-files dir))
                                   inc_list))))
  ">")

(defvar yyc-c/c++-hightligh-included-files-key-map nil)
(if yyc-c/c++-hightligh-included-files-key-map
    nil
  (setq yyc-c/c++-hightligh-included-files-key-map (make-sparse-keymap))
  (define-key yyc-c/c++-hightligh-included-files-key-map (kbd "<RET>") 'find-file-at-point))


(require 'ctypes)
(ctypes-auto-parse-mode 1)

(provide 'emacs-rc-c-mode)
;;; emacs-rc-c-mode.el ends here
