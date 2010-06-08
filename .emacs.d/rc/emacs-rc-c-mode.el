;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: emacs-rc-c-mode.el, 星期一, 五月 10 2010

;; (if (eq system-type 'windows-nt)
;;     (setq inc_list (list
;;                     "d:/gnu/lib/gcc/mingw32/3.4.5/include"
;;                     "d:/gnu/include/c++/4.1.1"
;;                     "d:/gnu/include/sys"
;;                     "d:/gnu/include"
;;                     "d:/gnu/include/bits"
;;                     "d:/gnu/include/glib-2.0"
;;                     "d:/gnu/include/gtk-2.0"
;;                     "d:/gnu/include/gnu"
;;                     "d:/gnu/include/gtk-2.0/gtk"
;;                     "d:/gnu/include/gtk-2.0/gdk-pixbuf"
;;                     "d:/gnu/include/gtk-2.0/gtk"
;;                     ))
;;   (setq inc_list (list "/usr/include"
;;                        "/usr/local/include"
;;                        "/usr/include/bits"
;;                        "/usr/include/glib-2.0"
;;                        "/usr/include/gtk-2.0"
;;                        "/usr/include/gnu"
;;                        "/usr/include/gtk-2.0/gtk"
;;                        "/usr/include/gtk-2.0/gdk-pixbuf"
;;                        "/usr/include/gtk-2.0/gtk"
;;                        )))

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
                                   cedet-sys-include-dirs))))
  ">")

(defvar yyc-c/c++-hightligh-included-files-key-map nil)
(if yyc-c/c++-hightligh-included-files-key-map
    nil
  (setq yyc-c/c++-hightligh-included-files-key-map (make-sparse-keymap))
  (define-key yyc-c/c++-hightligh-included-files-key-map (kbd "<RET>") 'find-file-at-point))


(require 'ctypes)
(ctypes-auto-parse-mode 1)

;;;; C++自动代码生成
(defvar one-function-definition nil)
(defvar one-function-declaration nil)
(defvar function-definition nil)
(defvar return-type nil)
(defvar pos nil)

(defun make-cpp-function-definition (buffer class-name start end)
  "generate c++ function definition and insert it into `buffer'"
  (interactive "BAppend to buffer: \nMClass name: \nr")
  (setq function-declaration (buffer-substring-no-properties start end))
  (setq function-definition nil)
  (defun iter (pos)
    (if (string-match
  "\\(?:\\(?:virtual\\|inline\\|static\\)[ \t\n]*\\)?\\(?:\\(\\(?:const[ \t\n]*\\)?[^ \t\n;* \t\n]*\\([^;]+\\)\\)\\);"
  function-declaration
  pos)
 (progn
   (setq return-type
  (match-string 1 function-declaration))
   (setq one-function-definition
  (match-string 2 function-declaration))
   (if (equal class-name "")
       (setq one-function-declaration
 (concat return-type "\n" one-function-definition))
     (setq one-function-declaration
    (concat return-type "\n"
     class-name "::" one-function-definition)))
   (setq function-definition
  (concat function-definition
   one-function-declaration "\n{\n}\n\n"))
   (iter (match-end 0)))
      '()))
  (save-excursion
    (iter 0)
    (set-buffer (get-buffer-create buffer))
    (setq pos (point))
    (insert function-definition)
    (indent-region pos (point)))
  (if (one-window-p)
      (split-window-vertically))
  (switch-to-buffer-other-window buffer))

(provide 'emacs-rc-c-mode)
;;; emacs-rc-c-mode.el ends here
