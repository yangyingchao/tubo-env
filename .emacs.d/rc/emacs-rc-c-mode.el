;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id: emacs-rc-c-mode.el, 星期一, 五月 10 2010

(setq auto-mode-alist
      (append
       '(("\\.C$"    . c++-mode)
         ("\\.H$"    . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.hh$"   . c++-mode)
         ("\\.h$"   . c++-mode)
         ("\\.c$"    . c-mode)
         ("\\.h$"    . c-mode)
         ("\\.m$"    . objc-mode)
         ("\\.java$" . java-mode)
         ) auto-mode-alist))

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

(require 'ctypes)
(ctypes-auto-parse-mode 1)

;;;; C++自动代码生成
(require 'member-function)
(setq  mf--source-file-extension "cpp")

(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm"
                                                   #'expand-member-functions)))

;;;; C-mode-hooks .
(defun yyc/c-mode-keys ()
  "description"
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  )
(add-hook 'c-mode-common-hook 'yyc/c-mode-keys)



(provide 'emacs-rc-c-mode)
;;; emacs-rc-c-mode.el ends here
