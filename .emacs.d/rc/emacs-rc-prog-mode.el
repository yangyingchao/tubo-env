;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-prog-mode.el, 08-27-2010


;;;; CEDET Settings

(require 'cedet)
(require 'semantic/senator)

;;;; gcc setup
(require 'eldoc)
(require 'semantic/analyze/refs)

(setq semantic-imenu-auto-rebuild-directory-indexes nil)
(setq semanticdb-search-system-databases t)
(setq senator-minor-mode-name "SN")

(autoload 'speedbar-frame-mode "speedbar" "popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "jump to speedbar frame" t)

(defadvice push-mark (around semantic-mru-bookmark activate)
  "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
  (semantic-mrub-push semantic-mru-bookmark-ring
                      (point)
                      'mark)
  ad-do-it)

;; smart complitions
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;;;; Semanticdb 定制
;; Semantic DataBase存储位置
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/semanticdb"))
;; 使用 gnu global 的TAGS。
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)


(define-key-after (lookup-key global-map [menu-bar tools])
  [speedbar]
  '("speedbar" .
    speedbar-frame-mode)
  [calendar])

;;;; Include settings
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/c)

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public" "."
        "../.." "../../include" "../../inc" "../../common" "../../public"))

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

(let ((include-dirs cedet-user-include-dirs))
  (setq include-dirs (append include-dirs cedet-sys-include-dirs))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

(setq semantic-c-dependency-system-include-path "/usr/include/")

;;;; TAGS Menu
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hooks 'my-semantic-hook)


;;;;  Helper tools.
(custom-set-variables
 '(semantic-default-submodes (quote (;; global-semantic-highlight-func-mode
                                     global-semantic-decoration-mode global-semantic-idle-completions-mode
                                                                     global-semantic-idle-scheduler-mode global-semanticdb-minor-mode
                                                                     global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
 '(semantic-idle-scheduler-idle-time 3))

(semantic-mode)
(global-semantic-show-parser-state-mode 1)

;;;; Enable Ede
(require 'ede)
(global-ede-mode 1)
(require 'my-project)

;;;; Custom template for srecode
(require 'srecode)
(setq srecode-minor-menu t)
(srecode-minor-mode 1)

(setq srecode-map-load-path
      (list (srecode-map-base-template-dir)
            (expand-file-name "~/.emacs.d/templates/srecode")
            ))

;;;; Customized functions to generate code quickly.
(defun yyc/insert-file-header ()
  "Insert file comment using srecode"
  (interactive)
  (srecode-insert "file:filecomment")
  )

(defun yyc/insert-single-comment ()
  "Insert signle line of comment using srecode"
  (interactive)
  (insert "	/* */")
  ;; (srecode-insert "declaration:comment-single")
  )



;;;; Cmake

(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(add-hook 'cmake-mode-hook 'alexott/common-hook)
(add-hook 'cmake-mode-hook 'alexott/show-prog-keywords)



;;;; xgtags settings.

(require 'xgtags)

(defun yp-xgtags-append ()
  (interactive)
  (if xgtags-mode
      (progn
        (message "start to global -uv")
        (start-process "yp-xgtags-append" "*scratch*" "global" "-uv"))))

  ;;;; "Setup key-binding for xgtags"
(defun yyc/xgtags-hook-func ()
  "My keybingdings for xgtag mode"
  ;;;; Commonly keybingdings, etags style.
  (global-set-key (kbd "<C-M-.>") 'xgtags-find-with-grep)
  (global-set-key (kbd "<C-M-;>") 'xgtags-find-rtag)
  ;;;; Cscope style
  (local-set-key "\C-css" 'xgtags-find-symbol)
  (local-set-key "\C-csd" 'xgtags-find-tag)
  (local-set-key "\C-csc" 'xgtags-find-rtag)
  (local-set-key "\C-csf" 'xgtags-find-file)
  (local-set-key "\C-csi" 'xgtags-find-with-idutils)
  (local-set-key "\C-csn" 'xgtags-select-next-tag)
  (local-set-key "\C-csp" 'xgtags-select-prev-tag)
  (local-set-key "\C-csu" 'xgtags-pop-stack)
  (local-set-key "\C-csU" 'yp-xgtags-append)
  )

(add-hook 'xgtags-mode-hook 'yyc/xgtags-hook-func)



;;;; Python Settings
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'jython-mode "python-mode" "Python editing mode." t)
(autoload 'py-shell "python-mode" "Start  interpreter in another window." t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'interpreter-mode-alist '("jython" . jython-mode))

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)



;;;; c-mode specific.

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
  (semantic-default-c-setup)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cl" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  )

(defun yyc/change-tab-mode ()
  "Function to change tabs quickly"
  (interactive)
  (setq-default tab-width 8)
  (setq-default c-basic-offset 8)
  (setq-default indent-tabs-mode t)
  )

(add-hook 'c-mode-common-hook 'yyc/c-mode-keys)



;;;; Common Program settings

(defun setup-program-keybindings()
  (interactive)

  ;;;; Common program-keybindings
  (local-set-key  [(return)] 'newline-and-indent)

  (xgtags-mode 1) ;; keybindings for xgtags.
  (local-set-key (kbd "M-|") 'align)
  (global-set-key (kbd "<C-tab>") (lambda()   (interactive)
                                    (align (line-beginning-position) (line-end-position))
                                    ))

  ;;;; "keybindings for sematic"
  (semantic-default-c-setup)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cl" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)

  ;;;; Keybindings for srecode
  (local-set-key "\C-cdf" 'srecode-document-insert-function-comment)
  (local-set-key "\C-cdh" 'yyc/insert-file-header)
  (local-set-key "\C-cds" 'yyc/insert-single-comment)
  ;;;; Others
  (local-set-key "\C-c\C-h" 'sourcepair-load)
  (local-set-key "\C-x\C-h" 'sourcepair-load)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )

(defun yyc/show-prog-keywords ()
  (interactive)
  ;; highlight additional keywords
  (font-lock-add-keywords nil
                          '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\|HACK\\):" 1
                             font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(FIX \\|FIXME \\|TODO \\|BUG \\|XXX \\|HACK \\)" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1
  font-lock-warning-face t))))

(defun my-program-hook ()
  ;; Enable hide-ifdef-mode
  (yyc/show-prog-keywords)
  (setup-program-keybindings)
  (program-mode-auto-pair)
  (local-set-key  [(tab)] 'indent-or-complete)
  )

(add-hook 'c-mode-common-hook 'my-program-hook)
(add-hook 'c-mode-hook 'my-program-hook)
(add-hook 'c++-mode-hook 'my-program-hook)
(add-hook 'python-mode-hook 'my-program-hook)
(add-hook 'java-mode-hook 'my-program-hook)
(add-hook 'lisp-mode-hook 'my-program-hook)
(add-hook 'emacs-lisp-mode-hook 'my-program-hook)
(add-hook 'shell-script-mode-hook 'my-program-hook)



(provide 'emacs-rc-prog-mode)
;;;;; emacs-rc-prog-mode.el ends here
