;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-prog-mode.el, 08-27-2010


;;;; CEDET Settings

(require 'cedet)

;; Enable code helpers.
(semantic-load-enable-code-helpers)

(global-semantic-decoration-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-show-parser-state-mode 1)
(global-semanticdb-minor-mode 1)
(setq semantic-idle-scheduler-idle-time 3)
(which-func-mode 1)

(setq global-semantic-idle-completions-mode 1)
(setq semantic-idle-work-parse-neighboring-files-flag t)

;; smart complitions
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;;;; Semanticdb 定制
;; Semantic DataBase存储位置
(setq semanticdb-default-save-directory
      (expand-file-name "~/.emacs.d/database/semanticdb"))
;; 使用 gnu global 的TAGS。
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;;;; Speedbar-frame-mode
(autoload 'speedbar-frame-mode "speedbar" "popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "jump to speedbar frame" t)
(define-key-after (lookup-key global-map [menu-bar tools])
  [speedbar]
  '("speedbar" .
    speedbar-frame-mode)
  [calendar])

;;;; Include settings
(require 'semantic-gcc)
(require 'semantic-c)

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

;;;; Enable Ede
;; (require 'ede)
(global-ede-mode 1)
(require 'my-project)

;;;; Custom template for srecode
(require 'srecode)
;;(setq srecode-minor-menu t)
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
  (insert " /* */")
  ;; (srecode-insert "declaration:comment-single")
  )

 ;;; ECB Setttings

(require 'ecb)
(require 'ecb-autoloads)

(setq ecb-auto-activate nil
      ecb-tip-of-the-day nil
      inhibit-startup-message t
      ecb-auto-compatibility-check nil
      ecb-version-check nil
      ecb-layout-window-sizes (quote (("left8" (nil . 0.2413793103448276)
                                       (nil . 0.27586206896551724)
                                       (nil . 0.1724137931034483))))
      ecb-options-version "2.40"
      ecb-primary-secondary-mouse-buttons (quote mouse-2--C-mouse-2)
      ecb-tip-of-the-day nil
      ecb-windows-width 0.2
      )

(defun update-method
  (ecb-select-source)
  (if (ecb--semantic-active-p)
      (ecb-update-methods-buffer--internal nil nil t)
    (ecb-rebuild-methods-buffer-for-non-semantic)))

(defun toggle-ecb ()
  "Toggles ECB"
  (interactive)
  (if (and (boundp 'ecb-activated-window-configuration) ecb-activated-window-configuration)
      (ecb-deactivate) (ecb-activate)))

(global-set-key [f8] 'toggle-ecb)




;;;; Cmake

(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(add-hook 'cmake-mode-hook 'alexott/common-hook)
(add-hook 'cmake-mode-hook 'alexott/show-prog-keywords)

 ;; *************************** TAGS Database Settings *********************

;;;; xgtags settings.

(require 'xgtags)

;;;; GNU global does not play with python, as a result, cscope is involved.
(require 'xcscope)

(defun yp-xgtags-append ()
  (interactive)
  (if xgtags-mode
      (progn
        (message "start to global -uv")
        (start-process "yp-xgtags-append" "*scratch*" "global" "-uv"))))

  ;;;; "Setup key-binding for xgtags"
(defun yyc/xgtags-hook-func ()
  "My keybingdings for xgtag mode and cscope mode"
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


 ;; *************************** Python Settings ****************************

;;;; Python Settings
(require 'auto-complete)
;(require 'pysmell)
(require 'python)

(autoload 'python-mode "python-mode" "Python Mode." t)
(autoload 'jython-mode "python-mode" "Python editing mode." t)
(autoload 'py-shell "python-mode" "Start  interpreter in another window." t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(setq ropemacs-enable-autoimport t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet))
                 )

            (rope-open-project "~/.emacs.d/database/python/")
            (local-set-key "\C-css" 'cscope-find-this-symbol)
            (local-set-key "\C-csd" 'cscope-find-global-definition)
            (local-set-key "\C-csg" 'cscope-find-global-definition)
            (local-set-key "\C-csG" 'cscope-find-global-definition-no-prompting)
            (local-set-key "\C-csc" 'cscope-find-functions-calling-this-function)
            (local-set-key "\C-csC" 'cscope-find-called-functions)
            (local-set-key "\C-cst" 'cscope-find-this-text-string)
            (local-set-key "\C-cse" 'cscope-find-egrep-pattern)
            (local-set-key "\C-csf" 'cscope-find-this-file)
            (local-set-key "\C-csi" 'cscope-find-files-including-file)
            (local-set-key "\C-csb" 'cscope-display-buffer)
            (local-set-key "\C-csB" 'cscope-display-buffer-toggle)
            (local-set-key "\C-csn" 'cscope-next-symbol)
            (local-set-key "\C-csN" 'cscope-next-file)
            (local-set-key "\C-csp" 'cscope-prev-symbol)
            (local-set-key "\C-csP" 'cscope-prev-file)
            (local-set-key "\C-csu" 'cscope-pop-mark)
            (local-set-key "\C-csa" 'cscope-set-initial-directory)
            (local-set-key "\C-csA" 'cscope-unset-initial-directory)
            (local-set-key "\C-csL" 'cscope-create-list-of-files-to-index)
            (local-set-key "\C-csI" 'cscope-index-files)
            (local-set-key "\C-csE" 'cscope-edit-list-of-files-to-index)
            (local-set-key "\C-csW" 'cscope-tell-user-about-directory)
            (local-set-key "\C-csS" 'cscope-tell-user-about-directory)
            (local-set-key "\C-csT" 'cscope-tell-user-about-directory)
            (local-set-key "\C-csD" 'cscope-dired-directory)
            ))

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

(defvar function-declaration nil nil)
(defvar function-definition nil "nil")
(defvar return-type nil "nil")
(defvar one-function-definition nil "nil")
(defvar one-function-declaration nil "nil")
(defvar class-name nil "nil")
(defvar pos nil "nil")

(defun iter (pos)
  (if (string-match
       "\\(?:\\(?:virtual\\|inline\\|static\\)[ \t\n]*\\)?\\(?:\\(\\(?:const[ \t\n]*\\)?[^ \t\n;* \t\n]*\\)\\([^;]+\\)\\);"
       function-declaration
       pos)
      (progn
        (setq return-type
              (match-string 1 function-declaration))
        (message (format "Return type: %s" return-type))
        (setq one-function-definition
              (match-string 2 function-declaration))
        (message (format "One: %s" one-function-definition))
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

(defun make-cpp-function-definition (buffer class-name start end)
  "generate c++ function definition and insert it into `buffer'"
  (interactive "BAppend to buffer: \nMClass name: \nr")
  (setq function-declaration (buffer-substring-no-properties start end))
  (setq function-definition nil)

  (save-excursion
    (iter 0)
    (set-buffer (get-buffer-create buffer))
    (setq pos (point))
    (insert function-definition)
    (indent-region pos (point)))
  (if (one-window-p)
      (split-window-vertically))
  (switch-to-buffer-other-window buffer))

;;;; C-mode-hooks .
(defun yyc/c-mode-keys ()
  "description"
  (semantic-default-c-setup)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cJ" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cl" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  )

;;;; Function to change settings for tab.
(defun yyc/toggle-tab-mode ()
  "Function to change tabs quickly"
  (interactive)
  (if indent-tabs-mode
      (progn
        (setq-default tab-width 4)
        (setq-default c-basic-offset 4)
        (setq-default indent-tabs-mode nil)
        )
    (progn
      (setq-default tab-width 8)
      (setq-default c-basic-offset 8)
      (setq-default indent-tabs-mode t)
      )
    )
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

(require 'cflow-mode)
(defvar cmd nil nil)
(defvar cflow-buf nil nil)
(defvar cflow-buf-name nil nil)

(defun yyc/cflow-function (function-name)
  "Get call graph of inputed function. "
  (interactive (list (car (senator-jump-interactive "Function name: "
                                                    nil nil nil))))
  (setq cmd (format "cflow  -b --main=%s %s" function-name buffer-file-name))
  (setq cflow-buf-name (format "**cflow-%s:%s**"
                               (file-name-nondirectory buffer-file-name)
                               function-name))
  (setq cflow-buf (get-buffer-create cflow-buf-name))
  (set-buffer cflow-buf)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (shell-command-to-string cmd))
  (pop-to-buffer cflow-buf)
  (goto-char (point-min))
  (cflow-mode)
  )


(defun yyc/show-prog-keywords ()
  ;; highlight additional keywords
  (font-lock-add-keywords nil
                          '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\|YYC\\|yyc\\|HACK\\)\\(:\\|!\\| \\)" 1
                             font-lock-warning-face t)))
    (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1
  font-lock-warning-face t))))

(defun my-program-hook ()
  ;; Enable hide-ifdef-mode
  (yyc/show-prog-keywords)
  (setup-program-keybindings)
  (program-mode-auto-pair)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key  [(tab)] 'indent-or-complete)
  (local-set-key "\C-ct" 'yyc/cflow-function)
  )

(defun my-lisp-hook ()
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
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'shell-script-mode-hook 'my-program-hook)
(add-hook 'sh-mode-hook 'my-program-hook)



(add-to-list 'auto-mode-alist '("\\.ebuild$" . shell-script-mode))

(provide 'emacs-rc-prog-mode)
;;;;; emacs-rc-prog-mode.el ends here
