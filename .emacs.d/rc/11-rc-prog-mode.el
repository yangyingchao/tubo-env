;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-prog-mode.el, 08-27-2010


;;;; CEDET Settings

(require 'cedet)
(require 'eassist)
(require 'semantic-c)
(require 'semantic-decorate-include)
(require 'semantic-gcc)
(require 'semantic-ia)
(require 'semanticdb-global)
(require 'semantic-lex-spp)


;; Enable code helpers.
(semantic-load-enable-code-helpers)

(global-semantic-decoration-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-show-parser-state-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(which-func-mode 1)
(global-srecode-minor-mode 1)

(setq semantic-idle-work-parse-neighboring-files-flag t)
(setq semantic-idle-scheduler-idle-time 2)
(global-ede-mode t)
(ede-enable-generic-projects)

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

(defvar mru-tag-stack '()
  "Tag stack, when jumping to new tag, current tag will be stored here,
and when jumping back, it will be removed.")


(defun yc/store-mru-tag (pt)
  "Store tag info into mru-tag-stack"
  (interactive "d")
  (let* ((tag (semantic-mrub-find-nearby-tag pt)))
    (if tag
        (let ((sbm (semantic-bookmark (semantic-tag-name tag)
                                      :tag tag)))
          (semantic-mrub-update sbm pt 'mark)
          (add-to-list 'mru-tag-stack sbm)
          )
      (error "No tag to go!")))
  )

(defun yc/goto-func (pt)
  "Store current postion and call (semantic-ia-fast-jump)"
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-ia-fast-jump pt)
)

(defun yc/goto-func-any (pt)
  "Store current postion and call (semantic-ia-fast-jump)"
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-complete-jump)
  )

(defun yc/symref (pt)
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-symref))

(defun yc/symref-symbol (sym)
  (interactive (list (car (senator-jump-interactive "Symrefs for: "
                                                    nil nil nil))))
  (yc/store-mru-tag (point))
  (semantic-symref-symbol sym))

(defun yc/return-func()
  "Return to previous tag."
  (interactive)
  (if (car mru-tag-stack)
      (semantic-mrub-switch-tags (pop mru-tag-stack))
    (error "TagStack is empty!")))

;; C mode

;;;; Include settings

(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public" "."
        "../.." "../../include" "../../inc" "../../common" "../../public"))

(defun yc/add-common-includes ( )
  "Add common includes"
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        cedet-user-include-dirs))

(setq semantic-c-dependency-system-include-path "/usr/include/")


(defun DE-imply-includes-in-directory (dir)
  "Add all header files in DIR to `semanticdb-implied-include-tags'."
  (let ((files (directory-files dir t "^.+\\.h[hp]*$" t)))
    (defvar-mode-local c++-mode semanticdb-implied-include-tags
      (mapcar (lambda (header)
                (semantic-tag-new-include
                 header
                 nil
                 :filename header))
              files))))

(defun yc/add-semantic-symbol (symbol)
  "Add symbol to semantic-lex-c-preprocessor-symbol-map"
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map symbol)
  )

(defun yc/add-wx-support ()
  "Add wxwidget related files."
  (let* ((wx-base-dir "/usr/include/wx-2.8")
         (wx-symbol-list (list '("WXDLLEXPORT" . "")
                               '("WXDLLIMPEXP_CORE" . "")
                               '("WXDLLIMPEXP_ADV" . "")
                               '("WXDLLIMPEXP_BASE" . "")
                               '("WXDLLIMPEXP_FWD_CORE" . "")
                               '("WXDLLIMPEXP_FWD_XML" . "")
                               '("WXDLLIMPEXP_FWD_BASE" . "")
                               )))
    ;; include files for wxwidgets
    (semantic-add-system-include wx-base-dir 'c++-mode)

    ;; preprocessor macro
    (mapc 'yc/add-semantic-symbol
          wx-symbol-list)
    (DE-imply-includes-in-directory (concat wx-base-dir "/wx/gtk"))
    ))


(defun yc/add-qt-support ( )
  "Add qt related files."
  (let* ((qt4-base-dir "/usr/include/qt4")
         (qt4-gui-dir (concat qt4-base-dir "/QtGui"))
         )
    (semantic-add-system-include qt4-base-dir 'c++-mode)
    (semantic-add-system-include qt4-gui-dir 'c++-mode)

    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt4-base-dir "/Qt/qconfig.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt4-base-dir "/Qt/qconfig-dist.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt4-base-dir "/Qt/qglobal.h"))

    (add-to-list 'semantic-lex-c-preprocessor-symbol-map
                 '("Q_GUI_EXPORT" . ""))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-map
                 '("Q_CORE_EXPORT" . ""))
    ))

(defun yc/add-gtk-support ( )
  "Add gtk related files."
  (let ((gtk-related-includes (list
                               "/usr/include/bits"
                               "/usr/include/glib-2.0"
                               "/usr/include/gtk-2.0"
                               )))
    (mapc (lambda(dir)
            (semantic-add-system-include dir 'c-mode))
          gtk-related-includes)))

(yc/add-common-includes)

;;;; XXX: It does not work, don't know why!
;; (yc/add-wx-support)
;; (yc/add-qt-support)
;; (yc/add-gtk-support)

;;;; TAGS Menu
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))

(add-hook 'semantic-init-hooks 'my-semantic-hook)

;;;; Enable Ede
;; (require 'ede)
(global-ede-mode 1)

;;;; Custom template for srecode
(require 'srecode)
;;(setq srecode-minor-menu t)
(srecode-minor-mode 1)

;; (setq srecode-map-load-path nil)

;; (add-to-list 'srecode-map-load-path
;;              (expand-file-name "~/.emacs.d/templates/srecode")
;;              )

(push (expand-file-name "~/.emacs.d/templates/srecode") srecode-map-load-path)
;;;; Customized functions to generate code quickly.

(defun yc/insert-single-comment ()
  "Insert signle line of comment using srecode"
  (interactive)
  (insert " /* */")
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
      ecb-windows-width 0.18
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


 ;; *************************** TAGS Database Settings *********************

;;;; xgtags settings.

(require 'xgtags)

(defvar cur-buffer nil
  "Current buffer, used to restore buffer after tags has been updated.")

(defun on-process-terminate (process event)
  (message "Finished updating TAGs ...")
   (pop-to-buffer "*Messages*")
   (goto-char (point-max))
   (pop-to-buffer cur-buffer))

(defun yc/update-tag ()
  (interactive)
  (let ((up-process nil))
    (if xgtags-mode
        (progn
          (setq cur-buffer (buffer-name))
          (message "start to global -uv")
          (setq up-process
                (start-process "yc/update-tag" "*Messages*" "global" "-uv"))
          (set-process-sentinel up-process 'on-process-terminate)))))


  ;;;; "Setup key-binding for xgtags"
(defun yc/xgtags-hook-func ()
  "My keybingdings for xgtag mode and cscope mode"
  ;;;; Commonly keybingdings, etags style.
  (global-set-key (kbd "<C-M-.>") 'xgtags-find-with-grep)
  (global-set-key (kbd "<C-M-;>") 'xgtags-find-rtag)
  (local-set-key "\C-css" 'xgtags-find-symbol)
  (local-set-key "\C-csd" 'xgtags-find-tag)
  (local-set-key "\C-csc" 'xgtags-find-rtag)
  (local-set-key "\C-csf" 'xgtags-find-file)
  (local-set-key "\C-csi" 'xgtags-find-with-idutils)
  (local-set-key "\C-csn" 'xgtags-select-next-tag)
  (local-set-key "\C-csp" 'xgtags-select-prev-tag)
  (local-set-key "\C-csU" 'yc/update-tag)
)

(add-hook 'xgtags-mode-hook 'yc/xgtags-hook-func)


 ;; *************************** Python Settings ****************************

;;;; Python Settings
(require 'auto-complete)

(require 'python)
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
            (autoload 'python-mode "python-mode" "Python Mode." t)
            (autoload 'jython-mode "python-mode" "Python editing mode." t)
            (autoload 'py-shell "python-mode" "Start  interpreter in another window." t)
            (autoload 'pymacs-apply "pymacs")
            (autoload 'pymacs-call "pymacs")
            (autoload 'pymacs-eval "pymacs" nil t)
            (autoload 'pymacs-exec "pymacs" nil t)
            (autoload 'pymacs-load "pymacs" nil t)
            (pymacs-load "ropemacs" "rope-")
            (setq ropemacs-enable-autoimport t)
            (rope-open-project "~/.emacs.d/database/python/")
            (local-set-key [(f1)] 'python-describe-symbol)
            ))

 ;;;; c-mode specific.

;; Add kernel style
(c-add-style "kernel-coding"
             '( "linux"
                (c-basic-offset . 8)
                (indent-tabs-mode . t)
                (tab-width . 8)
                (c-comment-only-line-offset . 0)
                (c-hanging-braces-alist
                 (brace-list-open)
                 (brace-entry-open)
                 (substatement-open after)
                 (block-close . c-snug-do-while)
                 (arglist-cont-nonempty))
                (c-cleanup-list brace-else-brace)
                (c-offsets-alist
                 (statement-block-intro . +)
                 (knr-argdecl-intro . 0)
                 (substatement-open . 0)
                 (substatement-label . 0)
                 (label . 0)
                 (statement-cont . +))
                ))
(c-add-style "my-coding-style"
             '( "k&r"
                (c-basic-offset . 4)
                (indent-tabs-mode . nil)
                (tab-width . 4)
                (c-comment-only-line-offset . 0)
                (c-hanging-braces-alist
                 (brace-list-open)
                 (brace-entry-open)
                 (substatement-open after)
                 (block-close . c-snug-do-while)
                 (arglist-cont-nonempty))
                (c-cleanup-list brace-else-brace)
                (c-offsets-alist
                 (statement-block-intro . +)
                 (knr-argdecl-intro . 0)
                 (substatement-open . 0)
                 (substatement-label . 0)
                 (label . 0)
                 (statement-cont . +))
                ))

(defvar kernel-keywords '("linux-" "kernel" "driver" "samba" "ssf")
  "Keywords which are used to indicate this file should use kernel-style.")

(add-hook 'c-mode-common-hook
          (lambda ()
            (let* ((filename (buffer-file-name))
                   (is-kernel-code nil))
              (if filename
                  (dolist (keyword kernel-keywords)
                    (if (string-match keyword filename)
                        (setq is-kernel-code t))))
              (if is-kernel-code
                  (progn
                    (c-set-style "kernel-coding"))
                (c-set-style "my-coding-style")))))

(setq auto-mode-alist
      (append
       '(("\\.C$"    . c++-mode)
         ("\\.H$"    . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.hh$"   . c++-mode)
         ("\\.c$"    . c-mode)
         ("\\.h$"    . c++-mode)
         ("\\.m$"    . objc-mode)
         ) auto-mode-alist))

(mapc
 (lambda (mode)
   (define-abbrev-table mode '(
                               ("inc" "" skeleton-include 1)
                               )))
 '(c-mode-abbrev-table c++-mode-abbrev-table))



;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.

(defun yc/filter-include-dirs ()
  "remove non-existed dirs from input-list"
  (setq inc-list nil)
  (mapc (lambda (inc-dir)
          (if (file-exists-p inc-dir)
              (add-to-list 'inc-list inc-dir)
            nil
            ))
        semantic-dependency-system-include-path)
  (princ inc-list)
  )

(define-skeleton skeleton-include
  "generate include<>" ""
  > "#include <"
  (completing-read "Include File:"
                   (mapcar (lambda (f) (list f ))
                           (apply 'append
                                  (mapcar
                                   (lambda (dir)
                                     (directory-files
                                      dir nil
                                      "\\(\\.h\\)?"))
                                   (yc/filter-include-dirs)
))))
  ">")

(require 'ctypes)
(ctypes-auto-parse-mode 1)

;;;; C++自动代码生成
(require 'member-function)
(setq  mf--source-file-extension "cpp")

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key
             "\C-cm"
             'expand-member-functions)))

;;;; C-mode-hooks .

;;;; Function to change settings for tab.
(defun yc/toggle-tab-mode ()
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




(when (try-require  'auto-header)
  (setq header-max-search (* fill-column 2))
  (if (string-match "ITC-208024" system-name)
      (progn
        (setq header-max-search 1)
        (setq header-copyright-notice
              (format "INVENTEC corporation (c)%s all rights reserved."
                      (format-time-string "%Y" (current-time)))
              )
        (setq header-email-address "Yang.Ying-chao@inventectj.com")
        ( setq header-field-list
               '(copyright filename description update up_head up_sep
                           icreated yc/sep issues))
        )
    (progn
      (setq header-copyright-notice
            (format "Copyright (C) %s, Yang, Ying-chao"
                    (format-time-string "%Y" (current-time))))
      (setq header-email-address "yangyingchao@gmail.com")
      (setq header-field-list
             '(copyright filename description author gpl))
      )
    )
  )



;;;; Common Program settings

(defun yc/basic-prog-keybinding ()
  "Some Basic keybinds fro programming"
  (interactive)
  (local-set-key  [(return)] 'newline-and-indent)
  (local-set-key (kbd "M-|") 'align)
  (local-set-key  [(tab)] 'indent-or-complete)
  )

(defun setup-program-keybindings()
  ;;;; Common program-keybindings
  (interactive)
  (xgtags-mode 1) ;; keybindings for xgtags.

  ;;;; "keybindings for sematic"
  (semantic-default-c-setup)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "M-n") 'senator-next-tag)
  (local-set-key (kbd "M-p") 'senator-previous-tag)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c}" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cJ" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cP" 'semantic-ia-show-summary)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-c\C-j" 'yc/goto-func-any)
  (local-set-key "\C-cj" 'yc/goto-func)
  (local-set-key "\C-cR" 'yc/symref)
  (local-set-key "\C-cr" 'yc/symref-symbol)
  (local-set-key [S-f12] 'yc/return-func)
  (local-set-key [S-f8] 'yc/return-func)
  (local-set-key [M-S-f12] 'yc/return-func)
  (local-set-key (kbd "C-x SPC") 'yc/store-mru-tag)
  (local-set-key "\C-cp" 'semantic-ia-show-doc)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)

  ;;;; Keybindings for srecode
  (local-set-key "\C-cdf" 'srecode-document-insert-function-comment)
  (local-set-key "\C-cdh" 'header-make)
  (local-set-key "\C-cds" 'yc/insert-single-comment)

  (local-set-key "\C-cl" 'yc/list-attentions)
  ;;;; Others
  (local-set-key "\C-c\C-h" 'sourcepair-load)
  (local-set-key "\C-x\C-h" 'sourcepair-load)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key (kbd "<M-return>") 'semantic-complete-inline-done)
  )

(require 'cflow-mode)

(defun yc/cflow-function (function-name)
  "Get call graph of inputed function. "
  (interactive (list (car (senator-jump-interactive "Function name: "
                                                    nil nil nil))))
  (let* ((cmd (format "cflow  -b --main=%s %s" function-name buffer-file-name))
        (cflow-buf-name (format "**cflow-%s:%s**"
                                (file-name-nondirectory buffer-file-name)
                                function-name))
        (cflow-buf (get-buffer-create cflow-buf-name)))
    (set-buffer cflow-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (shell-command-to-string cmd))
    (pop-to-buffer cflow-buf)
    (goto-char (point-min))
    (cflow-mode)))



(defvar yc/common-warn-words
  (rx bow
      (or "todo" "fix" "fixme" "yyc" "@bug" "XXX"
          "TODO" "FIX" "FIXME" "YYC" "@BUG" "TODOLIST")
      (zero-or-one ":") eow)
  "Commonly used warnning words")

(defvar yc/common-keyword-words
  (rx bow (or "PDEBUG") (zero-or-one ":") eow)

  "Commonly used keyword")


(defun yc/show-prog-keywords ()
  ;; highlight additional keywords
  (yc/add-keyword yc/common-warn-words 'font-lock-warning-face)
  (yc/add-keyword yc/common-keyword-words 'font-lock-builtin-face)
  (font-lock-add-keywords
   nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))

  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1
                                 font-lock-warning-face t)))
  )

(defun yc/add-senator-expand-to-hippie ()
  "Add senator-try-expand-semantic to hippie-try-functions-list.
This can be done automatically, But I'd like to put this
senator-try-expand-semantic after yas/hippie-try-expand."
  (make-local-variable 'hippie-expand-try-functions-list)
  (make-local-variable 'senator-try-function-already-enabled)
      ;; Does nothing if semantic completion is already enabled (via
      ;; customization for example).
      (setq senator-try-function-already-enabled
            (memq 'senator-try-expand-semantic
                  hippie-expand-try-functions-list))

      (if senator-try-function-already-enabled
          nil
        (progn ;; Remove yas/expand to avoid to load it multiple times.
          (setq hippie-expand-try-functions-list
                (delq 'yas/hippie-try-expand
                      hippie-expand-try-functions-list))
          (setq hippie-expand-try-functions-list
                (cons 'senator-try-expand-semantic
                      hippie-expand-try-functions-list))
          (setq hippie-expand-try-functions-list
                (cons 'semantic-ia-complete-symbol
                      hippie-expand-try-functions-list))
          (setq hippie-expand-try-functions-list
                (cons 'yas/hippie-try-expand
                      hippie-expand-try-functions-list))
          )))


(defun my-program-hook ()
  ;; Enable hide-ifdef-mode
  (yc/show-prog-keywords)
  (yc/basic-prog-keybinding)
  (yc/add-senator-expand-to-hippie)
  (setup-program-keybindings)
  (program-mode-auto-pair)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key "\C-ct" 'yc/cflow-function)
  (flyspell-prog-mode)
  )


(defvar yc/lisp-keywords
  (rx bow (or "add-to-list" "try-require" "add-hook" "autoload") eow)
  "My Lisp keywords")

(defun my-lisp-hook ()
  ;; Enable hide-ifdef-mode
  (yc/show-prog-keywords)
  (yc/basic-prog-keybinding)
  (setup-program-keybindings)
  (program-mode-auto-pair)
  (local-set-key  [(tab)] 'indent-or-complete)
  (yc/add-keyword yc/lisp-keywords 'font-lock-keyword-face)
  (make-local-variable 'header-field-list)
  (setq header-field-list
        '(lisp_desc blank copyright blank author blank n_emacs gpl
                    blank e_comment blank))
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

;; ***************** flymake *****************
(require 'flymake)
(autoload 'flymake-find-file-hook "flymake" "" t)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level 0)
(setq flymake-master-file-dirs
      '("." "./src" "../src" "../../src"
        "./source" "../source" "../../source"
        "./Source" "../Source" "../../Source"
        "./test" "../test" "../../test"
        "./Test" "../Test" "../../Test"
        "./UnitTest" "../UnitTest" "../../UnitTest"))


(defun yc/flymake-disable ()
  "Disable flymake mode. It's really borering when reading kernel code."
  (interactive)
  (setq flymake-allowed-file-name-masks '())
  (flymake-mode nil))


(defun yc/flymake-enable ()
  "Enable flymake mode"
  (interactive)
  (setq flymake-allowed-file-name-masks
        '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-gcc-init)
          ;; ("\\.\\(?:h\\(?:pp\\)?\\)\\'" flymake-master-make-gcc-header-init flymake-master-cleanup)
          ("\\.py\\'" flymake-pyflakes-init)
          ("\\.xml\\'" flymake-xml-init)
          ("\\.html?\\'" flymake-xml-init)
          ("\\.cs\\'" flymake-simple-make-init)
          ("\\.p[ml]\\'" flymake-perl-init)
          ("\\.php[345]?\\'" flymake-php-init)
          ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
          ("\\.tex\\'" flymake-simple-tex-init)
          ("\\.idl\\'" flymake-simple-make-init)
          ))
  (flymake-mode t))

(setq flymake-allowed-file-name-masks '()) ;; Disable flymake by default.



(add-to-list 'auto-mode-alist '("\\.ebuild$" . shell-script-mode))


 ;;;;;;;; Configurations  about GDB;

(defun gdb-mode-hook-func  ()
  (local-set-key (kbd "<C-f5>") 'gud-go)
  (local-set-key (kbd "<C-f6>") 'gud-step)
  (local-set-key (kbd "<C-f7>") 'gud-next)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-buffer-on-exit)
  '(auto-fill-mode nil)
  (gud-tooltip-mode t)
  (gdb-many-windows t))

(add-hook 'gdb-mode-hook 'gdb-mode-hook-func)



;;;;;;;; Configurations of PowerShell-mode ;;;;;;;;
(require 'powershell-mode)

(defun yc/pws-find-tag (function)
  "Find defination of function under current poin"
  (interactive
   (let* ((fn (thing-at-point 'symbol))
         (val nil)
         (cmd nil))
     (message fn)
     (setq val (completing-read (if fn
                                    (format "Search for (default %s): " fn)
                                  "Search for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (if (null function)
      (message "You didn't specify a function")
    (progn
      (setq cmd (concat "egrep -i \"^function +" function "\" . -rI"))
      (eshell-command cmd)
      (pop-to-buffer (get-buffer "*grep*"))
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (kill-line 3)
      (insert (concat "*********************** Find Tag for:"
                      function "********************\n\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      )))

(defun yc/pws-get-help (function)
  "Display the documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (thing-at-point 'symbol))
                  val)
     (message fn)
     (setq val (completing-read (if fn
                                    (format "Help for (default %s): " fn)
                                  "Help for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (if (null function)
      (message "You didn't specify a function")
    (progn
      (start-process "Powershell-help" nil "devhelp" "-s" function))))



(add-hook 'powershell-mode-hook
          (lambda()
            (progn
              (yc/show-prog-keywords)
              (program-mode-auto-pair)
              (yc/basic-prog-keybinding)
              (local-set-key [(f1)] 'yc/pws-get-help)
              (local-set-key [(meta .)] 'yc/pws-find-tag)
              )))


 ;;;;;;;; Perl mode;;;;;;;;;;;;;;;
(add-hook 'perl-mode-hook 'my-program-hook)


;;;;;;;;; Emacs-lisp mode ;;;;;;;;;;;;;;

(if (try-require 'sawfish)
    (progn
      (autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
      (setq auto-mode-alist
            (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
            auto-mode-alist
            (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
            auto-mode-alist
            (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist)))
  (message "Failed to load sawfish.")
  )

 ;;;;;;;;;;;;;;;; For wxwidgets ;;;;;;;;;;;;;;;;;;;;;




(provide '11-rc-prog-mode)

;;;;; emacs-rc-prog-mode.el ends here
