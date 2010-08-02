;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;; cedet site-lisp configuration
(require 'cedet)
(require 'semantic/senator)

;;;; gcc setup
(require 'eldoc)
(require 'srecode)
(require 'semantic/analyze/refs)




;; (global-semantic-mru-bookmark-mode 1)
(global-srecode-minor-mode 1)
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
(setq srecode-map-load-path
  (list (srecode-map-base-template-dir)
	(expand-file-name "~/.emacs.d/templates/srecode")
	))

(provide 'emacs-rc-cedet)
;;; emacs-rc-cedet.el.el ends here