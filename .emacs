;; -*- emacs-lisp -*- -*- coding: utf-8; -*-

;;;; 设置lisp加载路径
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisps/")
          (default-directory my-lisp-dir))
       (setq load-path (cons my-lisp-dir load-path))
       (normal-top-level-add-subdirs-to-load-path)))
;;;; 将一些自己修改过的lisp放到load-path的最顶层，以变能够正确加载到。
(push "~/.emacs.d/lisps/modified/" load-path)
(push "~/.emacs.d/rc" load-path)
(push "~/.emacs.d/projects" load-path)


;;;; 基本配置
(auto-image-file-mode t); 自动加载图像
(column-number-mode t); 显示列数
(fset 'yes-or-no-p 'y-or-n-p); 用y/n替代yes/no
(mouse-avoidance-mode 'animate); 光标碰到鼠标所在位置时，鼠标自动移开
(scroll-bar-mode nil); 去掉滚动条
(setq frame-title-format '("" buffer-file-name )); 设置title
(setq inhibit-startup-message t);  关闭启动界面
(setq-default major-mode 'text-mode); 默认模式为文本模式
(setq mouse-yank-at-point t); 支持鼠标中键粘贴
(setq visible-bell t); 视觉响铃
(setq w32-get-true-file-attributes nil)
(setq x-select-enable-clipboard t) ; 支持外部剪贴板
(show-paren-mode t) ; 显示匹配的括号
(tool-bar-mode nil) ; 不显示菜单栏
(transient-mark-mode t) ; 高亮显示选中的部分
(setq-default fill-column 78); 列宽
(setq-default auto-fill-function 'do-auto-fill)
(global-cwarn-mode 1)
(setq-default global-font-lock-mode t) ; 语法高亮
(setq sentence-end-double-space nil)
(setq font-lock-maximum-decoration t)

;;;; Garbage Collection
(setq-default garbage-collection-messages t)
(setq-default gc-cons-threshold 99999999)

;;;; 时间和日期
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)


;;;; Tab设置
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default show-paren-mode t)
(setq indent-line-function 'indent-relative-maybe)


;;; hippie-try-expand settings
(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
;;        senator-try-expand-sematic
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs))

(put 'upcase-region 'disabled nil) ;;  Enable upcase-region
(put 'downcase-region 'disabled nil);; Enable downcase-region
(put 'set-goal-column 'disabled nil)


;;;; 语言设置
;;(set-language-environment 'Chinese-GBK)
(prefer-coding-system 'utf-8-unix)

(setq font-lock-maximum-decoration t)
(setq warning-suppress-types (quote (nil)))

;;;; Color Settings 颜色配置
(when window-system
  ;;; Common Settings
  (set-face-foreground 'modeline-buffer-id "RoyalBlue4")
  (set-face-background 'modeline "LightCyan1")
  (set-face-background 'modeline-inactive "DarkSlateGray2")
  (set-face-background 'fringe "#eeeeec")
  (set-face-background 'region "#fce94f")
  (set-face-background 'default "#f8f8d8")
  (set-face-background 'header-line "lemonchiffon1")
  (set-face-background 'trailing-whitespace "#f57900")
  ;; Settings for Font faces.
  (set-face-foreground 'font-lock-builtin-face "#4e9a06")
  (set-face-foreground 'font-lock-comment-face  "#cc0000")
  (set-face-foreground 'font-lock-constant-face "#3465a4")
  (set-face-foreground 'font-lock-doc-face "#204a87")
  (set-face-foreground 'font-lock-string-face  "#ce5c00")
  (set-face-attribute 'font-lock-keyword-face nil
                      :bold t)
  (set-face-attribute 'font-lock-type-face nil
                      :bold t)
  (set-face-attribute 'font-lock-variable-name-face  nil
                      :bold t)
  (set-face-attribute 'font-lock-warning-face  nil
                      :foreground "#d14a14"
                      :background "#1248d1"
                      :bold t)
  (set-face-attribute 'font-lock-function-name-face nil
                      :bold t)
  )

;;;; backup settings 备份设置
(defvar autosave-dir   "~/.emacs.d/backups/")
(make-directory autosave-dir t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq delete-old-versions t) ; Delete Old verion of backup files.
(setq kept-new-versions 5)
(setq kept-old-versions 2)
(setq make-backup-files t)
(setq version-control t)


;;;; Simple lisps needed to be enabled

(require 'icomplete)
(icomplete-mode t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'make-mode)
(autoload 'makefile-mode "makefile-mode" nil t)
(add-to-list 'auto-mode-alist
             '("Makefile.*" . makefile-mode))
(add-to-list 'auto-mode-alist
             '("makefile.*" . makefile-mode))

(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 99)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)



;;;; Load plugins
(require 'emacs-rc-platform)
(require 'emacs-rc-global)
(require 'emacs-rc-dired)
(require 'emacs-rc-auto-complete)
(require 'emacs-rc-auto-header)
(require 'emacs-rc-auto-insert)
(require 'emacs-rc-highlight-utility)
(require 'emacs-rc-browse-kill-ring)
(require 'emacs-rc-ispell)
(require 'emacs-rc-psvn)
(require 'emacs-rc-python)
(require 'emacs-rc-speedbar)
(require 'emacs-rc-rfc)
(require 'emacs-rc-tramp)
(require 'emacs-rc-woman)
(require 'emacs-rc-xml)
(require 'emacs-rc-yasnippet)
(require 'emacs-rc-c-mode)
(require 'emacs-rc-cedet)
(require 'emacs-rc-ecb)
(require 'emacs-rc-functions)
(require 'emacs-rc-org)
(require 'emacs-rc-auctex)
(require 'emacs-rc-keybindings)


;;;; Start Emacs as server.
(server-start)
(require 'emacs-rc-chrome)

(recentf-open-files)