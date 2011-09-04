;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;; 设置lisp加载路径

(defun my-add-subdirs-to-load-path (dir)
  "把DIR的所有子目录都加到`load-path'里面"
  (interactive)
  (let ((default-directory (concat dir "/")))
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(my-add-subdirs-to-load-path "~/.emacs.d/site-lisp/")
;;;; 将一些自己修改过的lisp放到load-path的最顶层，以变能够正确加载到。
(push "~/.emacs.d/site-lisp/modified/" load-path)
(push "~/.emacs.d/rc" load-path)
(push "~/.emacs.d/projects" load-path)


;;;; 基本配置
(auto-image-file-mode t); 自动加载图像
(column-number-mode t); 显示列数
(fset 'yes-or-no-p 'y-or-n-p); 用y/n替代yes/no
(mouse-avoidance-mode 'animate); 光标碰到鼠标所在位置时，鼠标自动移开
(scroll-bar-mode nil); 去掉滚动条
(setq frame-title-format '("" buffer-file-name)); 设置title
(setq inhibit-startup-message t);  关闭启动界面
(setq-default major-mode 'text-mode); 默认模式为文本模式
(setq mouse-yank-at-point t); 支持鼠标中键粘贴
(setq visible-bell t); 视觉响铃
(setq w32-get-true-file-attributes nil)
(setq x-select-enable-clipboard t) ; 支持外部剪贴板
(show-paren-mode t) ; 显示匹配的括号
(tool-bar-mode -1) ; 不显示菜单栏
(transient-mark-mode t) ; 高亮显示选中的部分
(setq-default fill-column 78); 列宽
(setq-default auto-fill-function 'do-auto-fill)
(global-cwarn-mode 1)
(setq-default global-font-lock-mode t) ; 语法高亮
(setq sentence-end-double-space nil)
(setq font-lock-maximum-decoration t)
(setq-default show-trailing-whitespace t)
(setq-default max-specpdl-size 2048)

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
  (set-face-attribute 'default nil
                      :background "#f0f0f0")
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "navy"
                      :bold t)
  (set-face-attribute 'mode-line nil
                      :foreground "black"
                      :background "#dee7f7")
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "gray20"
                      :background "gray90")
  (set-face-attribute 'fringe nil
                      :background "#eeeeec")
  (set-face-attribute 'region nil
                      :background "#fce94f")
  (set-face-attribute 'header-line nil
                      :background "lemonchiffon1")
  (set-face-attribute 'trailing-whitespace nil
                      :background "#f57900")
  (set-face-attribute 'font-lock-constant-face nil
                      :foreground "dark cyan")
  (set-face-attribute 'font-lock-doc-face nil
                      :foreground "#204a87")
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "#ce5c00")
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground "Purple"
                      :bold t)
  (set-face-attribute 'font-lock-type-face nil
                      :foreground "ForestGreen"
                      :bold t)
  (set-face-attribute 'font-lock-variable-name-face  nil
                      :foreground "sienna"
                      :bold t)
  (set-face-attribute 'font-lock-warning-face  nil
                      :foreground "#d14a14"
                      :background "#1248d1"
                      :bold t)
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground "Blue"
                      :bold t)
  (set-face-attribute 'font-lock-builtin-face nil
                      :bold t
                      :foreground "lime green")
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#cc0000")
  (set-face-attribute 'font-lock-preprocessor-face nil
                      :italic nil
                      :foreground "lavender")
  (set-face-attribute 'font-lock-preprocessor-face nil
                      :foreground "green2"
                      :bold nil)
  )

;;;; backup settings 备份设置
(defvar autosave-dir   "~/.emacs.d/backups/")
(make-directory autosave-dir t)
(setq backup-by-copying t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq delete-old-versions t) ; Delete Old verion of backup files.
(setq kept-new-versions 2)
(setq kept-old-versions 2)
(setq make-backup-files t)
(setq version-control t)

(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S 93free")

;;;; User Info 用户信息
(setq user-full-name "Yang, Ying-chao")
(setq user-mail-address "yangyingchao@gmail.com")

;;;; 最大化窗口
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 160) (height . 60)))

 ;;;; Fonts Settings

(defvar shellpath nil
  "Path of which shell will be used by emacs.")
(defvar platform-rfc-dir nil "rfc dir for rfc-view")
(setq shellpath "/bin/bash")
(setq shell-file-name shellpath)
(setq-default explicit-shell-file-name shellpath)
(setenv "SHELL" shell-file-name)


;;;; Load plugins
(require 'site-gentoo) ;; Packages provided by gentoo.
(require '01-rc-functions)
(require '02-rc-keybindings)
(require '03-rc-misc)
(require '04-rc-complete)
(require '11-rc-prog-mode)
(require '98-emacs-rc-modes)
(require '99-proj)
(setup-font)

;;;; Start Emacs as server.
(server-start)

 ;;;; Following lines were generated by emacs itself.

