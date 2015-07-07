;;; 01-generics.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Generic settings irritated to specific mode.

;;; Code:
;;

 ;; Package Management...

(require 'package)

(custom-set-variables
 '(package-archives
   '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ;; ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
     ("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
     ))
 '(package-archive-priorities
   '(;; ("melpa-stable" . 10)
     ("gnu" . 5)
     ("melpa" . 10))))

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(package-initialize)

(custom-set-variables
 '(ivy--display-transformers-alist nil)
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-update-melpa-p nil)

 '(use-package-always-ensure nil) ; Auto-download package if not exists
 '(use-package-always-defer t) ; Always defer load package to speed up startup
 '(use-package-verbose nil) ; Report loading details
 '(use-package-expand-minimally t)  ; make the expanded code as minimal as possible
 '(use-package-always-pin nil)
 '(use-package-enable-imenu-support t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'quelpa)
  (package-install 'use-package))

(require 'use-package)

(use-package quelpa-use-package
  :ensure t)

(require 'quelpa-use-package)


;;;; User Info
(put (quote setq-default) 'lisp-indent-function 'defun)

(setq-default
  user-full-name		"Yang,Ying-chao"
  user-mail-address 		"yingchao.yang@icloud.com"

  messages-buffer-max-lines 	t
  mouse-avoidance-mode 		'animate; 光标碰到鼠标所在位置时，鼠标自动移开

  ;; 设置 title
  frame-title-format 		'(:eval
                                  (cond
                                   (buffer-file-name buffer-file-name)
                                   (dired-directory dired-directory)
                                   (t (buffer-name))))

  inhibit-startup-message 	t ;  关闭启动界面
  inhibit-default-init 		t

  initial-scratch-message 	nil
  initial-major-mode 		'text-mode
  mouse-yank-at-point 		t ; 支持鼠标中键粘贴
  visible-bell 			t; 视觉响铃
  x-select-enable-clipboard t
  use-dialog-box 		nil
  major-mode                    'text-mode; 默认模式为文本模式
  indicate-buffer-boundaries    'left
  show-trailing-whitespace      nil
  create-lockfiles              nil

  ;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
  scroll-margin 3
  scroll-conservatively 10000

  ;; 列宽, this got reset by (yc/setup-display) or by 100-private.el
  fill-column 78
  auto-fill-function 'do-auto-fill
  font-lock-maximum-decoration nil
  ;; 语法高亮
  global-font-lock-mode t
  max-specpdl-size 8192
  max-lisp-eval-depth 8192

  custom-theme-directory "~/.emacs.d/themes"
  custom-safe-themes t

 ;;;; Garbage Collection
  garbage-collection-messages nil
  gc-cons-threshold 99999999
  )


(add-function
 :after after-focus-change-function
 (lambda ()
   (unless (frame-focus-state)
     (garbage-collect))))

(fset #'yes-or-no-p #'y-or-n-p); 用y/n替代yes/no

 ;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-80col-face)

(defvar yc/modeline--lsp "" "Nil.")

(setq-default
  mode-line-position
  '("%p ("
   (:propertize "%l," 'face 'mode-line)
   (:eval (propertize "%c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line)))
   ") ")

 mode-line-format
 '("%e"
   mode-line-front-space
   "%Z "
   ;; mode-line-buffer-identification
   (:eval
    (cond (buffer-read-only
           (propertize "%b" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "%b" 'face 'mode-line-modified-face))
          (t (propertize "%b" 'face 'mode-line-buffer-id))))
   "  "

   ;; is remote or local?
   (:eval (if buffer-file-name mode-line-remote ""))
   (:eval (if buffer-file-name " " ""))

   ;; Position, including warning for 80 columns
   "  " mode-line-position

   ;; File size
   "  "  (:eval (if buffer-file-name "%I " ""))

   "  "
   ;; Major mode
   "%m"

   ;; LSP status, if possible
   "  " yc/modeline--lsp

   ;; which function
   "  ["
   (:propertize which-func-current
                local-map ,which-func-keymap
                face which-func)
   "] "

   " "
   (:eval (format-time-string "%H:%M:%S" (current-time)))

   mode-line-end-spaces
   ))


;; 不要闪烁光标
(blink-cursor-mode 1)
(show-paren-mode t)

(defun yc/try-evaluate (&rest args)
  "Try evaluate ARGS without throwing error.
Each ARG should be of form: '(FUNC ARG)"
  (dolist (pair args)
    (condition-case err
        (eval pair)
      ('error "failed to evaluate"))))

(yc/try-evaluate
 '(scroll-bar-mode -1)
 '(tool-bar-mode -1)
 '(menu-bar-mode -1))

(transient-mark-mode t) ; 高亮显示选中的部分


;; Tab设置
(setq-default show-paren-mode t)
(setq indent-line-function 'indent-relative-maybe)

(put 'upcase-region 'disabled nil) ;;  Enable upcase-region
(put 'downcase-region 'disabled nil);; Enable downcase-region
(put 'set-goal-column 'disabled nil)


;;;; 语言设置
;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

(setq buffer-file-coding-system 'utf-8)
(setq system-time-locale "C")

(setq kill-do-not-save-duplicates t)

;; 先格式化再补全
(setq font-lock-maximum-decoration t)
(setq history-delete-duplicates t)

;; 可以递归的使用 minibuffer
(setq enable-recursive-minibuffers t)

(defconst yc/emacs-cache-dir (expand-file-name "~/.cache/emacs/")
  "Caching directory.")

(defconst yc/emacs-tools-dir
  (expand-file-name "~/.emacs.d/tools/")
  "Tools directory.")

(defun yc/make-cache-path (path &optional mkdir)
  "Compose cache directory for PATH.
If `MKDIR' is t, make directory named PATH."
  (let ((path   (expand-file-name path yc/emacs-cache-dir )))
    (when (and mkdir (not (file-exists-p path)))
      (make-directory path t))
    path))

;;;; backup settings 备份设置
(let ((emacs-backup-dir (yc/make-cache-path "backups" t)))
  (setq backup-directory-alist   `((".*" . ,emacs-backup-dir))
        ;; auto-save-file-name-transforms `((".*" ,emacs-backup-dir t))
        auto-save-list-file-prefix  nil
        delete-old-versions t ; Delete Old verion of backup files.
        kept-new-versions 6
        kept-old-versions 2
        make-backup-files t
        version-control t))

(defun yc/backup-enable-predicate (filename)
  "Check if FILENAME should be backup or not.
Files stored in ~/Work/ are controlled by git or svn."
  (and (not (string-match (rx (or "work/" "Work/")) filename))
       (normal-backup-enable-predicate filename)))

(setq backup-enable-predicate 'yc/backup-enable-predicate)

(custom-set-variables
 '(time-stamp-active nil)
 '(time-stamp-warn-inactive t)
 '(time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S 93free")
 '(gc-cons-threshold 99999999))

(setq ring-bell-function 'ignore)

;; Remove vc-hooks, I don't use it.
(setq vc-handled-backends nil)
(setq-default custom-theme-directory "~/.emacs.d/themes"
              custom-safe-themes t)


(put 'narrow-to-region 'disabled nil)

(provide '01-generics)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 01-generics.el ends here
