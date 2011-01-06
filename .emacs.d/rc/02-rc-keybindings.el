;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;; 02-rc-keybindings.el begins ---

(require '01-rc-functions)

;; C-F1: 'open-mylist
;; C-S-f1: org-agenda
;; F1: 为woman快捷键
(global-set-key [f2] 'auto-rename-buffer)
(global-set-key (kbd "<C-f2>") 'rename-buffer)
(require 'artist)
(global-set-key (kbd "<C-S-f2>") 'artist-mode)

(require 'highlight-utility)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-remove-all)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

(global-set-key [f4] 'goto-line)
(global-set-key (kbd "<C-f4>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<S-f4>") ' kmacro-end-or-call-macro)
(global-set-key (kbd "<C-S-f4>") 'kmacro-end-and-call-macro)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)

;; eshell ssh
(defun eshell/ssh (&rest args)
  "Secure shell"
  (let ((cmd (eshell-flatten-and-stringify
              (cons "ssh" args)))
        (display-type (framep (selected-frame))))
    (cond
     ((and
       (eq display-type 't)
       (getenv "STY"))
      (send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
     ((eq display-type 'x)
      (eshell-do-eval
       (eshell-parse-command
        (format "Terminal -e %s &" cmd)))
      nil)
     (t
      (apply 'eshell-exec-visual (cons "ssh" args))))))

(global-set-key [f5] 'eshell)
(global-set-key (kbd "<S-f5>") 'ansi-term)
;;;; Following Keybinds was set in other files.
;; C-F6: do-compile
;; C-S-F6: Open Makefile
;; f7: 打开/关闭speedbar

(global-set-key (kbd "<C-f9>") 'bookmark-jump)
(global-set-key [f9] 'bookmark-set)
; f10 show menu.
(global-set-key (kbd "<C-f10>") 'bookmark-bmenu-list)
;;;; f11 was used when compiling source codes.
;; **** Following was set in 03-rc-misc.el ****
;; (global-set-key (kbd "<M-S-f11>") 'ispell-buffer)
;; (global-set-key (kbd "<C-f11>") 'flyspell-mode)
;; (global-set-key (kbd "<C-S-f11>") 'flyspell-prog-mode)
;; (global-set-key (kbd "<S-f11>") 'ispell-word)

(autoload 'smerge-mode "smerge-mode" nil t)
(global-set-key [f12] 'smerge-ediff)

;;;; 常用的几个热键
(global-set-key (kbd "C-,") 'backward-page); 文件头
(global-set-key (kbd "C-.") 'forward-page); 文件尾
(global-set-key (kbd "C-w") 'kill-region); For Windows OS.
(global-set-key [(meta ?/)] 'hippie-expand)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\M-b" 'ibuffer-update)
(global-set-key "\C-x\M-f" 'fill-paragraph)
(global-set-key "\C-xT" 'tabify)
(global-set-key "\C-xt" 'untabify)
(global-set-key "\C-x\M-l" 'fill-region)
(global-set-key "\C-c\C-w" 'kill-buffer)
(require 'rect-mark)
(global-set-key (kbd "<C-S-SPC>") 'rm-set-mark)

(require 'ediff)
(global-set-key "\C-xv=" 'my-ediff-revision)
(global-set-key "\C-xv+" 'ediff-revision)

(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(require 'home-end)
(global-set-key [end]  'home-end-end)
(global-set-key [home] 'home-end-home)

(global-set-key "\C-c;" 'yyc/comment-dwim-line)
(global-set-key "\C-c:" 'yyc/comment-dwim-line)

;;; ### Doc-view ###
;;; --- 文档阅读器
(require 'doc-view)
(setq doc-view-continuous t)
(lazy-unset-key
 '(".")
 doc-view-mode-map)                     ;卸载按键
(lazy-set-key
 '(
   ("C-M-j" . doc-view-scroll-up-or-next-page+)       ;翻另一个窗口中图书的下一页
   ("C-M-k" . doc-view-scroll-down-or-previous-page+) ;翻另一个窗口中图书的上一页
   ))
(lazy-unset-key
 '("x" "M-<" "M->")
 doc-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . doc-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 doc-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . doc-view-next-page)                      ;下一页
   ("P" . doc-view-previous-page)                  ;上一页
   ("." . doc-view-first-page)                     ;第一页
   ("," . doc-view-last-page)                      ;最后一页
   ("g" . doc-view-goto-page)                      ;跳到第几页
   ("e" . doc-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . doc-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . doc-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . doc-view-previous-line-or-previous-page) ;上一行或上一屏
   ("t" . doc-view-show-tooltip)                   ;当前页提示
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . doc-view-kill-proc-and-buffer)           ;退出并结束进程
   ("C-s" . doc-view-search)                       ;搜索
   ("C-S-n" . doc-view-search-next-match)          ;下一个匹配
   ("C-S-p" . doc-view-search-previous-match)      ;上一个匹配
   ("+" . doc-view-enlarge)                        ;放大页面
   ("-" . doc-view-shrink)                         ;缩小页面
   ("C-c C-c" . doc-view-toggle-display)           ;在文本和图像间切换
   ("C-c C-t" . doc-view-open-text)                ;打开文本
   ("r" . revert-buffer)                           ;刷新
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 doc-view-mode-map
 )

(setq doc-view-cache-directory "~/.emacs.d/docview")

(provide '02-rc-keybindings)
;;; 02-rc-keybindings.el ends here