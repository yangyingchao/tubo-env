;;; 054-prog-debug.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

 ;;;;;;;; configurations  about gdb;
(use-package gdb-mi
  :commands (gdb)
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gdb-non-stop-setting nil)
  (gdb-show-threads-by-default t)
  (gdb-switch-when-another-stopped nil)
  (gdb-speedbar-auto-raise t)
  :config
  (yc/set-company-backends 'gud-mode 'company-gdb 'company-dabbrev-code)
  ;; (yc/set-keys (list
  ;;                 (cons "\C-c\C-c" 'comint-interrupt-subjob))
  ;;              gud-minor-mode-map)

  (defadvice! yc/gdb-setup-windows-adv (&rest args)
    "Setup gdb wondows.
ORIG-FUNC is called with ARGS."
    :override #'gdb-setup-windows
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let ((win0 (selected-window))
          (win1 (split-window nil nil 'left)))
      (select-window win1)
      (set-window-buffer
       win1
       (if gud-last-last-frame
           (gud-find-file (car gud-last-last-frame))
         (if gdb-main-file
             (gud-find-file gdb-main-file)
           ;; Put buffer list in window if we
           ;; can't find a source file.
           (list-buffers-noselect))))
      (setq gdb-source-window (selected-window))
      ;; (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))) ;; local/register
      ;;   (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3))
      (select-window win0))))

 ;; realgud
(use-package realgud
  :hook ((realgud-short-key-mode . setup-prog-keybindings))
  :custom
  (realgud-safe-mode nil)
  (realgud-file-find-function (lambda (&rest args) nil))
  (realgud-window-split-orientation 'horizontal)
  :config
  (defadvice! yc/realgud:gdb-track-mode-hook-adv (&rest args)
    "Setup realgud-track-mode, to make sure comint-key-map works."
    :override #'realgud:gdb-track-mode-hook
    (realgud-track-mode-setup 't))

  (defadvice! yc/realgud:cmd-eval-dwim-adv (&rest args)
    "ORIG-FUNC is called with ARGS."
    :override #'realgud:cmd-eval-dwim
    (interactive)
    (cond
     ((region-active-p)
      (call-interactively #'realgud:cmd-eval-region)
      (deactivate-mark))
     ((and (not current-prefix-arg)
           (thing-at-point 'symbol))
      (realgud:cmd-run-command
       (thing-at-point 'symbol)
       "eval"))

     (t
      (call-interactively #'realgud:cmd-eval)
      (if (region-active-p)
          (deactivate-mark)))))

  (defadvice! yc/realgud:cmd-run-command-adv (orig-func &rest args)
    "Restore to originally focused window after ORIG-FUNC is called with ARGS."
    :around  #'realgud:cmd-run-command
    (let ((window (current-word))
          (buf (current-buffer)))
      (apply orig-func args)
      (pop-to-buffer-same-window buf)))

  (yc/set-company-backends 'realgud:gdb-track-mode 'company-gdb
    'company-dabbrev-code))

(use-package realgud-lldb
  :commands (realgud--lldb))

(use-package realgud-gdb
  :commands (realgud:gdb realgud:gdb-pid)
  :config
  (require 'realgud)
  (setf (gethash "eval"     realgud:gdb-command-hash) "pprint %s"))

(defalias 'realgud:lldb 'realgud--lldb)
(defalias 'lldb 'realgud--lldb)


;; Function to debug process, either attaching to a running one, or start a new one.
(use-package debug-utils
  :commands (;; attach-pg-idle attach-pg-wal attach-pg-main attach-pg-proc
             yc/uniq-stack
             yc/debug-proc  yc/debug-core
             yc/attach-proc yc/attach-proc-su
             yc/kill-gdb-buffers company-gdb
             yc/parse-backtrace yc/parse_segfault
             ))


(provide '054-prog-debug)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 054-prog-debug.el ends here
