;;; emacs-rc-woman.el begins ---
(require 'woman)
(set-face-foreground 'woman-italic "#73d2e6")
(set-face-foreground 'woman-bold "#a40000")

(if (eq system-type 'windows-nt)
    (setq woman-manpath (quote ("d:/gnu/home/yyc/mandb")))
  (setq woman-manpath (quote ("/usr/share/man"  "/usr/local/share/man")))
  )

(defun see-woman()
  (interactive)
  (let ((woman-topic-at-point t))
    (woman)))

(global-set-key [(f1)] (lambda()        ;;设定F1为woman快捷键
			 (interactive)
			 (let ((woman-topic-at-point t))
			   (woman))))
(setq woman-use-own-frame nil);; 不打开新的 frame

(provide 'emacs-rc-woman)
;;; emacs-rc-woman.el ends here
