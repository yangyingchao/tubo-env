;;; emacs-rc-rfc.el begins ---

(autoload 'rfcview-mode "rfcview" nil t)

(setq  platform-rfc-dir "~/Documents/TechBooks/RFCs/")
(setq auto-mode-alist
      (cons '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode)
            auto-mode-alist))
(eval-after-load "speedbar" '(load-library "sb-rfcview"))
(custom-set-variables
 '(speedbar-supported-extension-expressions
   (append
    speedbar-supported-extension-expressions
    '("rfc[0-9]+\\.txt"))))
;; Customized face of rfc.
(custom-set-faces
 '(rfcview-title-face ((t (:foreground "darkgreen" :weight bold)))))

;;;; get-rfc
(autoload 'get-rfc-view-rfc "get-rfc" "Get and view an RFC" t nil)
(autoload 'get-rfc-view-rfc-at-point "get-rfc" "View the RFC at point" t nil)
(autoload 'get-rfc-grep-rfc-index "get-rfc" "Grep rfc-index.txt" t nil)

(setq get-rfc-wget-program "wget")
(setq get-rfc-remote-rfc-directory "http://www.rfc-editor.org/rfc/")

(setq  get-rfc-local-rfc-directory platform-rfc-dir)
(custom-set-faces
 '(rfcview-title-face ((t (:foreground "darkgreen" :weight bold)))))

(provide 'emacs-rc-rfc)
;;; emacs-rc-rfc.el ends here
