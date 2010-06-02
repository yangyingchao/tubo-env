;;; emacs-rc-makefile-mode.el begins ---
;;;; Makefile mode
(require 'make-mode)
(autoload 'makefile-mode "makefile-mode" nil t)
(add-to-list 'auto-mode-alist
			 '("Makefile.*" . makefile-mode))
(add-to-list 'auto-mode-alist
			 '("makefile.*" . makefile-mode))

(provide 'emacs-rc-makefile-mode)
;;; emacs-rc-makefile-mode.el ends here
