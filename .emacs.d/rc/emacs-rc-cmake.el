;;; emacs-rc-cmake.el begins ---
(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(add-hook 'cmake-mode-hook 'alexott/common-hook)
(add-hook 'cmake-mode-hook 'alexott/show-prog-keywords)

(provide 'emacs-rc-cmake)
;;; emacs-rc-cmake.el ends here
