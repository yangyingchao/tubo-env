;;; emacs-rc-python.el begins ---

(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'jython-mode "python-mode" "Python editing mode." t)
(autoload 'py-shell "python-mode" "Start  interpreter in another window." t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'interpreter-mode-alist '("jython" . jython-mode))

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(provide 'emacs-rc-python)
;;; emacs-rc-pyton.el ends here
