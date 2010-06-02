;;; emacs-rc-ecb.el ---
(require 'ecb)
(require 'ecb-autoloads)

(setq ecb-auto-activate nil
      ecb-tip-of-the-day nil
      inhibit-startup-message t
      ecb-auto-compatibility-check nil
      ecb-version-check nil
      ecb-layout-window-sizes (quote (("left8" (nil . 0.2413793103448276)
                                       (nil . 0.27586206896551724)
                                       (nil . 0.1724137931034483))))
      ecb-options-version "2.40"
      ecb-primary-secondary-mouse-buttons (quote mouse-2--C-mouse-2)
      ecb-tip-of-the-day nil
      ecb-windows-width 0.2
      )

(defun update-method
  (ecb-select-source)
  (if (ecb--semantic-active-p)
  	  (ecb-update-methods-buffer--internal nil nil t)
  	(ecb-rebuild-methods-buffer-for-non-semantic)))

(defun toggle-ecb ()
  "Toggles ECB"
  (interactive)
  (if (and (boundp 'ecb-activated-window-configuration) ecb-activated-window-configuration)
      (ecb-deactivate) (ecb-activate)))

(global-set-key [f8] 'toggle-ecb)

(provide 'emacs-rc-ecb)

;;; emacs-rc-ecb.el ends here