;;; emacs-rc-xml.el begins ---

(defun my-nxml-mode-hook ()
  (local-set-key "\C-c/" 'nxml-finish-element)
  (auto-fill-mode)
  (rng-validate-mode)
  (unify-8859-on-decoding-mode)
  (setq ispell-skip-html t)
  (hs-minor-mode 1)
  (base-auto-pair)
  )

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(add-to-list
 'auto-mode-alist
 (cons (concat "\\."
               (regexp-opt
                '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "rdf") t) "\\'")
       'nxml-mode))

(push '("<\\?xml" . nxml-mode) magic-mode-alist)
(setq nxml-attribute-indent 2)
(setq nxml-auto-insert-xml-declaration-flag t)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
               ""
               nil))
(provide 'emacs-rc-xml)
;;; emacs-rc-xml.el ends here
