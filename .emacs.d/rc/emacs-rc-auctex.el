;;; emacs-rc-auctex.el begins ---
;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-

(load "auctex.el" nil t t)

(require 'tex)

(setq TeX-auto-save t)
(setq TeX-global-PDF-mode t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)
;(setq-default TeX-master nil)

(defun yyc/insert-tex-paper ()
  "Insert latex template for writing paper"
  (interactive)
  (insert-file "~/.emacs.d/templates/auto-insert/article.tex")
  )

(defun yyc/insert-tex-beamer ()
  "Insert latex template for writing paper"
  (interactive)
  (insert-file "~/.emacs.d/templates/auto-insert/beamer.tex")
  )

(defun tex-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "（") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "“") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "$") 'skeleton-pair-insert-maybe)
  )

(defvar LaTeX-environment-list nil)
(push '(("lstlisting" LaTeX-env-args
  [TeX-arg-key-val LaTeX-listings-key-val-options])) LaTeX-environment-list)


;;; Redefine following keys to make them in accord with  other modes.
;;; 重新定义下列命令，使之与其他模式保持一致。
(define-key TeX-mode-map "\C-c;"    'TeX-comment-region)
(define-key TeX-mode-map "\C-c:"    'TeX-uncomment-region)

(defun yyc/latex-mode-hook ()
  "Hooks for latex mode."
  (interactive)
  (auto-fill-mode nil)
  (yyc/show-prog-keywords)
  (tex-mode-auto-pair)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (flyspell-mode)
  )

(add-hook 'LaTeX-mode-hook 'yyc/latex-mode-hook)

(provide 'emacs-rc-auctex)
;;; emacs-rc-auctex.el ends here
