;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:	logviewer.el
;; Version:
;; Description:   Simple viewer of logs.
;; Author:        Yang, Ying-chao <yangyingchao@gmail.com>
;; Created at:    Tue Sep 13 22:23:45 2011
;; TODO
;;     1. Add function to filt LOGs of special level.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom hooks
(defvar logviewer-mode-hook nil)

;; default mode map, really simple
(defvar logviewer-mode-map
  (let ((logviewer-mode-map (make-keymap)))
    ;;    (define-key logviewer-mode-map "\r" 'logviewer-indent-line)
    ;; (define-key logviewer-mode-map "\t" 'logviewer-indent-line)
    logviewer-mode-map)
  "Keymap for PS major mode")

(defvar logviewer-indent-width 4)

(defvar logviewer-font-lock-keywords
  `(
    ;; Date & time.
    (,(rx line-start
          (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit)
          )
     . font-lock-builtin-face)
    (,(rx symbol-start
          (group (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit))
          (1+ space) (group (1+ (or word blank))) (? "["(* digit) "]")":")
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))
    (,(rx symbol-start
          (group (or "ERROR" "FATAL" "error" "fatal" )) ":"
          (group (+ (*? not-newline))) line-end)
    (1 font-lock-warning-face) (2 font-lock-comment-face))
    (,(rx symbol-start
          (group (or "info" "INFO" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-function-name-face) (2 font-lock-doc-face))
    (,(rx symbol-start
          (group (or "DEBUG" "debug" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-keyword-face) (2 font-lock-string-face))
    )
  )

(defvar logviewer-mode-syntax-table (make-syntax-table)
  "Syntax table for Logviewer mode")
;; (modify-syntax-entry ?( "()" logviewer-mode-syntax-table)
;;                      (modify-syntax-entry ?) ")("
;;                      logviewer-mode-syntax-table)

(defvar logviewer-imenu-expressions
  '((nil "^\\(?:[fF]unction\\|Add-Class\\)\\s-+\\([-a-z0-9A-Z_^:.]+\\)[^-a-z0-9A-Z_^:.]" 1))
  "alist of regexp identifying the start of logviewer definitions"
  )
(defun logviewer-setup-imenu ()
  "Installs logviewer-imenu-expression."
  (require 'imenu t)
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression logviewer-imenu-expressions)
  (imenu-add-menubar-index)
  (require 'which-func t)
  (which-function-mode t)
  )

(defun logviewer-mode ()
  "Major mode for editing Logviewer files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'logviewer-mode)
  (setq mode-name "logviewer")
  (set-syntax-table logviewer-mode-syntax-table)
  (use-local-map logviewer-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(logviewer-font-lock-keywords))
  (run-hooks 'logviewer-mode-hook))

(add-to-list 'auto-mode-alist '("\\.log\\'" .
                                logviewer-mode))
(add-to-list 'auto-mode-alist '("\\messages\\'" .
                                logviewer-mode))
(provide 'logviewer)
