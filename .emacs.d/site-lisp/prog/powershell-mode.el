;; powershell-mode.el, version 0.5
;;
;; Author: Vivek Sharma (http://www.viveksharma.com/techlog)
;; Provides: Major mode for editing PS (PowerShell) scripts
;; Last Updated: 08/19/08
;;
;; Modified by Yang,Ying-chao(yangyingchao@gmail.com)
;;

;; custom hooks
(defvar powershell-mode-hook nil)

;; default mode map, really simple
(defvar powershell-mode-map
  (let ((powershell-mode-map (make-keymap)))
    ;;    (define-key powershell-mode-map "\r" 'powershell-indent-line)
    (define-key powershell-mode-map "\t" 'powershell-indent-line)
    powershell-mode-map)
  "Keymap for PS major mode")

(defvar powershell-indent-width 4)

(defvar powershell-continued-regexp  ".*\\(|[\\t ]*\\|`\\)$"
  "Regexp matching a continued line (ending either with an
explicit backtick, or with a pipe).")

(defun powershell-continuation-line-p ()
  "Returns t is the current line is a continuation line (i.e. the
previous line is a continued line, ending with a backtick or a pipe"
  (interactive)
  (save-excursion
    (forward-line -1)
    (looking-at powershell-continued-regexp)))

(defun powershell-indent-line-amount ()
  "Returns the column to which the current line ought to be indented."
  (interactive)
  (beginning-of-line)
  (let ((closing-paren (looking-at "[\t ]*[])}]")))
    (if (powershell-continuation-line-p)
        (progn
          (while (powershell-continuation-line-p)
            (forward-line -1))
          (+ (current-indentation) powershell-indent-width))
      (condition-case nil
          (progn
            (backward-up-list)
            (cond ((not (looking-at ".[\t ]*\\(#.*\\)?$"))
                   (forward-char)
                   (skip-chars-forward " \t")
                   (current-column))
                  (closing-paren
                   (current-indentation))
                  (t
                   (+ (current-indentation) powershell-indent-width))))
        (scan-error ;; most likely, we are at the top-level
         0)))))

(defun powershell-indent-line ()
  "Indent the current line of powershell mode, leaving the point
in place if it is inside the meat of the line"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (amount (save-excursion (powershell-indent-line-amount))))
    (if savep
        (save-excursion (indent-line-to amount))
      (indent-line-to amount))))

(defvar pws-font-lock-keywords
  `(
    ;; Basic keywords.
    (,(rx symbol-start
          (or
           "default" "try" "continue" "return" "param" "finally" "catch"
           "elseif" "foreach" "unction" "if" "else" "switch" "throw" "trap"
           "where" "while" "for" "Default" "Try" "Continue" "Return" "Param"
           "Finally" "Catch" "Elseif" "Foreach" "Unction" "If" "Else"
           "Switch" "Throw" "Trap" "Where" "While" "For" "break" "Break"
           "exit" "Exit"
           ) symbol-end)
     . font-lock-keyword-face)
    (,(rx symbol-start
          (or
           "env" "function" "global" "local" "private" "script" "variable")
          symbol-end)
     . font-lock-type-face)
    ;; Variables.
    (,(rx symbol-start
          (or
           "Args" "ConfirmPreference" "ConsoleFileName" "DebugPreference"
           "Error" "ErrorActionPreference" "ErrorView" "ExecutionContext"
           "foreach" "FormatEnumerationLimit" "HOME" "Host" "Input"
           "LASTEXITCODE" "MaximumAliasCount" "MaximumDriveCount"
           "MaximumErrorCount" "MaximumFunctionCount"
           "MaximumHistoryCount" "MaximumVariableCount" "MyInvocation"
           "NestedPromptLevel" "OFS" "OutputEncoding" "PID" "PROFILE"
           "PSHOME" "PWD" "ProgressPreference"
           "ReportErrorShowExceptionClass" "ReportErrorShowInnerException"
           "ReportErrorShowSource" "ReportErrorShowStackTrace" "ShellId"
           "ShouldProcessPreference" "ShouldProcessReturnPreference"
           "StackTrace" "VerbosePreference" "WarningPreference"
           "WhatIfPreference" "false" "input" "lastWord" "line" "null"
           "true" ))
     . font-lock-variable-name-face)
    (, (rx symbol-start "$"  (1+ word) (0+ "_" (1+ word)))
       . font-lock-variable-name-face)
    ;; Digital Numbers.
    (,(rx symbol-start (1+ digit) (0+ "." (1+ digit)))
     . font-lock-constant-face)
    ;; Function declaretions.
    (,(rx symbol-start (group (any "f" "F") "unction")
          (1+ space) (group (1+ (or word ? (any "_" "-")))))
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ;; @blocks.
    (,(rx line-start (* (any " \t"))
          (group "@" (1+ (or word ?_)) (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; built-in comparations
    (,(rx symbol-start "in"
          symbol-end)
     . font-lock-builtin-face)
    (, (rx symbol-start "-"
           (or

            "and" "as" "band" "bnot" "bor" "bxor" "casesensitive"
            "ccontains" "ceq" "cge" "cgt" "cle" "clike" "clt" "cmatch"
            "cne" "cnotcontains" "cnotlike" "cnotmatch" "contains"
            "creplace" "eq" "exact" "f" "file" "ge" "gt" "icontains"
            "ieq" "ige" "igt" "ile" "ilike" "ilt" "imatch" "ine"
            "inotcontains" "inotlike" "inotmatch" "ireplace" "is"
            "isnot" "le" "like" "lt" "match" "ne" "not" "notcontains"
            "notlike" "notmatch" "or" "replace" "wildcard")
           )
       . font-lock-builtin-face)
    ;; Builtin Functions
    (,(rx (1+ (1+ word) "-") (1+ word))
     . font-lock-function-name-face)
    )
  )

(defvar powershell-mode-syntax-table (make-syntax-table)
  "Syntax table for Powershell mode")
(modify-syntax-entry ?# "<" powershell-mode-syntax-table)
(modify-syntax-entry ?' "\"" powershell-mode-syntax-table)
(modify-syntax-entry ?- "w" powershell-mode-syntax-table)
(modify-syntax-entry ?.  "_" powershell-mode-syntax-table)
(modify-syntax-entry ?:  "_" powershell-mode-syntax-table)
(modify-syntax-entry ?\\ "_" powershell-mode-syntax-table)
(modify-syntax-entry ?\n ">" powershell-mode-syntax-table)
(modify-syntax-entry ?_  "w" powershell-mode-syntax-table)
(modify-syntax-entry ?` "\\" powershell-mode-syntax-table)
(modify-syntax-entry ?{ "(}" powershell-mode-syntax-table)
(modify-syntax-entry ?} "){" powershell-mode-syntax-table)
(modify-syntax-entry ?[ "(]" powershell-mode-syntax-table)
                     (modify-syntax-entry ?] ")["
                     powershell-mode-syntax-table)
(modify-syntax-entry ?( "()" powershell-mode-syntax-table)
                     (modify-syntax-entry ?) ")("
                     powershell-mode-syntax-table)

(defvar powershell-imenu-expressions
  '((nil "^\\(?:[fF]unction\\|Add-Class\\)\\s-+\\([-a-z0-9A-Z_^:.]+\\)[^-a-z0-9A-Z_^:.]" 1))
  "alist of regexp identifying the start of powershell definitions"
  )
(defun powershell-setup-imenu ()
  "Installs powershell-imenu-expression."
  (require 'imenu t)
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression powershell-imenu-expressions)
  (imenu-add-menubar-index)
  (require 'which-func t)
  (which-function-mode t)
  )

(defun powershell-mode ()
  "Major mode for editing PowerShell files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'powershell-mode)
  (setq mode-name "PS")
  (set-syntax-table powershell-mode-syntax-table)
  (use-local-map powershell-mode-map)
  (setq indent-line-function 'powershell-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(pws-font-lock-keywords))
  (make-local-variable 'powershell-indent-width)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s*")
  (set-syntax-table powershell-mode-syntax-table)
  (run-hooks 'powershell-mode-hook))

(add-to-list 'auto-mode-alist '("\\.ps1\\'" .
                                powershell-mode))
(provide 'powershell-mode)
