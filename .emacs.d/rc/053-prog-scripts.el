;;; 053-prog-scripts.el -- Brief introduction here.
;; Author: Yang,Ying-chao <yangyingchao@icloud.com>
;;; Commentary:
;;; Code:

 ;; *************************** Python Settings ****************************
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (yc/lsp--setup
                          "pyright-langserver"
                          "npm i -g pyright")))
  :ensure t)

(use-package py-autopep8
  :commands (py-autopep8-buffer)
  :config
  (progn
    (setq py-autopep8-options '("--max-line-length=120"))))

(use-package pydoc
  :bind (:map python-mode-map
              (;; ,(kbd "<S-f1>")
               [S-f1]. pydoc-at-point)))

(use-package python
  :preface
  (defun yc/lsp-load-project-configuration-python-mode (root-file)
    "Load python-specific configurations of LSP, for workspace rooted at ROOT-FILE.."
    (PDEBUG "ENTER, ROOT:" root-file)
    (unless (featurep 'lsp-pyright)
      (require 'lsp-pyright)))

  (defvar yc/missing-autoflake-reported nil)

  (defun python-remove-unused-imports()
    "Removes unused imports and unused variables with autoflake."
    (interactive)
    (if (executable-find "autoflake")
        (progn (shell-command
                (format "autoflake --remove-all-unused-imports -i %s"
                        (shell-quote-argument (buffer-file-name))))
               (revert-buffer t t t))
      (unless yc/missing-autoflake-reported
        (setq yc/missing-autoflake-reported t)
        (warn "python-mode: Cannot find autoflake executable.\nExecute: pip install autoflake to install it"))))

  (defun yc/python-remove-unused-import ()
    "Hook to call `python-remove-unused-imports' after saving python file."
    (when (and buffer-file-name
               (equal major-mode 'python-mode))
      (python-remove-unused-imports)))

  :hook ((after-save . yc/python-remove-unused-import))

  :custom
  (python-shell-interpreter "python")
  (python-indent-guess-indent-offset-verbose nil)
  )

;; ***************** sh-mode *****************
(use-package sh-script
  :mode (((rx (or (: (+? ascii) "." (or "zsh" "ebuild" "eclass"))
                  (: (or ".bashrc.d" ".zshrc.d") "/" (+ nonl) "rc")
                  (: "/etc/" (or "init.d" "zsh" "profi") (+ nonl))
                  (: (+? nonl) "/zsh/" (+? nonl) "functions" (+ nonl))
                  "PKGBUILD"
                  )
              buffer-end)
          . sh-mode))
  :custom
  (sh-builtins
   (quote
    (
     (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait" "die"
            "edebug" "elog" "einfo" "ewarn" "ebegin" "eend" "PDEBUG")

     (bash sh-append shell "." "alias" "bg" "bind" "builtin" "caller" "compgen" "complete"
           "declare" "dirs" "disown" "enable" "fc" "fg" "help" "history" "jobs" "kill"
           "let" "local" "popd" "printf" "pushd" "shopt" "source" "suspend" "typeset"
           "unalias" "command" "hash" "test" "type" "eval" "export" "getopts" "newgrp" "pwd"
           "read" "readonly" "times" "ulimit" "alias" "bg" "false" "fc" "fg"
           "jobs" "kill" "let" "print"  "time" "typeset" "unalias" "whence"
           "edebug" "einfo" "ewarn" "eerror" "emerge" "alias_if_exists")

     (zsh sh-append bash "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
          "disable" "disown" "echotc" "enable" "functions" "getln" "hash" "history"
          "integer" "limit" "local" "log" "popd" "pushd" "r" "readonly" "rehash" "sched"
          "setopt" "source" "suspend" "true" "ttyctl" "type" "unfunction" "unhash"
          "unlimit" "unsetopt" "vared" "which" "zle" "compdef" "compinit" "zstyle" "colors"))))
  :config
  (progn
    (yc/add-run-unit 'shell 70
     (when (or (equal ext "sh")
               (equal ext "SH"))
       (lambda ()
         (interactive)
         (format "./%s" file)))))

  :hook (
         (sh-mode . (lambda ()
                      (yc/lsp--setup "bash-language-server" "npm i -g bash-language-server"))))
  )

 ;; Make script executable.

(defvar new-script-list nil "List of newly created scripts.")
(defun check-script-status ()
  "Check status of scripts."
  (unless (and buffer-file-name
               (file-exists-p buffer-file-name))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (and (looking-at "^#!")
                   (not (member buffer-file-name new-script-list)))
          (add-to-list 'new-script-list buffer-file-name))))))

(defun make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable."
  (interactive)
  (when (or (member buffer-file-name new-script-list)
            (called-interactively-p 'interactive))
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o111))
    (setq new-script-list (delete buffer-file-name new-script-list))
    (message "Made %s executable" buffer-file-name)))

(add-hook 'before-save-hook 'check-script-status)
(add-hook 'after-save-hook 'make-script-executable)


(provide '053-prog-scripts)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 053-prog-scripts.el ends here
