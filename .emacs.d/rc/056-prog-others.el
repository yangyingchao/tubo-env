;;; 056-prog-others.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

(use-package counsel-woman
  :bind (([f1] . counsel-woman))
  :custom
  (woman-use-own-frame nil)
  :config
  (aif (getenv "EPREFIX")
      (mapc
       (lambda (x)
         (push (concat (file-name-as-directory it) x) woman-manpath))
       '("usr/share/man"
         "usr/local/share/man"
         "share/man"
         "local/share/man")))

  )

 ;; php mode
(use-package php-mode
  :mode (rx "." (or (: "php" (* (or "s" "t" digit))) "phtml" "Amkfile" "amk"))
  :interpreter ("php" . php-mode))


(use-package systemtap-mode :mode "\\.stp\\'")

 ;; Javascript mode
(use-package js-mode
  :commands (js-mode)
  :mode (rx (or (: bow "manifest") ".json" ".js" ".pac") eol)
  :config
  (yc/add-safe-local-var '(js2-basic-offset . 4))
  :bind (:map js2-mode-map
              ("\C-c\C-x" . executable-interpret)))


;; (use-package java
;;   :defer t
;;   :hook ((java-mode
;;           .
;;           (lambda ()
;;             ;; (yc/add-company-backends company-eclim)
;;             (unless (featurep 'lsp-java)
;;               (require 'lsp-java))
;;             (lsp)))))


(use-package batch-mode :mode (rx "." (or "bat" "cmd")))

;; Poweshell
(use-package powershell-mode
  :mode "\\.ps1\\'"
  :bind (:map powershell-mode-map
              ([(f1)] .  'yc/pws-get-help)
              ([(meta .)] . 'yc/pws-find-tag)))

(defun yc/pws-find-tag (function)
  "Find defination of FUNCTION under current poin."
  (interactive
   (let* ((fn (thing-at-point 'symbol))
          (val nil))
     (message fn)
     (setq val (completing-read (if fn
                                    (format "search for (default %s): " fn)
                                  "search for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (let ((cmd nil))
    (if (null function)
        (message "you didn't specify a function")
      (progn
        (setq cmd (concat "egrep -i \"^function +" function "\" . -ri"))
        (eshell-command cmd)
        (pop-to-buffer (get-buffer "*grep*"))
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (kill-line 3)
        (insert (concat "*********************** find tag for:"
                        function "********************\n\n"))
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun yc/pws-get-help (function)
  "Display the documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (thing-at-point 'symbol))
         val)
     (message fn)
     (setq val (completing-read (if fn
                                    (format "help for (default %s): " fn)
                                  "help for: ")
                                obarray 'fboundp t nil nil
                                ))
     (list (if (equal val "")
               fn (intern val)))))

  (if (null function)
      (message "you didn't specify a function")
    (progn
      (start-process "powershell-help" nil "devhelp" "-s" function))))

(use-package yaml-mode
  :mode (rx (or ".clang-format" (: ".y" (? "a" ) "ml")) eol)
  :hook ((yaml-mode .           (lambda ()
            (unless (buffer-file-name)
              (flycheck-mode -1))

            (yc/lsp--setup "yaml-language-server"
                           "npm install -g yaml-language-server")))))


(use-package qml-mode :mode "\\.qml$")
(use-package swig-mode  :mode (rx (or ".i" ".swig") eol))

(defun yc/bison-setup-imenu-function ()
  "Setup imenu-index-function for Bison mode.."
  (interactive)
  (PDEBUG "ENTER.")
  (setq imenu-create-index-function
        (lambda ()
          (let ((end))
            (beginning-of-buffer)
            (re-search-forward "^%%")
            (forward-line 1)
            (setq end (save-excursion (re-search-forward "^%%") (point)))
            (cl-loop while (re-search-forward "^\\([a-z].*?\\)\\s-*\n?\\s-*:" end t)
                  collect (cons (match-string 1)
                                (point))))))
  (PDEBUG "BUF:" (current-buffer)
          "FUNC:" imenu-create-index-function))

(use-package bison-mode
  :mode (rx "." (or "yy" "y" "jison") eol)
  :config
  (progn
    (setq  bison-rule-enumeration-column 10)))

 ;; SQL Mode
(use-package sqlup-mode  :commands (sqlup-mode)
  :config
  (defadvice! yc/sqlup-comment-p-adv (&rest args)
    "Check if we are in comment."
    :override #'sqlup-comment-p
    (looking-back (rx (or "#" "--") (*? nonl))) nil)

  (defadvice! yc/sqlup-capitalize-as-you-type-adv (&rest args)
    "Don't invoke original function unless buffer is modified."
    :before-while #'sqlup-capitalize-as-you-type
    (buffer-modified-p))
  :hook ((sql-mode . sqlup-mode)))


(use-package sql-indent
  :commands (sqlind-minor-mode)
  :config
  (setq sqlind-indentation-offsets-alist
        `((select-clause 0)
          (insert-clause 0)
          (delete-clause 0)
          (update-clause 0)
          ,@sqlind-default-indentation-offsets-alist))

  :hook ((sql-mode . sqlind-minor-mode)))

(use-package sql+
  :commands (sql/choose-dbms sql/choose-database company-sql
                             company-sql-update-candidates
                             eshell/restart_pg sql/remove-costs)
  :bind (:map sql-mode-map
              (;; (kbd "C-c C-x")
               "" . sql/eval-sql)))

(defun yc/sql-mode-hook ()
  "My hook to run for sql mode."
  (ws-butler-mode -1)
  )

(use-package sql
  :mode ((rx (or (: "." (or "sql" "ddl") (? (or "_in" ".result" ".reject")))
                 (: (or "input" "output") "/" (+? nonl) ".source")
                 (: "test/" (+? nonl) (or "output" "input") "/"
                    (+? nonl) ".source")
                 (: (or "results" "expected") (*? nonl) "/"
                    (+? nonl) (or ".out" ".source"))
                 ) eol)  . sql-mode)
  :init
  (add-hook! '(sql-mode-hook sql-interactive-mode-hook) #'yc/sql-mode-hook)
  :config
  (yc/set-company-backends '(sql-mode sql-interactive-mode)
     'company-files 'company-sql 'company-keywords  'company-dabbrev-code 'company-dabbrev)
  (progn
    (require 'sql+)))

 ;; protobuf-mode
(use-package protobuf-mode :mode (rx ".proto" eol))

 ;; console mode
(use-package console-mode  :commands (console-mode))

 ;; scala-mode.
(use-package ensime
  :preface
  (defun yc/ensime-find-definition (pt)
    "Find definition at PT."
    (unless (fboundp 'ensime-edit-definition)
      (require 'ensime))
    (if (ensime-edit-definition nil)
        (yc/push-stack (cons 'ensime-pop-find-definition-stack nil))))
  :commands (ensime)
  :custom
  (ensime-startup-notification nil)
  (ensime-startup-snapshot-notification nil)
  (ensime-db-default-port "5005")
  (ensime-startup-dirname (yc/make-cache-path "ensime"))
  :config
  (yc/add-company-backends 'ensime-mode 'ensime-company))

(use-package scala-mode
  :mode (rx ".scala" eol)
  :hook ((scala-mode . yc/scala-mode-hook))
  :config
  (modify-syntax-entry ?\" "\"" scala-syntax:syntax-table)
  (require 'ensime))


(use-package cperl-mode
  :interpreter ("perl" . cperl-mode)
  :mode "\\.\\([pP][Llm]\\|al\\)$"
  :custom
  (cperl-extra-newline-before-brace t )
  (cperl-brace-offset -2              )
  (cperl-merge-trailing-else nil      )
  :hook ((cperl-mode .
                     (lambda ()
                       (yc/lsp--setup t
                                      "cpan -f -i Perl::LanguageServer"))))
  :config
  (yc/add-company-backends 'cperl-mode 'company-dabbrev-code 'company-dabbrev)
  (yc/add-safe-local-var '(cperl-brace-imaginary-offset . 0)))

 ;; swig
(yc/add-compile-unit 'swig 55
 (when (or (equal ext "i")
           (equal ext "swig"))
   (lambda ()
     (interactive)
     (format "%s -c++ -java %s" (executable-find "swig") file))))


 ;; llvm
;; (use-package llvm-mode
;;   :mode (rx ".ll" eol)
;;   )

;; (use-package tablegen-mode
;;   :mode (rx ".td" eol))

;; (require 'autodisass-llvm-bitcode)

 ;; rust
(use-package rust-mode
  :init
  (yc/add-compile-unit 'rust 60
    (if (locate-dominating-file default-directory "Cargo.toml")
        (lambda ()
          (interactive)
          (if (executable-find "cargo")
              (concat "cargo build" (if current-prefix-arg " --release"))
            (error "Can't find executable: cargo")))
      (when (equal major-mode 'rust-mode)
        (lambda ()
          (interactive)
          (if (executable-find "rustc")
              (concat "rustc " file)
            (error "Can't find executable: rustc"))))))

  (yc/add-run-unit 'rust 60
    (aif (locate-dominating-file default-directory "Cargo.toml")
        (lambda ()
          (interactive)
          (let* ((target (if current-prefix-arg "test" "run"))
                 (custom-script (concat (expand-file-name it) "my_script_"
                                        target)))
            (if (and (file-exists-p custom-script)
                     (file-executable-p custom-script))
                custom-script
              (if (executable-find "cargo")
                  (concat "cargo "
                          (if current-prefix-arg
                              (if (listp current-prefix-arg)
                                  "test -- --nocapture"
                                "test"
                                )
                            "run"))
                (error "Can't find executable: cargo")))))

      (when (equal major-mode 'rust-mode)
        (lambda ()
          (interactive)
          (if (executable-find "rustc")
              (concat "rustc " file " && ./" (file-name-sans-extension file))
            (error "Can't find executable: rustc"))))))
  :hook ((rust-mode
          .
          (lambda ()
            (interactive)
            (yc/lsp--setup "rls" "rustup-init component add rls --toolchain'"))))
  :config
  (defun yc/rust-trace-on ()
    "Turn on trace of rust.."
    (interactive)
    (setenv "RUST_BACKTRACE" "1"))

  (defun yc/rust-trace-off ()
    "Turn off trace of rust.."
    (interactive)
    (setenv "RUST_BACKTRACE" "0"))

  :mode ((rx ".rs" buffer-end)))


(use-package lua-mode
  :commands (lua-mode)
  :mode (rx "." (or "lua") eow)
  )

(use-package typescript-mode
  :mode (rx "." (or "ts") buffer-end)
  :hook ((typescript-mode . lsp)))

(use-package go-mode
  :mode "\\.go\\'"
  )

(use-package groovy-mode
  :mode (rx "." (or "grovvy" "gradle") buffer-end)
  )


(provide '056-prog-others)


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 056-prog-others.el ends here
