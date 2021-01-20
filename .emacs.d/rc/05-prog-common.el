;;; 05-prog-common.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

(use-package which-func
  :commands (which-function-mode)
  :hook ((prog-mode . which-function-mode))
  :config
  (remove-hook 'after-change-major-mode-hook 'which-func-ff-hook)

  (defadvice! yc/which-func-update-adv (&rest args)
    "Apply function update to all windows.
ORIG-FUNC is called with ARGS."
    :override #'which-func-update
    nil
  (walk-windows
   (lambda (w)
     (when which-function-mode
       (which-func-update-1 w))) nil 'visible)))

(use-package flycheck
  :ensure t
  :commands (flycheck-mode global-flycheck-mode flycheck-define-checker)
  :bind (:map flycheck-mode-map
              ([M-S-f9] . flycheck-first-error)
              ([S-f9] . flycheck-list-errors)
              ([f9] . flycheck-next-error)
              ([M-f9] . flycheck-previous-error))
  :hook ((c++-mode . (lambda ()
                       (setq flycheck-clang-language-standard "c++11"
                             flycheck-gcc-language-standard "c++11"))))
  :custom
  (flycheck-checker-error-threshold nil)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-flake8-maximum-line-length 120)

  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (flycheck-check-syntax-automatically
   '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (flycheck-display-errors-delay 0.25)

  :config
  (defadvice! yc/flycheck-next-error-adv (orig-func  &rest args)
    "Wrap to first error when tail reached.
Call ORIG-FUNC which is 'flycheck-next-error with ARGS."
    :around #'flycheck-next-error
    (condition-case var
        (apply orig-func args)
      (user-error (progn
                    (PDEBUG "Wrap to first error...")
                    (flycheck-first-error))))
    )

  (defadvice! yc/lsp-adv-flycheck (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :after  #'lsp
    (flycheck-mode 1))

)

(use-package flycheck-popup-tip
  :ensure t
  :custom
  (flycheck-popup-tip-error-prefix "✕ ")
  :commands (flycheck-popup-tip-mode)
  :hook ((flycheck-mode . flycheck-popup-tip-mode)))

 ;; EMR.
(use-package iedit :ensure t)
(use-package emr
  :bind (([S-f6] . emr-show-refactor-menu))
  :commands (emr-initialize)
  ;; :local-repo (expand-file-name "site-lisp/emr" user-emacs-directory)
  :custom
  (emr-clang-format-style '(("BasedOnStyle" . "LLVM")
                            ("IndentWidth" . "4")
                            ("BreakBeforeBraces" . "Linux")
                            ("AllowShortIfStatementsOnASingleLine" . "false")
                            ("AlignConsecutiveAssignments". "true")
                            ("AccessModifierOffset". -4)
                            ("IndentCaseLabels" . "false")
                            ("PointerAlignment" . "Left")
                            ("UseTab" . "Never"))))


 ;; Common Settings for prog-mode.
(yc/defmacro yc/add-keyword (sym type)
  `(font-lock-add-keywords
    nil (list (list ,sym 1 ,type t))))

;;;; Function and macro to add an regular expression string formed by (rx)
;;;; macro into specified face.

(defun yc/warning-keywords-matcher (limit)
  "Setup warning keywords.
LIMIT is the limit of search."
  (let ((case-fold-search t))
    (re-search-forward
     (rx (group (or "@" bow)
                    (or "bug" "fixme" "note" "todo" "todolist" "xxx" "yyc"
                        "deprecated"  "obsolete" "wip"
                        )
                    ":"))
     limit 'no-error)))

(defun setup-prog-keywords ()
  "Highlight additional keywords."
  (font-lock-add-keywords nil '((yc/warning-keywords-matcher
                                 1 font-lock-warning-face t)))

  (unless (member major-mode '(mhtml-mode html-mode nxml-mode))
    (yc/add-keyword (rx (repeat 120 not-newline) (group (+? not-newline)) eol) 'font-lock-warning-face))
  (yc/add-keyword (rx bow (group (or "NIL" "nil")) eow) 'font-lock-constant-face)
  )

 ;;;; CEDET Settings

;; Semantic mode.
(use-package semantic
  :commands (semantic-mode semantic-force-refresh)
  :init
  (custom-set-variables
   '(semantic-default-submodes nil
                               (quote (global-semantic-decoration-mode
                                       global-semantic-idle-summary-mode
                                       global-semantic-idle-scheduler-mode
                                       global-semanticdb-minor-mode
                                       global-semantic-mru-bookmark-mode))
                               )
   '(semantic-idle-scheduler-idle-time 15)
   '(semantic-idle-scheduler-max-buffer-size 102400)
   '(semantic-lex-maximum-depth 20)
   '(pulse-flag 'never)
   '(semanticdb-default-save-directory (yc/make-cache-path "semanticdb"))
   '(srecode-map-save-file (yc/make-cache-path "srecode-map.el")))
  :config
  (defadvice! yc/semantic-fetch-tags-adv (&rest args)
    "Enable semantic mode, if not enabled yet.
ORIG-FUNC is called with ARGS."
    :before  #'semantic-fetch-tags
    (unless semantic-mode
      (semantic-mode 1))))

(use-package semantic/decorate/mode
  :defer t
  :config
  (progn
    (setq-default semantic-decoration-styles
                  '(("semantic-decoration-on-includes" . t)))))


;;;; Semanticdb 定制
;; Semantic DataBase存储位置
(use-package semantic/db-mode
  :commands (global-semanticdb-minor-mode)
  :config
  (setq-mode-local c-mode semanticdb-find-default-throttle
                   '(project unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
                   '(project unloaded system recursive)))

(use-package srecode/mode
  :commands (srecode-minor-mode)
  ;; :hook ((prog-mode . srecode-minor-mode)) ;; disabled for now, not used...
  :bind (:map prog-mode-map
              ("\C-cdc" . srecode-document-insert-comment)
              ("\C-cdf" . srecode-document-insert-function-comment)
              ("\C-cdv" . srecode-document-insert-variable-one-line-comment))

  :custom
  (srecode-insert-ask-variable-method 'field)
  :config
  (progn
    (dolist (dir '("~/.emacs.d/templates/srecode"
                   "~/.emacs.d/templates/srecode/private"))
      (add-to-list 'srecode-map-load-path (expand-file-name dir)))))


 ;; **** Unified Stack ****
(use-package gxref
  :ensure
  :commands (gxref-xref-backend))

(use-package ivy-xref
  :ensure
  :commands (ivy-xref-show-xrefs)
  :config
  (autoload 'yc/ivy-xref-show-xrefs-adv "prog-utils")
  (advice-add 'ivy-xref-show-xrefs :after #'yc/ivy-xref-show-xrefs-adv)

  (defadvice! yc/ivy-xref-make-collection-adv (orig-func xrefs)
    "Transform XREFS into a collection for display via `ivy-read', using
    relative file name. ORIG-FUNC is ignored.."
    :around  #'ivy-xref-make-collection
    (let ((collection nil)
          (root (projectile-project-root)))

      (dolist (xref xrefs)
        (with-slots (summary location) xref
          (let* ((line (xref-location-line location))
                 (file (xref-location-group location))
                 (candidate
                  (concat
                   (propertize
                    (concat
                     (if (and root
                              (string-prefix-p root file))
                         ;; TODO: hide some part of file name if it is too long...
                         (substring file (length root))
                       file)
                     (if (integerp line)
                         (format ":%d: " line)
                       ": "))
                    'face 'compilation-info)
                   (progn
                     (when ivy-xref-remove-text-properties
                       (set-text-properties 0 (length summary) nil summary))
                     summary))))
            (push `(,candidate . ,location) collection))))
      (nreverse collection))))

(use-package xref
  :commands (xref-backend-identifier-at-point  xref-find-backend)
  :config
  (progn
    (setq xref-show-xrefs-function 'ivy-xref-show-xrefs
          xref-show-definitions-function  'ivy-xref-show-defs)
    (setq-default xref-backend-functions '(gxref-xref-backend))))


(use-package semantic-uml
  :commands (uml/struct-to-dot uml/struct-to-dia uml/struct-to-puml)
  :bind (:map prog-mode-map
              ("\C-csD" . uml/struct-to-dot)
              ("\C-csd" . uml/struct-to-puml)
              ("C-c s M-d" . uml/struct-to-puml-fields-only)))

 ;; lsp
(defvar yc/lsp-warned-mode-list nil "List of modes already been warn for disabling LSP.")

(defun yc/lsp--setup (condition install-tip &optional prepare-func)
  "Setup LSP.
Enable LSP if EXECUTABLE is t, and if `SETUP-FUNC' is not nil,
call this function to setup LSP.  Or show INSTALL-TIP.
If PREPARE-FUNC is given, evaluate before starting lsp."
  (let ((met
         (cond
          ((stringp condition) (or (executable-find condition)
                                   (file-executable-p condition)))
          ((functionp condition) (funcall condition))
          (t condition))))

    (PDEBUG "ENTER ys/lsp-setup, CONDITION: "
            condition
            ", met: " met)

    (if met
        (progn
          (if prepare-func
              (funcall prepare-func))
          (lsp)
          )

      (unless (member major-mode yc/lsp-warned-mode-list)
        (add-to-list 'yc/lsp-warned-mode-list major-mode)
        (warn
         "LSP is disabled for %s since %s not found.%s"
         (symbol-name major-mode) condition
         (format "\n                Try install %s with command: '%s' to enable LSP."
                 condition install-tip))))))

(use-package lsp-mode
  :preface
  (autoload 'lsp-headerline--build-symbol-string "lsp-headerline")

    (defun yc/lsp-which-function ()
    "Returns current function symbol with support of LSP."
    (awhen (and
            buffer-file-name
            (bound-and-true-p lsp-mode)
            (lsp-feature? "textDocument/documentSymbol")
            (s-trim (lsp-headerline--build-symbol-string)))
      (if (> (length it) 0)
          (s-replace-regexp " *> *" "::" it))))

  (yc/eval-after-load 'which-func
    (pushnew! which-func-functions #'yc/lsp-which-function))

  (defun yc/modeline-update-lsp ()
    "Update `lsp-mode' status."
    (setq-local yc/modeline--lsp
                (if (bound-and-true-p lsp-mode)
                    (let ((symbol-face 'font-lock-keyword-face)
                          (server-name "--"))
                      (if-let (workspaces (lsp-workspaces))
                          (setq server-name
                                (string-join
                                 (--map
                                  (format "%s"
                                          (-> it lsp--workspace-client lsp--client-server-id symbol-name))
                                  workspaces))
                                symbol-face 'bold-italic))
                      (concat " " (propertize "" 'face symbol-face)
                              "[" (propertize server-name   'face 'italic)
                              "]")))))

  (defun yc/lsp-format-adv (func &rest args)
    "Advice for 'lsp-format-buffer'.
Call FUNC which is 'lsp-format-buffer with ARGS."
    (let ((p (point)) )
      (apply func args)
      (if (= (point-min) (point))
          (goto-char p))))

  (defun yc/lsp-switch-client ()
    "Switch to another LSP server."
    (interactive)
    (require 'lsp-mode)

    (let ((matching-clients
           (mapcar (lambda (client)
                     (cons (lsp--client-server-id client)
                           (lsp--client-priority client)))
                   (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                #'lsp--server-binary-present?)))))
      (unless matching-clients
        (error "No matching lsp client was found"))

      (let* ((client (ivy-read "Choose client: " matching-clients))
             (match (car (lsp--filter-clients
                          (lambda (c) (eq  (lsp--client-server-id c) (intern client))))))
             (workspaces (lsp-workspaces)))
        (unless match
          (user-error "Couldn't find an LSP client named %S" client))

        (PDEBUG "CLIENT" (stringp client)
                "MATCH:" match)

        (let ((old-priority (lsp--client-priority match)))
          (setf (lsp--client-priority
                 (car (lsp--filter-clients
                       (lambda (c)
                         (eq  (lsp--client-server-id c) (intern client))))))
                9999)
          (unwind-protect
              (if workspaces
                  (lsp-workspace-restart
                   (if (cdr workspaces)
                       (lsp--completing-read "Select server: "
                                             workspaces
                                             'lsp--workspace-print
                                             nil t)
                     (car workspaces)))
                (lsp-mode +1))
            (setf (lsp--client-priority
                   (car (lsp--filter-clients
                         (lambda (c)
                           (eq  (lsp--client-server-id c) (intern client))))))
                  old-priority))))))

  :commands (lsp lsp-workspaces lsp--workspace-print lsp-format-region
                 lsp-format-buffer)
  :custom
  (lsp-diagnostics-provider :flycheck)
  (lsp-diagnostics-flycheck-default-level 'warn)

  (lsp-restart 'ignore)

  (lsp-auto-configure t)

  (lsp-enable-imenu nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links t)
  (lsp-enable-snippet t)
  (lsp-log-io nil)

  (lsp-keep-workspace-alive t)

  (lsp-completion-enable t)
  (lsp-completion-provider :capf)

  (read-process-output-max (* 16 1024 1024))

  ;; Disable features that have great potential to be slow.
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-semantic-tokens-enable nil)

  ;; Don't modify our code without our permission
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)

  (lsp-keymap-prefix nil)

  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)

  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)

  :hook
  ((lsp-after-open . (lambda ()
                       (setq-local xref-backend-functions
                                   (cons #'lsp--xref-backend
                                         xref-backend-functions)))))
  (lsp-mode . yc/modeline-update-lsp)
  (lsp-after . yc/modeline-update-lsp)
  (buffer-list-update . yc/modeline-update-lsp)


  :config

  (defadvice! yc/lsp-adv (&rest args)
    "Advice for 'lsp'.
Call ORIG-FUNC which is 'lsp with ARGS.
Loading project specific settings before starting LSP."
    :before-while #'lsp
    ;; Load project-specific settings...
    (aif (condition-case err
             (yc/lsp-load-project-configuration)
           (user-error
            (progn
              (PDEBUG "LSP is disabled:" (cdr err))
              nil))
           (error (progn
                    (PDEBUG "LSP error: %s" (cdr err))
                    nil)))
        it

      ;;  should not load lsp, use semantic-mode
      (semantic-mode 1)
      (when (buffer-file-name)
        (semantic-force-refresh))
      (yc/push-find-func #'semantic-ia-fast-jump)
      nil))

  (advice-add 'lsp-format-buffer :around #'yc/lsp-format-adv)
  (advice-add 'lsp-format-region :around #'yc/lsp-format-adv)

  (defadvice! yc/lsp--imenu-create-index-adv (orig-func  &rest args)
    "Update cached symbols.
Call ORIG-FUNC which is 'lsp--imenu-create-index with ARGS."
    :around #'lsp--imenu-create-index
    (if (= yc/document-symbols-tick (buffer-chars-modified-tick))
        yc/cached-symbols

      (PDEBUG "Refreshing tags..." )
      (setq yc/document-symbols-tick (buffer-chars-modified-tick)
            yc/cached-symbols (apply orig-func args))))

  (defadvice! yc/lsp-enable-imenu-adv (&rest args)
    "Disable lsp-enable-imenu, which is very slow if there are lots of symbols.
ORIG-FUNC is called with ARGS."
    :before-until #'lsp-enable-imenu
    t)

  (defadvice! yc/lsp--suggest-project-root-adv (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :before-until #'lsp--suggest-project-root
    (when-let (root-file (yc/lsp-get-root-file))
      (expand-file-name (file-name-directory root-file))))

  (defadvice! yc/lsp-completion-mode-adv (&rest arg)
    "Remove `company-capf' from `company-backends'."
    :after  #'lsp-completion-mode
    (when (eq (car company-backends) 'company-capf)
      (pop company-backends))))


(defvar-local yc/cached-symbols nil "last cached index.")
(defvar-local yc/document-symbols-tick -1 "last tick of modification.")
(add-to-list 'auto-mode-alist (cons ".lsp-conf" 'emacs-lisp-mode))

(defun yc/lsp-get-root-file ()
  "Return root-file for lsp."
  (interactive)

  (defun normallize-file (file)
    (let ((fn (expand-file-name file)) )
      (if (s-ends-with-p "/" fn)
          (substring fn 0 -1)
        fn)))

  (let* ((f-list '(".lsp-conf" ".ccls-root" ".ccls" ".git"))
         (root-file
          (catch 'p-found
            (dolist (item f-list)
              (PDEBUG "YC/LSP-GET-ROOT-FILE: ITEM " item)
              (aif (locate-dominating-file default-directory item)
                  (throw 'p-found (concat (expand-file-name it) item)))))))

    (PDEBUG "FOUNT: " root-file)

    (if (and root-file
             (string= (normallize-file (getenv "HOME"))
                      (normallize-file (file-name-directory root-file))))
        (setq root-file nil))

    (if (called-interactively-p 'interactive)
        (message "Root file: %s." root-file))
    root-file))


(defun yc/lsp-get-log-file (client rootdir)
  "Get logfile name for specified workspace in ROOTDIR & CLIENT."
  (when (and (file-exists-p rootdir)
             (not (file-directory-p rootdir)))
    (setq rootdir (file-name-directory rootdir)))

  (format "%s%s_%s_%s.log" (temporary-file-directory)
          (aif (file-remote-p default-directory)
              (if (string-match
                   (rx bol "/" (or "ssh" "scp" "sudo") ":"
                       (group (+? nonl)) "@" (+? nonl) ":")
                   it)
                  (match-string 1 it)
                (error "File %s not handled"))
            user-login-name)
          client
          (let ((cmps (reverse (s-split "/" rootdir))))

                         (while (= (length (car cmps)) 0)
                           (pop cmps))
                         (or
                          (pop cmps)
                          "unamed"))))

(defun yc/lsp-load-project-configuration ()
  "Advice for 'lsp', loading project specific configurations."
  (interactive)
  (PDEBUG "MAJOR-MODE: " major-mode)

  (let* ((root-file (yc/lsp-get-root-file))
         (mode-specific-func
          (intern (concat "yc/lsp-load-project-configuration" "-"
                          (symbol-name major-mode)))))

    (PDEBUG "ROOT:" root-file)
    (cond
     ((not root-file)
      (user-error "Failed to parse root file"))
     ((file-directory-p root-file)
      (user-error "Root %s is a directory" root-file))
     ((string= (file-name-nondirectory root-file) ".git")
      (user-error "Root is .git, ignoring it"))
     ((string= (file-name-nondirectory root-file) ".lsp-conf")
      (progn
        (PDEBUG "Loading project settings from: " root-file)
        (with-temp-buffer
          (insert-file-contents root-file)
          (eval-buffer)
          (message "Loaded project configuration from %s" root-file))))
     (t (progn
          (PDEBUG "Enabling LSP, root file is:" root-file))))

    (if (fboundp mode-specific-func)
        (progn
          (PDEBUG "Loading: " mode-specific-func)
          (funcall mode-specific-func root-file))
      (PDEBUG "Project settings are ignored, function "
              mode-specific-func " is not defined..."))

    (PDEBUG "leave")
    t))


(use-package imenu
  :commands (imenu--make-index-alist)
  :custom
  (imenu-auto-rescan-maxout (* 1024 1024)))


;;;; Common Program settings
(use-package prog-utils
  :commands (yc/insert-single-comment
             yc/show-methods-dwim  yc/push-stack
             yc/push-find-func
             yc/asm-post-process)
  :bind (:map esc-map
              ("." . yc/find-definitions)
              ("?" . yc/find-references)
              ("*" . yc/return-func)
              ("r" . yc/return-reflist)
              ("i" . yc/find-implementation)
              ("m" . yc/show-methods-dwim))
  :bind (([remap rectangle-mark-mode] . yc/store-current-location))
  :bind (:map prog-mode-map
              ("C-c o" . yc/open-header)
              ("C-c d h" . yc/header-make)
              ("C-c d e" . yc/insert-empty-template)
              ("C-c d s" . yc/insert-single-comment)
              ("C-c C-a" . yc/wip-comment)
              )

  :hook ((asm-mode . yc/asm-post-process)))

(defun setup-prog-keybindings()
  "Common program-keybindings."
  (interactive)
  (local-set-key (kbd "M-|") 'align))

(defun yc/common-program-hook ()
  "My program hooks."
  (PDEBUG
    "FILE: " (or buffer-file-name (current-buffer))
    "MODE: " major-mode)

  (setq show-trailing-whitespace t)
  (setup-prog-keywords)
  (setup-prog-keybindings)
  (emr-initialize)

  (yc/run-with-idle-timer
   0.5 nil
   (lambda ()
     (unless (bound-and-true-p flycheck-mode)
        (flycheck-mode 1)))))

(use-package prog-mode
  :defer t
  :hook ((prog-mode . yc/common-program-hook)))


(cdsq yc/compile-commands nil
  "List of CompileUnit to be checked for `compile'.
Each CompileUnit is actually a function who accepts two parameters:
`file' : name of file & `ext' extension name.
This function returns a string as compile command, or nil if it can't handle
  the input file.")

(cdsq yc/run-commands nil
  "List of CompileUnit to be checked for `run'.
Each CompileUnit is actually a function who accepts two parameters:
`file' : name of file & `ext' extension name.
This function returns a string as compile command, or nil if it can't handle
  the input file.")


(yc/defmacro yc/add-unit-to-target (target name priority &rest body)
  "Append to an unit to `TARGET'.
`NAME' name of this unit, for debug purpose.
`BODY' function to call, should return a command, or nil.
`PRIORITY' priority of this unit, if multiple units are available, unit with higher priority
  should be chosen."
  `(if (numberp ,priority)
       (add-to-list ',target (list :name ,name
                                   :priority ,priority
                                   :func (lambda (file ext) ,@body)))
     (error "Priority of %s should be a number" ,name)))

(yc/defmacro yc/add-compile-unit (name priority &rest body)
  "Append to yc/compile-commands."
  `(yc/add-unit-to-target yc/compile-commands ,name ,priority ,@body))

(yc/defmacro yc/add-run-unit (name priority &rest body)
  "Append to yc/run-commands."
  `(yc/add-unit-to-target yc/run-commands ,name ,priority ,@body))

(yc/add-compile-unit 'makefile 99
  (aif (or (yc/file-exists-p "makefile")
           (yc/file-exists-p "Makefile"))
      (lambda ()
        (counsel-make nil t t))))

(defun get-command-from-list (commands)
  "Get command from LST."
  (let* ((file (if buffer-file-name (file-name-nondirectory buffer-file-name)))
         (ext (if file (file-name-extension file)))
         best)
    (dolist (unit commands)
      (let ((name (plist-get unit :name))
            (priority (plist-get unit :priority)))
        (PDEBUG "Checking unit: " name)
        (awhen (funcall (plist-get unit :func) file ext)
          (when (or (not best) ;; never set
                    (> priority (plist-get best :priority))) ;; new one has higher priority
            (PDEBUG "Updating best match: "
              (plist-get best :name) "("(plist-get best :priority)") ==> "
              name"("priority")")
            (setq best (list :name name :priority priority :cmd it))))))

    (PDEBUG "FINAL: " best)
    (awhen (plist-get best :cmd)
      (funcall it))))

(defun get-compile-command()
  "Return command to compile current target."
  (get-command-from-list yc/compile-commands))

(defun get-run-command()
  "Return command to run current target."
  (get-command-from-list yc/run-commands))


(use-package dash-docs
  :config
  (setq dash-docs-docsets-path
        (let ((original-dash-path (expand-file-name "~/Library/Application Support/Dash/DocSets")))
          (if (and IS-MAC
                   (file-directory-p original-dash-path))
              original-dash-path
            (expand-file-name "~/Documents/dash-docsets")))
        dash-docs-browser-func 'eww
        dash-docs-common-docsets (dash-docs-installed-docsets)))

(use-package counsel-dash
  :commands (counsel-dash counsel-dash-at-point counsel-dash-install-docset)
  :bind (("<M-f1>" . counsel-dash-at-point))
  )


(provide '05-prog-common)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 05-prog-common.el ends here
