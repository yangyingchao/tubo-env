;;; 051-prog-c.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:
;;;  Provides configurations for c family.
;;; Code:

 ;; cc-mode and derived.
(use-package hideif
  :commands (hide-ifdef-mode hif-set-var)
  :custom
  (hide-ifdef-initially t)
  (hide-ifdef-read-only nil)
  (hide-ifdef-shadow nil)

  :config
  (advice-add
   'hide-ifdefs :around
   (lambda (&rest args)
     (interactive)
     (setq hif-outside-read-only buffer-read-only)
     (unless hide-ifdef-mode (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
     (if hide-ifdef-hiding
         (show-ifdefs))			; Otherwise, deep confusion.
     (setq hide-ifdef-hiding t)
     (hide-ifdef-guts)
     (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))))

  (defadvice! yc/hif-set-var-adv (key val)
    "Don't set hide-ifdef-env if KEY VAL is already added."
    :override  #'hif-set-var
    (let ((pair (cons key val)))
      (unless (member pair hide-ifdef-env)
        (setq hide-ifdef-env (cons pair hide-ifdef-env)))))


  (define-key hide-ifdef-mode-map "\C-c@t" 'hide-ifdef-toggle-shadowing))


(use-package clang-format
  :commands (clang-format-region clang-format-buffer))

(use-package cwarn
  :hook ((c-mode-common . cwarn-mode)))

(use-package c-utils
  :commands (yc/get-c-style yc/format-files yc/switch-h-cpp yc/enable-disable-c-block
             yc/preprocess-file yc/insert-empty-template yc/header-make
             c++filt-buffer yc/cc-c-c++-objc-mode))

(use-package ccls
  :custom
  (ccls-executable (or (executable-find "ccls.sh") "ccls"))
  ;; (ccls-sem-highlight-method 'font-lock)
  :config
  (defadvice! yc/ccls--suggest-project-root-adv (&rest args)
      "Only enable ccls for some modes.
ORIG-FUNC is called with ARGS."
      :before-while #'ccls--suggest-project-root
      (memq major-mode '(c-mode c++-mode cuda-mode objc-mode)))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection (lambda () (cons ccls-executable ccls-args)))
      :major-modes '(c-mode c++-mode cuda-mode objc-mode)
      :server-id 'ccls-remote
      :multi-root nil
      :remote? t
      :notification-handlers
      (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
              ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
      :initialization-options (lambda () ccls-initialization-options)
      :library-folders-fn nil)))

(defun yc/c-mode-common-hook ()
  "My hooks to run for c-mode-common-hook."
  (c-setup-doc-comment-style)

  (yc/lsp--setup "ccls" "by your own" (lambda ()
                                           (unless (featurep 'ccls)
                                             (load "ccls"))))
  (yc/run-with-idle-timer
   0.5 nil
   (lambda ()
     (let ((style (yc/get-c-style (buffer-file-name))) )
       (c-set-style style)
       (when (string= style "kernel-coding")
         (add-to-list
          'hide-ifdef-define-alist
          '(kernel __KERNEL__ CONFIG_SMP CONFIG_PCI CONFIG_MMU))
         (hide-ifdef-use-define-alist 'kernel)))

     (when (bound-and-true-p lsp-mode)
       (ccls-code-lens-mode)))))

(use-package cc-mode
  :commands (c++-mode objc-mode c-mode)
  :mode (((rx "." (or "H" "cc" "hh" "moc" "ipp") (? ".in") buffer-end) . c++-mode)
         ((rx "." (or "C" "c" "ic") buffer-end) . c-mode)
         ((rx "." (or "mm" "m") buffer-end) . objc-mode))
  :mode ((rx  "." (or "h" ) (? ".in") buffer-end) . yc/cc-c-c++-objc-mode)
  :bind (:map c-mode-base-map
              ("\C-c\C-h" . yc/switch-h-cpp)
              ( ;(kbd "M-:")
               [134217786] . yc/enable-disable-c-block)
              (;(kbd "C-c s M-d")
               [3 115 134217828] .
               (lambda ()
                 (interactive)
                 (let ((uml/extract-type 'fields))
                   (uml/struct-to-puml (region-beginning) (region-end)))))
              )

  :hook ((c-mode-common . yc/c-mode-common-hook))

  :config
  (require 'smartparens-c)
  (yc/add-safe-local-var
   '(eval c-set-offset
               (quote innamespace)
               0)
   '(eval c-set-offset
               (quote substatement-open)
               0))

  ;; Customized doc-font
  (cdsq tbdoc-font-lock-doc-comments
    (let ((symbol "[a-zA-Z0-9_]+")
          (header "^ \\* "))
      `((,(concat header "\\("     symbol "\\):[ \t]*$")
         1 ,c-doc-markup-face-name prepend nil)
        (,(concat                  symbol     "()")
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat header "\\(" "@" symbol "\\):")
         1 ,c-doc-markup-face-name prepend nil)
        (,(concat "[#%@]" symbol)
         0 ,c-doc-markup-face-name prepend nil)
        (,(concat "\\\\" symbol)
         0 ,c-doc-markup-face-name prepend nil)
        )))

  (cdsq tbdoc-font-lock-doc-protection
    `(("< \\(public\\|private\\|protected\\) >"
       1 ,c-doc-markup-face-name prepend nil)))

  (cdsq tbdoc-font-lock-keywords
    `((,(lambda (limit)
          (c-font-lock-doc-comments "/\\*\\*.*$" limit
            tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!.*" limit
            tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!-+" limit
            tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*!< " limit
            tbdoc-font-lock-doc-comments)
          (c-font-lock-doc-comments "/\\*< " limit
            tbdoc-font-lock-doc-protection)
          (c-font-lock-doc-comments "///.*$" limit
            tbdoc-font-lock-doc-comments)))))

 ;;;; This is a sample, real c-doc-comment-style will be set in "10-emacs-custome.el"
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (c-add-style
   "kernel"
   `("linux"
     (tab-width . 8)
     (indent-tabs-mode . t)
     (c-offsets-alist
      (arglist-cont-nonempty
       c-lineup-gcc-asm-reg
       c-lineup-arglist-tabs-only))))

  (c-add-style "llvm.org"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)))))

  ;; "Based on Google C/C++ Programming Style"
  (c-add-style
   "tubo"
   `("bsd"
     (c-recognize-knr-p . nil)
     (c-basic-offset . 4)
     (tab-width . 4)
     (indent-tabs-mode . nil)
     (comment-column . 40)
     (c-hanging-braces-alist . ((defun-open after)
                                (defun-close before after)
                                (class-open after)
                                (class-close before after)
                                (namespace-open after)
                                (inline-open after)
                                (inline-close before after)
                                (block-open after)
                                (block-close . c-snug-do-while)
                                (extern-lang-open after)
                                (extern-lang-close after)
                                (statement-case-open after)
                                (substatement-open after)))
     (c-hanging-colons-alist . ((case-label)
                                (label after)
                                (access-label after)
                                (member-init-intro before)
                                (inher-intro)))
     (c-hanging-semi&comma-criteria
      . (c-semi&comma-no-newlines-for-oneline-inliners
         c-semi&comma-inside-parenlist
         c-semi&comma-no-newlines-before-nonblanks))
     (c-indent-comments-syntactically-p . nil)
     (c-cleanup-list . (brace-else-brace
                        brace-elseif-brace
                        brace-catch-brace
                        empty-defun-braces
                        defun-close-semi
                        list-close-comma
                        scope-operator))
     (c-offsets-alist . ((func-decl-cont . ++)
                         (member-init-intro . +)
                         (member-init-cont  . c-lineup-multi-inher)
                         (inher-intro . ++)
                         (comment-intro . 0)
                         (arglist-close . c-lineup-arglist)
                         (topmost-intro . 0)
                         (block-open . 0)
                         (inline-open . 0)
                         (substatement-open . 0)
                         (statement-cont
                          . c-lineup-assignments)
                         (label . /)
                         (case-label . 0)
                         (statement-case-open . 0)
                         (statement-case-intro . +) ; case w/o {
                         (access-label . -)
                         (inextern-lang . 0)
                         (innamespace . 0)))
     (c-doc-comment-style . ((c-mode . tbdoc)
                             (c++-mode . tbdoc)
                             (objc-mode . tbdoc)
                             (java-mode . tbdoc)
                             (awk-mode . autodoc)
                             (other . tbdoc)))))

  ;; Coding style for MySql
  (c-add-style
   "mysql"
   '("tubo"
     (c-basic-offset . 2)
     (indent-tabs-mode . nil)
     (c-comment-only-line-offset . 0)
     (tab-width . 2)
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . 0)
                         (substatement-open . 0)
                         (label . -)
                         (statement-cont . +)
                         (arglist-intro . c-lineup-arglist-intro-after-paren)
                         (arglist-close . c-lineup-arglist)
                         (innamespace . 0)
                         (inline-open . 0)
                         (statement-case-open . +)))))

  (c-add-style
   "postgres"
   '("tubo"
     (c-basic-offset . 4)
     (c-auto-align-backslashes . nil)
     (indent-tabs-mode . t)
     (c-comment-only-line-offset . 0)
     (tab-width . 4)
     (fill-column . 78)
     (c-offsets-alist . ((statement-block-intro . +)
                         (knr-argdecl-intro . 0)
                         (substatement-open . 0)
                         (case-label . +)
                         (label . -)
                         (statement-cont . +)
                         (arglist-intro . c-lineup-arglist-intro-after-paren)
                         (arglist-close . c-lineup-arglist)
                         (innamespace . 0)
                         (inline-open . 0)
                         (statement-case-open . 0)))))



  (custom-set-variables
   '(c-doc-comment-style
     (quote ((c-mode . tbdoc)
             (c++-mode . tbdoc)
             (objc-mode . tbdoc)
             (java-mode . javadoc)
             (pike-mode . autodoc)))))

  ;; special keyword for `c++-mode'.
  (font-lock-add-keywords
   'c++-mode
   `((,(rx bow (group "NEW") (+ space)
           (group (+? (or alnum "_"))) eow)
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     (,(rx bow (group
                (or "NEW" "DELETE"
                    "DEC_ALWAYS_INLINE"
                    "ALWAYS_INLINE"
                    "NO_INLINE" "MAY_ALIAS"
                    )) eow)
      (1 font-lock-keyword-face))
     (,(rx bow (group (+ (or upper "_" digit))) (* blank) "(")
      (1 font-lock-builtin-face))
     ))

  (font-lock-add-keywords
   'c-mode
   `((,(rx bow (group (+ (or upper "_" digit))) (* blank) "(")
      (1 font-lock-builtin-face))))



  ;; C++
  (yc/add-compile-unit 'c++ 66
    (progn
      (PDEBUG "ext:" ext)
      (when (or (equal ext "cc")
                (equal ext "cpp"))
        (lambda ()
          (format "%s %s %s -std=gnu++11 -g -o %s"
                  (yc/get-env "CXX" 'executable-find
                              "clang++" "g++" "mingw-g++")
                  (or (getenv "CPPFLAGS")"-Wall  ")
                  file
                  (file-name-sans-extension file))))))

  ;; C
  (yc/add-compile-unit 'c 65
    (when (or (equal ext "c")
              (equal ext "C"))
      (lambda ()
        (interactive)
        (format "%s -o %s %s %s -g -std=gnu99"
                (yc/get-env "CC" 'executable-find
                            "clang" "gcc" "mingw-gcc")
                (file-name-sans-extension file)
                (or (getenv "CPPFLAGS") "-Wall")
                file)))))

(defvar project-ccls-black-list nil
  "List of files to be added into blacklist.
Should be set by .lsp-conf.")

(defun yc/lsp-load-project-configuration-cc-mode (root-file)
  "Advice for 'ccls--suggest-project-root'.
Call FUNC which is 'ccls--suggest-project-root with ARGS."
  (PDEBUG "ENTER: root-" root-file)
  (let* ((blacklist '("/.ccls-cache/")))

    (dolist (item project-ccls-black-list)
      (push item blacklist))

    ;;  Use compile database file which is newer...
    (setq ccls-initialization-options nil)

    (when root-file
      (let ((root-dir (file-name-directory root-file)) )
        (setq ccls-args
              (list
               (format "--log-file=%s" (yc/lsp-get-log-file "ccls" root-dir))
               "-v=2"))
        ;; guessing compliation database....
        (PDEBUG "Before advice" ccls-initialization-options)

        (unless (member :compilationDatabaseDirectory ccls-initialization-options)
          (let (compile-dir last-mod-time)
            (dolist (dir (append
                          '("." )
                          (directory-files root-dir nil (rx(? "cmake_") "build" ))))
              (unless (string= "." dir)
                (push dir blacklist))

              (let* ((file (format "%s/%s/compile_commands.json" root-dir dir))
                     (mod-time (if (file-exists-p file)
                                   (file-attribute-modification-time
                                    (file-attributes file)))))

                (when (and mod-time
                         (or (not compile-dir) ;; not set..
                             (time-less-p last-mod-time mod-time) ;; file is newer
                             ))
                  (PDEBUG (format "Using newer database %s, generated at: %s"
                                  file (format-time-string "%D %T" mod-time)))
                  (setq compile-dir dir
                        last-mod-time mod-time))))

            (when compile-dir
              (push compile-dir ccls-initialization-options)
              (push :compilationDatabaseDirectory ccls-initialization-options))))

        (unless (member :index ccls-initialization-options)
          (push (list :threads (max (/ (yc/get-cpu-cores) 4) 2)
                      :blacklist (vconcat blacklist nil)
                      :whitelist (vconcat '("quelpa")))
                ccls-initialization-options)
          (push :index ccls-initialization-options))

        (PDEBUG "After advice" ccls-initialization-options)))
    (PDEBUG "leave")))

(defalias 'yc/lsp-load-project-configuration-c-mode
  'yc/lsp-load-project-configuration-cc-mode)

(defalias 'yc/lsp-load-project-configuration-c++-mode
  'yc/lsp-load-project-configuration-cc-mode)

(use-package modern-cpp-font-lock
  :hook ((c++-mode . modern-c++-font-lock-mode))
  :config
  (push "assert_cast" modern-c++-keywords))

(use-package member-function
  :commands (expand-member-functions)
  :hook ((c++-mode . (lambda () (local-set-key "\C-cm" #'expand-member-functions))))
  :custom
  (mf--insert-commentary nil))

(use-package prog-utils
  :commands (
             yc/asm-post-process
             uniq-stack gcrash-analyze-buffer))


(provide '051-prog-c)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 051-prog-c.el ends here
