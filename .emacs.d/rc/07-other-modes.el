;;; 07-other-modes.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

(use-package highlight-parentheses
  :ensure t
  :commands (highlight-parentheses-mode)
  :hook ((prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors
        '("red" "yellow" "cyan" "magenta" "green" "firebrick1" "IndianRed4")))

(use-package highlight-indentation
  :commands (highlight-indentation-mode))

(use-package ace-link
  :preface
  (defadvice! yc/ace-link--eww-action-adv (orig-func pt new-buffer)
    "If called with prefix, open in new buffer, instead of external browser.
ORIG-FUNC is called with ARGS."
    :around  #'ace-link--eww-action
    (when (number-or-marker-p pt)
      (goto-char pt)
      (if (or new-buffer current-prefix-arg)
          (eww-open-in-new-buffer)
        (eww-follow-link))))

  (defadvice! yc/ace-link-adv (orig-func &rest args)
    "Browse with eww.
ORIG-FUNC is called with ARGS."
    :around  #'ace-link
    (let ((browse-url-browser-function 'eww-browse-url))
      (apply orig-func args)))

  :hook ((emacs-startup . ace-link-setup-default))
  :bind (("M-o" . ace-link)))

(use-package info
  :commands (info)
  :config
  (let* ((r-match-info (rx (group (+ ascii)) ".info" (* ascii)))
         res)
    ;; initialize Info-directory-list if needed.
    (unless Info-directory-list
      (info-initialize))

    ;; add Info-default-directory-list at tail.
    (mapcar (lambda (x)
              (add-to-list 'Info-directory-list x t))
            Info-default-directory-list)))

(use-package counsel-info
  :commands (counsel/info)
  :bind (([remap info] . counsel/info))
  )

(use-package ispell
  :bind (([M-S-f11] . ispell-buffer)
         ([S-f11]   . ispell-word))

  :config
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name (executable-find "aspell"))
  :custom
   (ispell-extra-args '("--sug-mode=ultra" "--run-together" "--dont-tex-check-comments"))
   (ispell-skip-html t)
   (ispell-dictionary "english")
   (ispell-local-dictionary "english")
  )

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook! '(org-mode-hook
               markdown-mode-hook
               TeX-mode-hook
               rst-mode-hook
               mu4e-compose-mode-hook
               message-mode-hook
               log-edit-mode-hook
               git-commit-mode-hook)
             #'flyspell-mode)
  (add-hook! '(yaml-mode-hook
               conf-mode-hook
               prog-mode-hook)
             #'flyspell-prog-mode)
  :custom
  (flyspell-issue-welcome-flag nil)
  ;; Significantly speeds up flyspell, which would otherwise print
  ;; messages for every word when checking the entire buffer
  (flyspell-issue-message-flag nil)
  :config
  (substitute-key-definition
   'flyspell-goto-next-error  'backward-page flyspell-mode-map)
  (substitute-key-definition
   'flyspell-auto-correct-word 'forward-page flyspell-mode-map))

(use-package tdict
  :bind (("<C-f10>" . tdict-search)
         ("<S-f10>" . ace-tdict-search)))

(use-package tabbr
  :bind (("<S-f11>" . tabbr-search)
         ("<C-S-f11>" . tabbr-edit)))

(use-package image-mode
  :bind (:map image-mode-map
              ("C-c o" . yc/open-with-external-app)))

(use-package ediff
  :preface
  (defvar yc/ediff-bufferes-to-restore nil
    "List of buffers whose flycheck-mode is disabled temporarily.")

  (defun yc/ediff-prepare (&rest args)
    "Prepare BUFFERS for Ediff."
    (let ((buffers (-flatten (-filter #'bufferp args))))
      (PDEBUG "YC/EDIFF-PREPARE: " buffers)
      (dolist (buffer buffers)
        (PDEBUG "BUF:" buffer)
        (with-current-buffer buffer
          (ws-butler-mode -1)
          (if (bound-and-true-p flycheck-mode)
              (push (current-buffer) yc/ediff-bufferes-to-restore))

          (if (fboundp 'show-ifdefs)
              (show-ifdefs))

          (PDEBUG "YC/EDIFF-PREPARE END.")))))


  (defun yc/ediff-copy-file-name-A ()
    "Description."
    (interactive)
    (yc/ediff-copy-file-name 'A))

  (defun yc/ediff-copy-file-name (id &optional absolute)
    "Description."
    (interactive)
    (let* ((buf
            (cond
             ((eq id 'A) ediff-buffer-A)
             ((eq id 'B) ediff-buffer-B)
             (t (error "OOPS"))))
           (name (buffer-file-name buf)))

      (kill-new     (if absolute
                        name
                      (file-name-base name)))))

  (defun ediff-copy-both-to-C ()
    "Copy both regions into C."
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (defun yc/ediff-region ()
    "Select a region to compare"
    (interactive)

    (when (use-region-p)  ; there is a region
        (let (buf)
        (setq buf (get-buffer-create "*Diff-regionA*"))
        (save-current-buffer
          (set-buffer buf)
          (erase-buffer))
        (append-to-buffer buf (region-beginning) (region-end)))
      )

    (deactivate-mark)
    (message "Now select other region to compare and run `diff-region-now`"))

  (defun yc/ediff-region-now ()
    "Compare current region with region already selected by `diff-region`"
    (interactive)
    (when (use-region-p)
      (let (bufa bufb)
        (setq bufa (get-buffer-create "*Diff-regionA*"))
        (setq bufb (get-buffer-create "*Diff-regionB*"))
        (save-current-buffer
          (set-buffer bufb)
          (erase-buffer))
        (append-to-buffer bufb (region-beginning) (region-end))
        (deactivate-mark)
        (ediff-buffers bufa bufb))))

  :commands (ediff-files)
  :bind (("<f12>" . ediff-buffers)
         ("M-<f12>" . yc/ediff-region)
         ("M-S-<f12>" . yc/ediff-region-now))

  :custom
  (ediff-diff-options "") ;; turn off whitespace checking
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-ignore-similar-regions nil)

  :config
  (advice-add 'ediff-prepare :before #'yc/ediff-prepare)

  (defadvice! yc/ediff-quit-adv (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :before  #'ediff-quit
    (dolist (buffer yc/ediff-bufferes-to-restore)
      (with-current-buffer buffer
        (flycheck-mode 1)
        (PDEBUG "BUF: " buffer
                "Need-Save:" (and buffer-file-name
                                  (buffer-modified-p)))
        (when (and buffer-file-name
                   (buffer-modified-p))
          (save-buffer))))

    (setq yc/ediff-bufferes-to-restore nil))


  (setq ediff-diff-ok-lines-regexp
        (concat
         "^\\("
         "[0-9,]+[acd][0-9,]+\C-m?$"
         "\\|[<>] "
         "\\|---"
         "\\|.*Warning *:"
         "\\|.*No +newline"
         "\\|.*missing +newline"
         "\\|.*文件尾没有 newline 字符"
         "\\|^\C-m?$"
         "\\)"))

  :hook
  ((ediff-keymap-setup
    .
    (lambda ()
      (define-key 'ediff-mode-map "d" 'ediff-copy-both-to-C)
      (define-key 'ediff-mode-map "ff" 'yc/ediff-copy-file-name-A)))

   ;; (ediff-startup . yc/ediff-startup)
   ;; (ediff-quit . yc/ediff-cleanup)
   ))

(use-package smerge-mode
  :bind (("<S-f12>" . smerge-ediff))
  :config
    (advice-add 'smerge-ediff :before #'yc/ediff-prepare))

(use-package yc-utils
  :commands (
             edit-rcs edit-template edit-peda edit-sway
             reload-file reload-all-files edit-emacs debug-on
             uniq-region
             xmind/convert-to-org
             yc/add-subdirs-to-load-path
             yc/copy-file-name yc/reload-emacs
             yc/decode-hex-color
             yc/encode-hex-color
             yc/http-save-page
             yc/insert-current-date
             yc/insert-current-date-time
             yc/kill-proc yc/move-snapshot  yc/expand-macro
             yc/pack-env yc/unpack-env dos-unix unix-dos
             yc/syntax-color-hex
             yc/choose-compiler
             yc/show-compilers
             yc/update-env-from
             yc/view-with-ediff
             yc/get-cpu-cores
             yc/get-compiling-threads
             yc/get-env
             yc/uef
             yc/touch-file
             yc/git-copy-file-path
             yc/new-snippet
             yc/dired-compress-file
             yc/list-attentions
             yc/kill-ghost-buffers
             )

  :bind ((;(kbd "C-x J")
          "J" . yc/eval-and-insert-comment)
         (;(kbd "C-x j")
          "j" . yc/eval-and-insert)
         ("C-x M-e" . yc/eval-and-kill)
         ("C-x M-k" . yc/kill-ghost-buffers)

         ("C-o" . zl-newline)
         ("C-S-o" . zl-newline-up)
         ("" . zl-newline-up)

         ([mouse-4] . down-slightly)
         ([mouse-5] . up-slightly)

         ("<C-return>" . yc/insert-line-number)
         (;(kbd "<M-K>")
          [M-K] . kill-current-buffer)

         ("<M-f11>" . yc/fill-region)

         ("\C-c>" . shift-region-right)
         ( "\C-c<" . shift-region-left)
         ( "\M-;" . yc/comment-dwim-line)

         ( ;(kbd "C-+")
          [67108907] . increase-font-size)
         ( ;(kbd "C--")
          [67108909] . decrease-font-size)

         (;(kbd "C-h V")
          "V" . yc/debug-variable)

         (;(kbd "M-W")
          [134217815] . yc/kill-file-ln)

         ("C-x ^" . yc/enlarge-window)
         ("C-x v" . yc/shrink-window)

         (;;(kbd "C-x >")
          ">" . yc/enlarge-window-horizontal)
         ("C-x <" . yc/shrink-window-horizontal)

         ([f5] . yc/open-eshell)
         ([f2] . auto-rename-buffer)
         ("C-<f2>" . rename-buffer)
         ([remap shell-command] . yc/exec-command-via-eshell))

  :hook ((before-save . yc/make-file-writable)))

(use-package yc-dump
  :commands (yc/dump-emacs yc/config-emacs))

(use-package diff-mode
  :mode (rx (or ".rej"  "patch") eol)
  :bind (:map diff-mode-map
              ("C-c C-v" . yc/view-with-ediff)
              ("C-c M-v" . (lambda ()
                                 (interactive)
                                 (yc/view-with-ediff nil t)))
              ("C-c C-f" . (lambda ()
                          (interactive)
                          (kill-new (file-name-nondirectory buffer-file-name)))))
  :hook ((diff-mode . (lambda ()
                        (which-function-mode -1)))))

(use-package highlight-symbol
  :bind (([(control f3)]  . highlight-symbol-at-point)
         ( ;(kbd "M-[ 1 3 ^")
          [134217819 49 51 94] . highlight-symbol-at-point)

         ([f3]  . highlight-symbol-next)
         ([(shift f3)]  . highlight-symbol-remove-all)
         ([(meta f3)]  . highlight-symbol-prev)
         ([(control meta f3)]  . highlight-symbol-query-replace)))


(use-package irfc
  :commands (irfc-visit irfc-follow)
  :config
  (custom-set-variables
   '(irfc-download-base-url "http://www.rfc-editor.org/rfc/")
   '(irfc-directory  "~/Documents/TechBooks/RFCs/")
   )
  :mode ("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . irfc-mode))

(use-package tramp
  :custom
  (tramp-default-method
     (cond
      ((and IS-WINDOWS  (executable-find "pscp")) "pscp")
      ((and IS-WINDOWS  (executable-find "plink")) "plink")
      ;; There is an ssh installation.
      ((executable-find "scp") "scp")
      ;; Fallback.
      (t "ftp")))
  (tramp-completion-without-shell-p t)
  (tramp-auto-save-directory "~/.emacs.d/auto-save-list")
  (tramp-remote-path '("~/.local/bin/"
                       "/usr/local/bin"
                       "/usr/local/sbin"
                       "/opt/bin" "/opt/sbin" "/opt/local/bin"
                       "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                       ))


  :config
  (tramp-set-completion-function "ssh"
                                   '((tramp-parse-sconfig "/etc/ssh_config")
                                     (tramp-parse-sconfig "~/.ssh/config")))
  (tramp-set-completion-function "scp"
                                   '((tramp-parse-sconfig "/etc/ssh_config")
                                     (tramp-parse-sconfig "~/.ssh/config"))))

(use-package browse-url
  :commands (browse-url-generic)
  :custom
  (browse-url-generic-program (cond (IS-MAC
                                     "/usr/bin/open")
                                    (IS-LINUX
                                     (or
                                      (executable-find "google-chrome-stable")
                                      (executable-find "google-chrome")
                                      (executable-find "google-chrome-beta")
                                      (executable-find "firefox")
                                      (executable-find "firefox-bin")
                                      "/usr/bin/xdg-open"))
                                    (t nil)))
  )

(use-package shrface
  :commands (shrface-basic shrface-trial)
  :config
  :custom
  (shrface-href-versatile t)
  (shrface-toggle-bullets nil)
  :hook ((eww-after-render . shrface-mode)
         (nov-mode . shrface-mode)
         (mu4e-view-mode . shrface-mode)
         (elfeed-show-mode  . shrface-mode)))

(use-package shr
  :preface
  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let* ((class (dom-attr dom 'class))
             (mode (cond
                    ((or (not class) (s-contains? "src-text" class)) nil)
                    (t (eww-buffer-auto-detect-mode)))))

        (PDEBUG "CLASS:" class
                "MODE:" mode)
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  (defun eww-buffer-auto-detect-mode ()
    (unless (featurep 'language-detection)
      (require 'language-detection))
    (let* ((map '((ada ada-mode)
                  (awk awk-mode)
                  (c c-mode)
                  (cpp c++-mode)
                  (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode)
                  (css css-mode)
                  (dart dart-mode)
                  (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode)
                  (erlang erlang-mode)
                  (fortran fortran-mode)
                  (fsharp fsharp-mode)
                  (go go-mode)
                  (groovy groovy-mode)
                  (haskell haskell-mode)
                  (html html-mode)
                  (java java-mode)
                  (javascript javascript-mode)
                  (json json-mode javascript-mode)
                  (latex latex-mode)
                  (lisp lisp-mode)
                  (lua lua-mode)
                  (matlab matlab-mode octave-mode)
                  (objc objc-mode c-mode)
                  (perl perl-mode)
                  (php php-mode)
                  (prolog prolog-mode)
                  (python python-mode)
                  (r r-mode)
                  (ruby ruby-mode)
                  (rust rust-mode)
                  (scala scala-mode)
                  (shell shell-script-mode)
                  (smalltalk smalltalk-mode)
                  (sql sql-mode)
                  (swift swift-mode)
                  (visualbasic visual-basic-mode)
                  (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))

      (PDEBUG "Detected language:"language)
      (when (fboundp mode)
        mode)))

  (setq shr-external-rendering-functions
        '((pre . eww-tag-pre)))

  (defadvice! yc/shr-colorize-region-adv (orig-func &rest args)
    "Don't render region with color.
ORIG-FUNC is called with ARGS."
    :around  #'shr-colorize-region)

  :custom
  (shr-use-fonts nil)
  (shr-use-colors t)
  (shr-blocked-images
   (rx
    (or (: "https://www.postgresql.org"
           (or
            "/media/img/about/press/elephant.png"
            "/media/img/atpostgresql.png"
            "/media/img/git.png")))))
  :config
  (shrface-basic)
  (shrface-trial))

(use-package eww
  :defer t
  :preface

  (defun yc/ace-link-eww-new-buffer ()
    "Use ace-link, but open in new buffer."
    (interactive)
    (let ((pt (avy-with ace-link-eww
                (avy-process
                 (mapcar #'cdr (ace-link--eww-collect))
                 (avy--style-fn avy-style)))))
      (ace-link--eww-action pt t)))

  (defvar yc/eww-layout-cfg nil "Previous buffer before eww is called.")

  (defadvice! yc/quit-window-adv (orig-func &rest args)
    "Kill buffer on quit-window.
ORIG-FUNC is called with ARGS."
    :around  #'quit-window
    (let ((mode major-mode))

      (PDEBUG "CURRENT:" (current-buffer) major-mode
              "EWW:" (eq mode 'eww-mode)
              "BUF:" (car yc/eww-layout-cfg))

      (if (member mode '(eww-mode Man-mode woman-mode Info-mode))
          (progn
            (kill-buffer (current-buffer))

            (when (and (eq mode 'eww-mode)
                       yc/eww-layout-cfg)
              (PDEBUG "Restory: " (car yc/eww-layout-cfg))
              (layout/restore-wincfg (pop yc/eww-layout-cfg))))
        (apply orig-func args))))

  :hook ((eww-mode . yc/disable-trailling-spaces))

  :bind (:map eww-mode-map
              ("C-c o" . eww-browse-with-external-browser)
              ("<M-left>" . eww-back-url)
              ("<M-right>" . eww-forward-url)
              ("<M-O>" . yc/ace-link-eww-new-buffer)
              ;; URL copy: bind to "w"
              )
  :custom
  (eww-search-prefix "https://www.google.com/search?q=")

  :config
  (defadvice! yc/eww--dwim-expand-url-adv (url)
    "Docs
ORIG-FUNC is called with ARGS."
    :override  #'eww--dwim-expand-url
    (setq url (string-trim url))
    (cond ((string-match-p "\\`file:/" url))
	  ;; Don't mangle file: URLs at all.
          ((string-match-p "\\`ftp://" url)
           (user-error "FTP is not supported"))
          (t
	   ;; Anything that starts with something that vaguely looks
	   ;; like a protocol designator is interpreted as a full URL.
           (if (or (string-match "\\`[A-Za-z]+:/" url)
		   ;; Also try to match "naked" URLs like
		   ;; en.wikipedia.org/wiki/Free software
		   (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
		   (and (= (length (split-string url)) 1)
		        (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
			         (> (length (split-string url "[.:]")) 1))
			    (string-match eww-local-regex url))))
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/")))
             (setq url (concat eww-search-prefix
                               (mapconcat
                                #'url-hexify-string (split-string url) "+"))))))
    url)


  (defadvice! yc/eww-open-in-new-buffer-adv (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :before  '(eww-open-in-new-buffer eww-follow-link)

    (let* ((layoutcfg (layout/capture-wincfg))
           (curbuf (car layoutcfg)))
      (PDEBUG "ENTER")
      (dolist (locfg yc/eww-layout-cfg)
        (if (eq curbuf (car locfg))
            (setq yc/eww-layout-cfg
                  (delq locfg yc/eww-layout-cfg))))
      (push layoutcfg yc/eww-layout-cfg))))

(use-package my-net-utils
  :commands (yc/download-url yc/open-url)
  :bind (("C-x C-d" . yc/download-url)
         ("C-x C-o" . yc/open-url)))

(use-package nov
  :mode ((rx ".epub") . nov-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :preface
  (defun yc/pdf-tools-re-install ()
    "Re-install `epdfinfo' even if it is installed.
The re-installation is forced by deleting the existing `epdfinfo'
binary.
Useful to run after `pdf-tools' updates."
    (interactive)
    (unless (featurep 'pdf-tools)
      (require 'pdf-tools))
    (when (pdf-info-running-p)
      (pdf-info-kill))
    (delete-file pdf-info-epdfinfo-program)
    (pdf-tools-install :no-query-p))

  (defvar-local yc/pdf-scaled-pages nil "List of scaled pages.")

  :commands (pdf-tools-install pdf-tools-enable-minor-modes)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook ((pdf-view-mode . pdf-tools-enable-minor-modes))
  :custom
  (pdf-info-epdfinfo-program (expand-file-name "~/.local/bin/epdfinfo"))
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)

  :bind (:map pdf-view-mode-map
              ("l" . pdf-history-backward)
              ("r" . pdf-history-forward)
              ("i" . tnote))
  :config
  (unless (file-executable-p pdf-info-epdfinfo-program)
    (message "Tool %s does not exist, compiling ..." pdf-info-epdfinfo-program)
    (pdf-tools-install))
  )

(use-package stringtemplate-mode
  :mode "\\.st\\'")

(use-package eshell
  :commands (eshell-command)
  :bind (("<C-f5>" . eshell))
  :hook ((eshell-mode . (lambda ()
                          (setq eshell-path-env (getenv "PATH")))))
  :init
  (progn
    (custom-set-variables
     '(eshell-buffer-shorthand t)
     '(eshell-directory-name (yc/make-cache-path "eshell"))
     '(eshell-aliases-file (expand-file-name "~/.emacs.d/eshell_alias"))
     '(eshell-buffer-maximum-lines 20000)
     '(eshell-history-size 350)
     '(eshell-hist-ignoredups t)
     '(eshell-buffer-shorthand t)
     '(eshell-plain-echo-behavior t)))
  :config
  (require 'esh-opt)

  ;; quick commands
  (defalias 'eshell/e 'find-file-other-window)
  (defalias 'eshell/d 'dired)
  (setenv "PAGER" "cat")

  ;; automatically truncate buffer after output
  (when (boundp 'eshell-output-filter-functions)
    (push 'eshell-truncate-buffer eshell-output-filter-functions))

  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)


  (yc/set-company-backends 'eshell-mode     'company-files 'company-dabbrev))

(use-package eshell+
  :commands (eshell/ldd eshell/restart_pg))

(use-package em-term
  :config
  (mapc (lambda (x) (push x eshell-visual-commands))
        '("el" "elinks" "htop" "less" "ssh" "tmux" "top" "vim" "tail"
          "spark-shell" "sbt" "watch")))

(use-package vterm
  :preface
  (defun yc/vterm-module-compile ()
    "Compile vterm module in quelpa-build directory, to avoid recompile it frequently."
    (interactive)
    (unless (executable-find "cmake")
      (error "Vterm needs CMake to be compiled.  Please, install CMake"))

    (let* ((build-dir (file-name-as-directory (expand-file-name "vterm" quelpa-build-dir)))
           (vterm-module (concat build-dir "vterm-module.so")))

      (when (or current-prefix-arg ;; if called with prefix-arg, recompile.
                (not (file-exists-p vterm-module)))
        (with-current-buffer (get-buffer-create "*Compile-Vterm*")
          (pop-to-buffer (current-buffer))
          (read-only-mode -1)
          (erase-buffer)
          (let ((ret (call-process "sh" nil (current-buffer) t "-c"
                                   (concat
                                    "cd " (shell-quote-argument build-dir) "; \
                rm -rf build; \
                cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
                -DCMAKE_BUILD_TYPE=Release -S . -B build; \
                make -C build;"))))

            (compilation-mode)
            (if (zerop ret)
                (message "Compilation of `emacs-libvterm' module succeeded")
              (error "Compilation of `emacs-libvterm' module failed!")))))
      (load-file vterm-module)))

  (defun yc/open-vterm (&optional name)
    "Open or switch to vterm.
If *vterm* buffer not exist, or called with current-prefix-arg,
create new buffer."
    (interactive)

    (unless (featurep 'vterm-module)
      (yc/vterm-module-compile))

    (unless (featurep 'vterm)
      (require 'vterm))

    (unless name
      (setq name vterm-buffer-name))

    (let* ((vbuffer (if current-prefix-arg
                        (progn
                          (with-current-buffer
                              (generate-new-buffer
                               name)
                            (vterm-mode)
                            (current-buffer)))
                      (get-buffer name)))
           (dir default-directory))

      (unless vbuffer
        (with-current-buffer (setq vbuffer (get-buffer-create name))
          (vterm-mode)))

      (pop-to-buffer-same-window vbuffer)

      (unless (string= dir default-directory)
        (vterm-send-string
         (concat "cd " dir))
        (vterm-send-return))
      vbuffer))

  :commands (vterm vterm-mode)
  :quelpa (vterm :fetcher github :repo "akermu/emacs-libvterm")
  :init
  (push (expand-file-name "vterm" quelpa-build-dir) load-path)
  :bind (([S-f5] . yc/open-vterm))
  :custom
  (vterm-kill-buffer-on-exit t)
  :config
  (defadvice! yc/vterm-module-compile-adv (&rest args)
    "Compile vterm module in quelpa-build directory, to avoid recompile it frequently."
    :override  #'vterm-module-compile
    (yc/vterm-module-compile))

  (add-to-list 'vterm-eval-cmds '("update-pwd"
                                  (lambda (path)
                                    (setq default-directory path)))))

(use-package comint
  :defer t
  :init
  (progn
    (add-hook 'comint-mode-hook
      (lambda ()
        (make-local-variable 'jit-lock-defer-timer)
        (set (make-local-variable 'jit-lock-defer-time) 0.25)))

    (custom-set-variables
     '(comint-buffer-maximum-size 32768))
    (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
    (add-hook 'comint-preoutput-filter-functions
      (lambda (text)
        (interactive)
        (let* ((regexp "\\(.\\{256\\}[;,: ]\\)")
               (shortened-text (replace-regexp-in-string regexp "\\1\n" text)))
          (if (string= shortened-text text)
              text
            shortened-text))))))

(use-package tabify
  :bind (("\C-xt" . untabify)
         ("\C-xT" . tabify)))

(use-package find-func
  :bind (("\C-cff"  . find-function)
         ( "\C-cfc" . find-function-on-key))
  :config
  (defadvice! yc/find-function-adv (&rest args)
    "Save location before `find-function' with ARGS."
    :before  #'find-function
    (condition-case error
        (yc/push-stack)
      ('error nil))))

(use-package nhexl-mode
  :commands (nhexl-mode nhexl-hex-edit-mode))

(use-package hexview-mode :bind (("C-x M-F" . hexview-find-file)))

(use-package graphviz-dot-mode
  :preface
  (defun yc/graphviz-dot-preview ()
    "Preview current buffer."
    (interactive)
    (unless (executable-find "dot")
      (error "Graphviz is required but not found"))

    (let* ((tempname (make-temp-file "dot-"))
           (output (concat tempname ".png"))
           (ob-name "*dot-preview*")
           (ob (get-buffer ob-name)))
      (write-region (point-min) (point-max) tempname)

      (when ob
        (kill-buffer ob))

      (setq ob (get-buffer-create ob-name))
      (set-buffer ob)
      (erase-buffer)
      (condition-case var
          (if (= (call-process "dot" nil ob nil "-Tpng" tempname "-o" output) 0)
              (progn
                (erase-buffer)
                (insert-file-contents output)
                (image-mode)
                (set-buffer-multibyte t)
                (display-buffer ob))

            (progn
              (message "Failed to compile: %s" (buffer-string))))

        (error (message "error: %s" var)))

      (dolist (fn (list tempname output))
        (when (file-exists-p fn)
          (delete-file fn)))))


  (defun yc/graphviz-dot-view-external ()
    "View with external tool."
    (interactive)
    (let ((fn (concat (file-name-sans-extension buffer-file-name)
                      "." graphviz-dot-preview-extension)))
      (unless (file-exists-p fn)
        (error "File not compiled??"))
      (yc/open-with-external-app fn)))
  :defer t
  :commands (graphviz-dot-mode graphviz-compile-command)
  :bind (:map graphviz-dot-mode-map
              ("C-c C-c" . yc/graphviz-dot-preview)
              ("C-c C-o" . yc/graphviz-dot-view-external))
  :custom
  (graphviz-dot-indent-width 4)
  :config
  (progn
    (yc/add-compile-unit 'dot 40
      (when (equal ext "dot")
        (lambda ()
          (graphviz-compile-command (buffer-file-name)))))))

(defalias 'dot-mode 'graphviz-dot-mode)
(defalias 'org-babel-execute:graphviz-dot 'org-babel-execute:dot)

(use-package dired
  :commands (dired)
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-verbosity '(uid gid))

  (dired-auto-revert-buffer t)  ; don't prompt to revert; just do it
  (dired-dnd-protocol-alist nil)
  (dired-dwim-target t)   ; suggest a target for moving/copying intelligently
  (dired-hide-details-hide-symlink-targets nil)
  ;; Always copy/delete recursively
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alh")
  :hook ((dired-mode . (lambda () (setq fill-column 9999))))
  :bind (:map dired-mode-map
              ("M-p" . dired-up-directory)
              ("<C-return>" . dired-find-file-other-window)
              ("<M-return>" . yc/open-with-external-app))
  :bind (("C-x C-j" . dired-jump))
  :config
  (defadvice! yc/dired-compress-adv (&rest args)
    "Compress file with 7z if possible.
ORIG-FUNC is called with ARGS."
    :before-until #'dired-compress
    (zerop (call-process "unpackfile" nil t nil (dired-get-filename))))
  (defadvice! yc/dired-find-file-adv (&rest args)
    "If file already opened, switch to that buffer without confirm.
ORIG-FUNC is called with ARGS."
    :before #'dired-find-file
    (awhen (find-buffer-visiting (dired-get-file-for-visit))
      (switch-to-buffer it)))

  (load-library "ls-lisp")
  (define-key ctl-x-map "d" nil))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  :bind (:map dired-mode-map
              ([f12] . dired-diff)))

(use-package wdired
  :after dired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package dired-x
  :config
  (add-to-list 'auto-mode-alist (cons "[^/]\\.dired$"
                                      'dired-virtual-mode)))

(use-package hl-line
  :hook ((bookmark-bmenu-mode . hl-line-mode)
         (ibuffer-mode . hl-line-mode)
         (grep-setup-mode . hl-line-mode)
         (dired-mode . hl-line-mode)))

;; vimrc-mode
(use-package vimrc-mode
  :mode (rx (or ".vim" (: "." (? "_") (? "g")  "vimrc") ".exrc")))

(use-package ztree
  :commands (ztree-dir ztree-diff))

(use-package charset-util :commands (yc/list-non-ascii))

(use-package image-file
  :commands (auto-image-file-mode)
  :init
  (when window-system
    (auto-image-file-mode 1))) ; 自动加载图像

(use-package t-report  :commands (yc/new-wp yc/new-mail))

(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region)))

(use-package vimish-fold
  :preface
  (defun yc/vimish-fold-toggle (beg end)
    "Description."
    (interactive "r")
    (unless (featurep 'vimish-fold)
      (load "vimish-fold"))
    (if mark-active
        (progn
          (dolist (overlay (overlays-in beg end))
            (when (vimish-fold--vimish-overlay-p overlay)
              (vimish-fold--delete overlay)))
          (vimish-fold beg end))
      (vimish-fold-toggle)))
  :commands (vimish-fold vimish-fold-toggle vimish-fold-delete)
  :bind (("C-c hr" . yc/vimish-fold-toggle)))

(use-package mediawiki
  :commands (mediawiki-open mediawiki-site))

(use-package desktop
  :commands (desktop-save-in-desktop-dir desktop-read)
  :config
  (progn
    (setq desktop-path (list (yc/make-cache-path "desktop" t))
          desktop-dirname (yc/make-cache-path "desktop" t))))

(use-package sgml-mode
  :preface
  (defun yc/html-mode-hook ()
    "My hook for html mode."
    (html-autoview-mode -1)
    (remove-hook 'after-save-hook 'browse-url-of-buffer t)
    (flyspell-mode 1))
  :mode ("/itsalltext/" . html-mode)
  :commands (html-autoview-mode)
  :hook ((html-mode . yc/html-mode-hook)))

(use-package css-mode
  :defer t
  :mode (rx "." (or "scss" "css" "rasi") eow)
  :config
  (yc/add-company-backends 'css-mode 'company-css)
  :custom
  (css-indent-offset 2)
)

(use-package nxml-mode
  :mode (rx "." (or "xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "rdf" "plist") eol)
  :custom
  (nxml-attribute-indent 2)
  (nxml-child-indent 2)
  (nxml-outline-child-indent 2)
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-bind-meta-tab-to-complete-flag t)
  (nxml-slash-auto-complete-flag t)
  :hook ((nxml-mode .
                    (lambda ()
                      (local-set-key (kbd "C-c /") 'nxml-finish-element)
                      (auto-fill-mode)
                      (rng-validate-mode)
                      (hs-minor-mode 1)
                      )))
  :config
  (yc/add-company-backends 'nxml-mode 'company-nxml))

(use-package text-mode
  :preface
  (defun yc/txt-to-png ()
    "Change a txt file into png file using ditaa."
    (interactive)
    (unless (executable-find "java")
      (error "Function `txt-to-png' requires java"))

    (let* ((infile (buffer-file-name))
           (txt2png-buf-name "*txt2png*")
           (cmd (list "java" "-jar" (yc/ditaa-path t) infile "--overwrite")))
      (get-buffer-create txt2png-buf-name)
      (pop-to-buffer txt2png-buf-name)
      (erase-buffer)
      (insert "\nInvoking command: "
              (s-join " " cmd))
      (set-process-sentinel
       (apply #'start-process (append (list "txt-to-png" txt2png-buf-name) cmd))
       (lambda (process state)
         (when (and (string-match "finished" state)
                    (yes-or-no-p "Open generated file?"))

           (save-excursion
             (goto-char (point-min))
             (unless (search-forward-regexp
                      (rx "Rendering to file:" (* space) (group (+ nonl))) nil t)
               (error "Can't find output file")))

           (let ((outfile (match-string 1)))
             (unless (file-exists-p outfile)
               (error "File %s does not exist" outfile))
             (kill-current-buffer)
             (find-file outfile)))))
      (message "This may take for a while, refer to *txt2png* to check progress...")))
  :bind (:map text-mode-map
              ("\C-c\C-e" . yc/txt-to-png))
  :config
  (sp-with-modes 'text-mode
    (sp-local-pair "```" "```"))
  :init
  (defalias 'txt-mode 'text-mode))

(use-package artist
  :bind (:map artist-mode-map
              ("\C-c\C-e" . yc/txt-to-png))
  :bind (("<C-S-f2>" . artist-mode)))

(use-package add-log
  :bind (:map change-log-mode-map
              (;;(kbd "<C-return>")
               [C-return] . add-change-log-entry-other-window)))

(use-package org-table
  :commands (orgtbl-mode))

(use-package markdown-mode
  :preface
  (defun yc/translate-markdown-filename (in)
    "Translate IN into filename.."
    (let ((out   (catch 'p-found
                   (dolist (ext '("" ".org" ".md"))
                     (aif (yc/file-exists-p (concat in ext))
                         (throw 'p-found it))
                     )
                   (concat in ".md"))))

      (PDEBUG "IN: " in
              "OUT: " out)
      out))


  :commands (markdown-mode markdown-follow-link-at-point)
  :hook ((markdown-mode . orgtbl-mode)
         (before-save . (lambda ()
                          (when (eq major-mode 'markdown-mode)
                            (save-excursion
                              (goto-char (point-min))
                              (while (search-forward "-+-" nil t)
                                (replace-match "-|-")))))))
  :custom
  (markdown-translate-filename-function 'yc/translate-markdown-filename)
  ;; (markdown-command "markdown_py")
  (markdown-xhtml-header-content
   "<style type=\"text/css\">html { margin: 0; font: .9em/1.6em \"Droid Serif\", \"Lucida Grande\", \"Lucida Sans Unicode\", \"DejaVu Sans\", Verdana, sans-serif; background-attachment: fixed; background-position: right bottom; background-repeat: no-repeat; background-color: white; }  body { font-size: 12pt; line-height: 18pt; color: black; margin-top: 0; }   pre { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace; font-size: 90%; padding: 1.2em; overflow: auto;  line-height: 1.3; font-weight: 100; background-color:#2e3436; box-shadow: 5px 5px 5px #888; border: none; margin-bottom: 10pt; color: white; padding: 1.2em; }  code { font-family: Droid Sans Mono, Monaco, Consolas, \"Lucida Console\", monospace;} </style>")
  :bind (:map markdown-mode-map
              (;(kbd "C-c C-e")
               "" . markdown-export)
              (;(kbd "C-c o")
               "o" . markdown-follow-link-at-point))
  :mode (((rx (or (: bow "README" eow)
                  ) eol) . markdown-mode)))

(use-package edit-server
  :commands (edit-server-start)
  :config
  (custom-set-variables
   '(edit-server-new-frame nil)
   '(edit-server-default-major-mode 'markdown-mode)
   '(edit-server-url-major-mode-alist
     (list
      (cons (rx (or (: ".css" eow)
                    "Configure.aspx"
                    (: "/admin/plugins" eow)))
            'css-mode)
      (cons (rx (or (: ".htm" (? "l") eow)
                    (: "/posts/" (+ alnum))
                    (: ".asp" eow)))
            'html-mode)
      (cons (rx (+? nonl) "/mediawiki/" (+? nonl) eow)
            'mediawiki-mode))))
  :hook ((after-init . edit-server-start)))

(use-package logviewer
  :commands (logviewer-special-handling-csv)
  :mode (((rx (or (: bow "messages" eow)
                  (:  "/" (+? nonl) "_log/" (+? nonl) "."
                      (or "txt" "log" "csv"))
                  (: "." (or "log" "LOG" "Log"))
                  (: (or "log" "LOG" "Log") ".txt")) eol) . logviewer-mode)))

(use-package htmlize
  :commands (htmlize-buffer htmlize-region)
  :config
  (custom-set-variables
   '(htmlize-output-type 'inline-css)))

;; Kconfig-mode
(use-package kconfig-mode  :mode "Kconfig")
(use-package tblog :commands (tblog/new-post tblog/export tblog/find-file))

(use-package conf-mode

  :mode (rx (or "Doxyfile"
                (: (? "_" (+? nonl)) "init_file" (? "_" (+? nonl)))

                (: (+? ascii) (or "." "_") (or "doxy" "doxygen" "service"  "conf" "config" "rc"
                                               "cnf" "options"))
                (: "fvwm/" (+? ascii))
                (: ".config/" (+? ascii) "rc" buffer-end)
                ".globalrc"
                ".gitmodules"
                ".suppressions"
                "conf\.d/")
            eol))

(use-package fvwm-mode
  :mode (rx ".fvwm/" (+ alnum) eol))

(use-package thrift
  :commands (thrift-mode)
  :mode (((rx ".thrift" eol) . thrift-mode)))

;; latex
(yc/add-compile-unit 'latex 10
 (when (or (equal ext "tex")
           (equal ext "TEX"))
   (lambda ()
     (format "xelatex %s" file))))

(use-package tex-mode
  :mode (((rx buffer-start "." (or "tex" "latex") buffer-end) . LaTex-mode)))

(use-package counsel-nerd-fonts
  :commands (counsel-nerd-fonts))

(use-package dockerfile-mode
  :mode (rx buffer-start (or "D" "d") "ockerfile" buffer-end))

(use-package elfeed-org
  :commands (elfeed-org))

(use-package elfeed
  :commands (elfeed)
  :preface
  (defun yc/elfeed-search-eww ()
    "Open feed item via eww."
    (interactive)
    (let ((browse-url-browser-function 'eww))
      (elfeed-search-browse-url)))

  (defun yc/elfeed-show-eww ()
    "Visit the current entry in eww."
    (interactive)
    (let ((browse-url-browser-function 'eww))
      (aif (get-text-property (point) 'shr-url)
          (shr-browse-url)
        (elfeed-show-visit))))

  (defun +rss/delete-pane ()
    "Delete the *elfeed-entry* split pane."
    (interactive)
    (let* ((buf (get-buffer "*elfeed-entry*"))
           (window (get-buffer-window buf)))
      (delete-window window)
      (when (buffer-live-p buf)
        (kill-buffer buf))))

  (defun yc/elfeed-show-entry-buffer (buff)
    "Customized function to show entry buffer (BUFF)."
    (pop-to-buffer buff)
    (PDEBUG "BUF:" buff)
    (yc/enlarge-window-horizontal))


  (defun ace-link-elfeed-search ()
    "Open a visible link in a `help-mode' buffer."
    (interactive)
    (let ((pt (avy-with ace-link-elfeed-search
                (avy-process
                 (mapcar #'cdr (ace-link--elfeed-search-collect))
                 (avy--style-fn avy-style)))))
      (ace-link--elfeed-search-action pt)))

  (defun ace-link--elfeed-search-action (pt)
    (PDEBUG "PT" pt)
    (when (numberp pt)
      (goto-char (1+ pt))
      (call-interactively #'elfeed-search-show-entry)))

  (defun ace-link--elfeed-search-collect ()
    "Collect the positions of visible links in the current `help-mode' buffer."
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))

        (goto-char (point-min))

        (let (beg end candidates )
          (while (looking-at ".+\n")
            (setq beg (match-beginning 0)
                  end (match-end 0))
            (push (cons (buffer-substring-no-properties beg end) beg)
                  candidates)
            (goto-char end))
          (nreverse candidates)))))

  :custom
  (elfeed-db-directory (yc/make-cache-path "elfeed/db/"))
  (elfeed-enclosure-default-dir (yc/make-cache-path "elfeed/enclosures/"))

  (elfeed-search-filter "@2-week-ago +unread")
  (elfeed-show-entry-switch #'yc/elfeed-show-entry-buffer)
  (elfeed-show-entry-delete #'+rss/delete-pane)
  (elfeed-use-curl t)
  (elfeed-search-remain-on-entry t)
  (elfeed-curl-extra-arguments '("-x" "127.0.0.1:7890"))

  (shr-max-image-proportion 0.8)

  :config
  (elfeed-org)

  ;; Some variables after package loaded..
  (setq elfeed-curl-program-name
        (if IS-MAC
            (s-trim (shell-command-to-string "brew list curl | grep bin | grep -v config"))
          "curl"))

  (defadvice! yc/elfeed-adv ()
    "Refresh content."
    :after  #'elfeed
    (elfeed-update))


  (defadvice! yc/ace-link-adv-elfeed-search (&rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :before-until  #'ace-link
    (cond
     ((eq major-mode 'elfeed-search-mode)
      (ace-link-elfeed-search))
     ((memq major-mode '(elfeed-show-mode))
      (ace-link-eww))
     (t nil)))

  :bind
  (:map elfeed-show-mode-map
        ("e" . yc/elfeed-show-eww))
  :bind (("<C-f12>" . elfeed)))

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.config/rime")
  :config
  (setq rime-librime-root
        (catch 'p-found
          (dolist (dir '("~/.local/" "/usr/local" "/usr"))
            (if (yc/file-exists-p (expand-file-name "include/rime_api.h" dir))
                (throw 'p-found (expand-file-name dir))))))

  (defadvice! yc/rime-compile-module-adv (&rest args)
    "Compile librime, with ARGS."
    :before-until  #'rime-compile-module
    (let* ((librime-source-dir (expand-file-name "~/Work/librime/src"))
           (librime-build-dir (expand-file-name "~/Work/librime/build/"))
           (base-command "gcc -std=c99 -shared lib.c -fPIC -O2 -Wall -o librime-emacs.so")
           (cflags (format " -I%s/ -I%s/src/rime" librime-source-dir librime-build-dir))
           (ldflags (format " -L %s/lib/ -Wl,-rpath %s/lib/ -lrime"
                            librime-build-dir librime-build-dir)))
      (awhen (yc/file-exists-p (expand-file-name "lib/librime.so"
                                                 librime-build-dir))
        (PDEBUG "CMD:" (concat  base-command cflags ldflags))
        (if (zerop (shell-command (concat  base-command cflags ldflags)))
            (message "Compile succeed!")
          (error "Compile Rime dynamic module failed"))))))


(use-package w3m
  :custom
  (w3m-default-display-inline-images t)
  (w3m-pixels-per-character 8)
  (w3m-pixels-per-line 16)
  (w3m-resize-image-scale 60)
  (w3m-resize-images t)
  (w3m-toggle-inline-images-permanently nil)
  (w3m-treat-image-size t))


(use-package counsel-tramp-docker
  :commands (counsel-docker counsel-tramp yc/deploy-my-utilies))

(use-package docker
  :commands (docker)
  :config
  (defun docker-container-vterm (container)
    "Open `eshell' in CONTAINER."
    (interactive (list (docker-container-read-name)))
    (let* ((container-address (format "docker:%s:/" container))
           (file-prefix (let ((prefix (file-remote-p default-directory)))
                          (if prefix
                              (format "%s|" (s-chop-suffix ":" prefix))
                            "/")))
           (default-directory (format "%s%s" file-prefix container-address))
           (vterm-bash-file (expand-file-name "emacs-vterm-bash.sh" (format "/docker:%s:~" container))))

      (vterm)

      (let* ((vbuffer (progn
                        (with-current-buffer
                            (generate-new-buffer
                             (concat "vterm-" container))
                          (vterm-mode)
                          (current-buffer))))
             (dir default-directory))

        (unless vbuffer
          (with-current-buffer (setq vbuffer (get-buffer-create name))
            (vterm-mode)))

        (pop-to-buffer-same-window vbuffer)

        (vterm-send-string
         (concat "docker exec -it " container " /bin/bash"))
        (vterm-send-return)

        (PDEBUG "BASH_FILE:" vterm-bash-file
                "D" default-directory
                "C" container
                "B"(format "docker:%s:~" container))

        (yc/deploy-my-utilies (format "/docker:%s:" container))

        ;; (unless (file-exists-p vterm-bash-file)
        ;;   (with-temp-file vterm-bash-file
        ;;     (insert "vterm_set_directory() {\n"
        ;;             "\tvterm_cmd update-pwd \"/" container-address "$PWD/\"\n"
        ;;             "}\n\n")
        ;;     (insert-file-contents "~/.emacs.d/quelpa/build/vterm/etc/emacs-vterm-bash.sh")
        ;;     (goto-char (point-max))
        ;;     (insert "\n")

        ;;     (insert "PROMPT_COMMAND=\"$PROMPT_COMMAND;vterm_set_directory\"\n")))

        (vterm-send-string
         "source ~/emacs-vterm-bash.sh")
        (vterm-send-return)
        vbuffer)))

  (transient-insert-suffix 'docker-container-shells "b"
    '("v" "Vterm" docker-container-vterm))
  )


;; use mu4e as MUA in emacs
(use-package mu4e
  :init
  ;; 配置环境变量 XAPIAN_CJK_NGRAM 为 1，
  ;; 这样使用 mu find 可以搜索任意单个中文字符。
  (setenv "XAPIAN_CJK_NGRAM" "yes")
  ;; 如果 imagemagick 可用，则使用它处理图像。
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; 使用 shr 渲染当前 buffer
  (defun shr-render-current-buffer ()
    "Render the selected region."
    (shr-render-region (point-min) (point-max)))


  ;;
  ;; Xapian, the search engine of mu has a poor support of CJK characters,
  ;; which causes only query contains no more than 2 CJK characters works.
  ;;
  ;; https://researchmap.jp/?page_id=457
  ;;
  ;; This workaroud breaks any CJK words longer than 2 characters into
  ;; combines of bi-grams. Example: 我爱你 -> (我爱 爱你)
  ;; from https://github.com/panjie/mu4e-goodies/blob/master/mu4e-goodies-hacks.el
  ;;
  (defun mu4e-goodies~break-cjk-word (word)
    "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
    (if (or (<= (length word) 2)
            (equal (length word) (string-bytes word))) ; only ascii chars
        word
      (let ((pos nil)
            (char-list nil)
            (br-word nil))
        (if (setq pos (string-match ":" word))     ; like: "s:abc"
            (concat (substring word 0 (+ 1 pos))
                    (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
          (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abc你好
              word
            (progn
              (setq char-list (split-string word "" t))
              (while (cdr char-list)
                (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
                (setq char-list (cdr char-list)))
              br-word))))))

  (defun mu4e-goodies~break-cjk-query (expr)
    "Break CJK strings into bi-grams in query."
    (let ((word-list (split-string expr " " t))
          (new ""))
      (dolist (word word-list new)
        (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))


  ;;
  ;; Remove extra blanklines generated by html2text command like
  ;; mu4e-shr2text.
  ;; from https://github.com/panjie/mu4e-goodies/blob/master/mu4e-goodies-hacks.el
  ;;
  (defun mu4e-goodies-message-delete-html-extra-blanklines (msg body)
    "Delete extra blank lines generated by html2text command/functions like mu4e-shr2text"
    (if mu4e~message-body-html
        (with-temp-buffer
          (insert body)
          (goto-char (point-min))
          (flush-lines "^$")
          (buffer-string))
      body))


  :custom
  (mail-user-agent 'mu4e-user-agent)

  ;; folders.
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder "/Sent Messages")
  (mu4e-trash-folder "/Deleted Messages")
  (mu4e-attachment-dir  "~/Downloads")


  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)

  ;; 回复邮件插入邮件引用信息
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

  ;; 启用 inline iamge 显示并定义显示图片最大宽度
  (mu4e-view-show-images t)
  (mu4e-view-image-max-width 800)

  (mu4e-html2text-command #'shr-render-current-buffer)

  ;; (mu4e-compose-reply-to-address "yingchao.yang@iclould.com")
  (mu4e-compose-signature-auto-include t)
  (message-signature-file "~/.emacs.d/.signature")
  (mu4e-compose-format-flowed nil)
  (mu4e-completing-read-function 'ivy-completing-read)
  (mu4e-compose-dont-reply-to-self t)

  ;; This enabled the thread like viewing of email similar to gmail's UI.
  (mu4e-headers-include-related t)
  (mu4e-sent-messages-behavior 'sent)

  (message-kill-buffer-on-exit t)
  (mu4e-confirm-quit nil)

  ;; give me ISO(ish) format date-time stamps in the header list
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")

  (mu4e-view-show-addresses t)

  (mu4e-query-rewrite-function #'mu4e-goodies~break-cjk-query)
  (mu4e-change-filenames-when-moving t)

  :config
  (require 'shr)

  ;; Sometimes html email is just not readable in a text based client, this lets me open the
  ;; email in my browser.
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-message-body-rewrite-functions
               'mu4e-goodies-message-delete-html-extra-blanklines)

  (require 'org-mime)

  (defun mu4e-compose-org-mail ()
    (interactive)
    (mu4e-compose-new)
    (org-mu4e-compose-org-mode))

  (defun htmlize-and-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize and send it."
    (interactive)

    (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (org-mime-htmlize)
      (message-send-and-exit))
    )

  (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)


  ;; Spell checking ftw.
  :hook ((mu4e-compose-mode . flyspell-mode))
  :bind (:map mu4e-main-mode-map
              ("g" . mu4e-update-mail-and-index)
              ("G" . mu4e-update-mail-and-index))
  :bind (("C-<f11>" . mu4e)))

(provide '07-other-modes)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 07-other-modes.el ends here
