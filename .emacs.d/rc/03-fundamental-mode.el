;;; 03-rc-fundamental-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Settings for all modes..

;;; Code:


(use-package helpful
  :commands (helpful-callable helpful-variable)
  :bind (([remap describe-key] . 'helpful-key)))

 ;; ivy mode
(use-package ivy
  :ensure t
  :commands (ivy-read)
  :custom
  (ivy-use-virtual-buffers t)        ;; Enable bookmarks and recentf
  (ivy-count-format "%d/%d ")        ;; Display count displayed and total
  (enable-recursive-minibuffers t)
  (ivy-height 17)                    ;; Number of result lines to display
  (ivy-initial-inputs-alist nil)     ;; No regexp by default
  (ivy-wrap nil)                     ;; wrap candidates
  (ivy-sort-max-size 7500)
  (ivy-on-del-error-function #'ignore) ;; don't quit minibuffer on delete-error

  :bind ((;(kbd "C-c C-r")
          "" . ivy-resume)
         (;(kbd "M-r")
          [134217842] . ivy-resume)

         ([remap switch-to-buffer] . ivy-switch-buffer)
         )
  :config
  (PDEBUG "LOADING ivy.." )
  (require 'ivy-rich)
  (ivy-mode)
  (ivy-rich-mode -1)
  (ivy-rich-mode 1))

;;;; Ivy-rich
;; More friendly display transformer for Ivy
(use-package ivy-rich
  ;; :after (ivy counsel)
  :preface
  (defun +ivy-rich-describe-variable-transformer (cand)
    "Previews the value of the variable (CAND) in the minibuffer."
    (let* ((sym (intern cand))
           (val (and (boundp sym) (symbol-value sym)))
           (print-level 3))
      (replace-regexp-in-string
       "[\n\t\^[\^M\^@\^G]" " "
       (cond ((booleanp val)
              (propertize (format "%s" val) 'face
                          (if (null val)
                              'font-lock-comment-face
                            'success)))
             ((symbolp val)
              (propertize (format "'%s" val)
                          'face 'font-lock-string-face))
             ((keymapp val)
              (propertize "<keymap>" 'face 'font-lock-constant-face))
             ((listp val)
              (prin1-to-string val))
             ((stringp val)
              (propertize (format "%S" val) 'face 'font-lock-string-face))
             ((numberp val)
              (propertize (format "%s" val) 'face 'font-lock-builtin-face))
             ((format "%s" val)))
       t)))
  :ensure t
  :custom
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-path-style 'abbrev)
  :config
  (PDEBUG "Loading ivy-rich...")
  (defadvice! yc/ivy-rich-switch-buffer-indicators-adv (orig-func candidate)
    "Append gdb status for gdb buffers.
ORIG-FUNC is called with CANDIDATE."
    :around  #'ivy-rich-switch-buffer-indicators
    (let* ((status (funcall orig-func candidate))
           (buffer (get-buffer candidate))
           (name (buffer-name buffer)))
      (if (string-match-p (rx "*gdb" (+ space) (+? nonl) (+ space) "shell*") name)
          (concat (if (string-empty-p status) "  " (concat status "-"))
                  (with-current-buffer buffer
                    (if (process-live-p (get-buffer-process buffer))
                        (if comint-last-prompt
                            (if (= (marker-position (car comint-last-prompt))
                                   (marker-position (cdr comint-last-prompt)))
                                "R" ;; running
                              "S")  ;; stopped
                          "U") ;; unkown
                      "D") ;; dead
                    ))
        status)))

  (plist-put! ivy-rich-display-transformers-list
    'ivy-switch-buffer
    '(:columns
     ((ivy-switch-buffer-transformer
       (:width 80))
      (ivy-rich-switch-buffer-size
       (:width 7))
      (ivy-rich-switch-buffer-indicators
       (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode
       (:width 12 :face warning))
      (ivy-rich-switch-buffer-project
       (:width 15 :face success))
      (ivy-rich-switch-buffer-path
       (:width
        (lambda
          (x)
          (ivy-rich-switch-buffer-shorten-path x
                                               (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda
       (cand)
       (get-buffer cand)))

    'counsel-bookmark
    '(:columns
      ((ivy-rich-candidate (:width 30))
       (ivy-rich-bookmark-info (:face font-lock-doc-face))))

    'counsel-describe-variable
    '(:columns
      ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
       (+ivy-rich-describe-variable-transformer (:width 50)) ; display variable value
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))

    ;; Apply switch buffer transformers to `counsel-projectile-switch-to-buffer' as well
    'counsel-projectile-switch-to-buffer
    (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))

(use-package counsel-utils
  :commands (yc/counsel-grep counsel-find-file-as-user counsel-grep-in-dir yc/projectile-grep)
  :bind (
         (;; ,(kbd "C-c k")
          "k"  . yc/counsel-grep)
         ([remap project-find-regexp] . yc/projectile-grep )))


(defvar yc/ivy-common-actions
  '(("u" counsel-find-file-as-user "Open as other user")
    ("g" counsel-grep-in-dir "Grep in current directory")
    ("l" find-file-literally "Open literally")
    ("v" vlf "Open with VLF")
    ("d" counsel-locate-action-dired "dired")
    )
  "My actions.")

(autoload 'ffap-string-at-point "ffap")

(use-package counsel
  :commands (counsel-find-file
             counsel-recentf counsel-semantic-tags
             counsel-fzf counsel-imenu-categorize-functions
             counsel-grep-or-swiper
             counsel-git-grep)
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (rx (or (: buffer-start (or "#" "."))
                                           (: (or "#" "~")  buffer-end)
                                           (: buffer-start ".ccls-cache" buffer-end)
                                           (: ".elc")
                                           )))
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never %s")
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)

  :bind (
         ([remap apropos]                  .  counsel-apropos)
         ([remap bookmark-bmenu-list]      .  counsel-bookmark)
         ([remap describe-bindings]        .  counsel-descbinds)
         ([remap describe-face]            .  counsel-faces)
         ([remap describe-function]        .  counsel-describe-function)
         ([remap describe-variable]        .  counsel-describe-variable)
         ([remap find-file]                .  counsel-find-file)
         ([remap imenu]                    .  counsel-imenu)
         ([remap org-goto]                 .  counsel-org-goto)
         ([remap org-set-tags-command]     .  counsel-org-tag)
         ([remap recentf-open-files]       .  counsel-recentf)
         ([remap set-variable]             .  counsel-set-variable)
         ([remap swiper]                   .  counsel-grep-or-swiper)
         ([remap occur]                    . counsel-grep-or-swiper)
         ([remap unicode-chars-list-chars] .  counsel-unicode-char)
         ([remap yank-pop]                 .  counsel-yank-pop)
         ([remap execute-extended-command] .  counsel-M-x)
         ([remap eshell-previous-matching-input] . counsel-esh-history))
  :bind (:map ctl-x-map
              ("\C-f" . counsel-find-file)
              ("\C-r" . counsel-recentf)
              ("F" . 'counsel-fzf)
              ("gg" . 'counsel-git-grep))
  :config
  (defalias 'git-grep 'counsel-git-grep)
  (setq counsel-fzf-dir-function
        (lambda ()
          default-directory))

  (if (executable-find "rg")
      (setq counsel-grep-base-command
            "rg -S --no-heading --line-number --color never %s %s"))

  (defadvice! yc/counsel-grep-or-swiper-adv (orig-func &rest args)
    "Call counsel-grep-or-swiper with symbol-at-point.
ORIG-FUNC is called with ARGS."
    :around #'counsel-grep-or-swiper
    (apply orig-func (or args (list (aif (symbol-at-point) (symbol-name it))))))

  (defadvice! yc/counsel-git-grep-action-adv (x)
    "ORIG-FUNC is called with ARGS."
    :before-until #'counsel-git-grep-action
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (let* ((file-name (match-string-no-properties 1 x))
             (line-number (match-string-no-properties 2 x))
             (buffer (get-file-buffer (expand-file-name
                                       file-name
                                       (ivy-state-directory ivy-last)))))
        (PDEBUG "BUF:" buffer
                "FILE:" file-name
                "LINE:" line-number)
        (when buffer
          (with-current-buffer buffer
            (switch-to-buffer buffer );; (display-buffer buffer)
            (goto-char (point-min))
            (forward-line (1- (string-to-number line-number)))

            (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
              (when swiper-goto-start-of-match
                (goto-char (match-beginning 0))))

            (swiper--ensure-visible)
            (recenter)

            (run-hooks 'counsel-grep-post-action-hook)
            (unless (eq ivy-exit 'done)
              (swiper--cleanup)
              (swiper--add-overlays (ivy--regex ivy-text))))
          t))))

  (ivy-add-actions 'counsel-find-file  yc/ivy-common-actions)

  (defvar yc/counsel-find-file-line-number nil "Nil.")

  (defadvice! yc/counsel-find-file-adv (orig-func &rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :around  #'counsel-find-file
    (PDEBUG "LINE_NUM: " yc/counsel-find-file-line-number)

    (let* ((string (aif (ffap-string-at-point) (file-name-nondirectory it)))
           (file-opened (apply orig-func args)))

      (PDEBUG "STR" string)
      (when (string-match (rx (group (+? nonl))  ":" (group (+ digit))) string)
        (let* ((file-name (match-string 1 string))
               (line-str  (match-string 2 string))
               (line-no   (string-to-number line-str)))
          (PDEBUG "F1:" file-name "L:" line-no)

          (PDEBUG "F2:" file-name "L:" line-no "G:"
                  "N" file-opened ", "
                  (and file-opened line-no
                       (string= (file-name-nondirectory file-opened) file-name)))
          (when (and file-opened line-no
                     (string= (file-name-nondirectory file-opened) file-name))
            (with-current-buffer (find-buffer-visiting file-opened)
              (PDEBUG "GOTO LINE:" line-no (current-buffer))
              (goto-char (point-min))
              (forward-line (1- line-no))))))))

  (defadvice! yc/counsel-git-grep-adv (orig-func &optional initial-input &rest args)
    "Wrapper of ORIG-FUNC, provide proper INITIAL-INPUT value, and save original position."
    :around  #'counsel-git-grep
    (let ((m (point-marker)))
      (push (or initial-input (aif (symbol-at-point) (symbol-name it))) args)
      (apply orig-func args)
      (yc/push-stack m))))

(use-package bookmark
  :custom
  (bookmark-default-file (yc/make-cache-path "bookmarks"))
  :config
  (defadvice! yc/bookmark-set-adv (&rest args)
    "Save book mark file after new bookmark is added.
ORIG-FUNC is called with ARGS."
    :after #'bookmark-set
    (bookmark-save)))


;; With smex, ivy can sort commands by frequency.
(use-package amx
  :ensure t
  :custom
  (amx-history-length 20))

 ;; Projectile...
(use-package projectile
  :preface
  (defun yc/kill-non-project-buffers (&optional kill-special)
    "Kill buffers that do not belong to a `projectile' project.

With prefix argument (`C-u'), also kill the special buffers."
    (interactive "P")
    (let ((bufs (buffer-list (selected-frame))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (let ((buf-name (buffer-name buf)))
            (when (or (null (projectile-project-p))
                      (and kill-special
                           (string-match "^\*" buf-name)))
              ;; Preserve buffers with names starting with *scratch or *Messages
              (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                (message "Killing buffer %s" buf-name)
                (kill-buffer buf))))))))
  :commands (projectile-project-root projectile-find-other-file)
  :bind (("C-x M-k" . projectile-kill-buffers)
         ("C-x M-j" . projectile-dired)
         ("C-x M-s" . projectile-save-project-buffers))

  :custom
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))
  (counsel-projectile-switch-project-action 'find-file)

  :config
  (cond
   ;; If fd exists, use it for git and generic projects. fd is a rust program
   ;; that is significantly faster than git ls-files or find, and it respects
   ;; .gitignore. This is recommended in the projectile docs.
   ((executable-find "fd")
    (setq-default projectile-generic-command "fd . -0 --type f --color=never"))
   ((executable-find "rg")
    (setq projectile-generic-command "rg -0 --files --follow --color=never --hidden"))))


(use-package counsel-projectile
  :ensure t
  :preface
  (defun yc/projectile-find-file (&rest args)
    "My own version of `projectile-find-file'.
Faster than projectile-find-file, since git submodules are ignored.
Call FUNC which is 'projectile-find-file with ARGS."
    (interactive "P")
    (unless (featurep 'counsel-projectile)
      (require 'counsel-projectile))
    (cond
     (current-prefix-arg
      (ivy-read "Find file: "

                (let ((cmd (cond
                             ((executable-find "fd")
                              "fd . -0 --no-ignore --type f --color=never")
                             ((executable-find "rg")
                              "rg -0 --no-ignore  --files --follow  --color=never --hidden")
                             (t (projectile-get-ext-command nil)))))
                  (projectile-files-via-ext-command default-directory cmd))

                :matcher counsel-projectile-find-file-matcher
                :require-match t
                :sort counsel-projectile-sort-files
                :action #'find-file
                :caller 'counsel-projectile-find-file))

     ((file-exists-p (concat (projectile-project-root) ".git"))
      ;; find file, exclude files in submodules.
      (unless (featurep 'magit-process)
        (require 'magit-process))
      (ivy-read (projectile-prepend-project-name "Find file: ")
                (magit-revision-files "HEAD")
                :matcher counsel-projectile-find-file-matcher
                :require-match t
                :sort counsel-projectile-sort-files
                :action counsel-projectile-find-file-action
                :caller 'counsel-projectile-find-file))
     (t
      (counsel-projectile-find-file))))
  :defines (counsel-projectile-find-file-matcher counsel-projectile-sort-files
                                                 projectile-files-via-ext-command)
  :functions (counsel-projectile-find-file-action)
  :commands (counsel-projectile-find-file)
  :hook ((emacs-startup . counsel-projectile-mode))

  :bind (("C-x M-f" . yc/projectile-find-file)
         ("C-x M-d" . counsel-projectile-find-dir)
         ("C-x M-b" . counsel-projectile-switch-to-buffer)
         ([remap project-switch-to-buffer] . counsel-projectile-switch-to-buffer)
         ([remap project-switch-project] . counsel-projectile-switch-project)))

(use-package smartparens
  :ensure t
  :commands (smartparens-global-mode sp-local-pairs sp-with-modes)
  :hook ((after-init . smartparens-global-mode))
  :custom
  (sp-escape-quotes-after-insert nil)
  (sp-max-prefix-length 25)
  :custom-face
  (sp-pair-overlay-face ((t nil)))
  :config
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              ;; (;; (kbd "C-M-n")
              ;;  [134217742] . sp-forward-sexp)

              ;; (;; (kbd "C-M-p")
              ;;  [134217744] . sp-backward-sexp)

              (;; (kbd "C-M-k")
               [134217739] . sp-kill-sexp)
              (;; (kbd "C-M-w")
               [134217751] . sp-copy-sexp)))

 ; VLF: view large file.
(use-package vlf
  :commands (vlf)
  :custom
  (vlf-batch-size 2000000) ;; 2 MB.
  )

(use-package files
  :custom
  (large-file-warning-threshold (* 20 1024 1024))
  :config
  ;;Handle file-error and suggest to install missing packages...
  (advice-add 'set-auto-mode :around #'yc/install-package-on-error)

  (defadvice! yc/find-file-noselect-adv (orig-func &rest args)
    "Docs.
ORIG-FUNC is called with ARGS."
    :around #'find-file-noselect
    (condition-case var
        (apply orig-func args)
      (error (progn
               (PDEBUG "VAR: " var)
               (if (string= (cadr var) "File already visited literally")
                   (find-buffer-visiting (car args))
                 (error "%s" (cadr var)))))))

  (defadvice! yc/abort-if-file-too-large-adv (size op-type filename  &optional OFFER-RAW)
    "Advice for `abort-if-file-too-large'.
If file SIZE larger than `large-file-warning-threshold', allow user to use
`vlf' to view part of this file, or call original FUNC which is
`abort-if-file-too-large' with OP-TYPE, FILENAME."
    :before-until #'abort-if-file-too-large
    (when (and (string= op-type "open")
               large-file-warning-threshold size
               (> size (* 100 1024 1024))
               (not (member (file-name-extension filename) '("pdf")))) ;; 100 MB
      (if (y-or-n-p (format "File %s is large (%s), view with VLF mode? "
                            (file-name-nondirectory filename)
                            (file-size-human-readable size)))
          (vlf filename)))))


(use-package server
  :commands (server-start server-running-p)
  :hook ((emacs-startup .
                        (lambda ()
                          (unless (server-running-p)
                            (server-start))))))

 ;;; ABBREV-MODE;;;
(use-package abbrev

  :custom
  (abbrev-file-name  "~/.emacs.d/abbrev_defs")
  (save-abbrevs 'silently)
  :hook ((after-init . abbrev-mode))
  :config
  (progn
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))))

(use-package ibuffer
  :custom
  (ibuffer-show-empty-filter-groups nil)
  :config

  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size)))

  :bind ((;; ,(kbd "C-x C-b")
          "". ibuffer))
  :bind (:map ibuffer-mode-map
              (;; (kbd "C-x C-f")
               "" . counsel-find-file)))


(autoload 'switch-window "switch-window" ""  t)
(defun yc/switch-window (&optional reverse)
  "Switch window.
With REVERSE is t, switch to previous window."
  (interactive)
  (if (> (length (window-list)) 3)
      (switch-window)
    (other-window (if reverse 2 1)))
  (yc/update-term-title))

(defun auto-rename-buffer ()
  "Rename current buffer."
  (interactive)
  (let ((newname (concat (buffer-name) "-" (format-time-string  "%H:%M:%S" (current-time)))))
    (rename-buffer newname)))

(yc/set-keys
 (list
  (cons (kbd "<C-f2>") 'rename-buffer)
  (cons (kbd "<f2>") 'auto-rename-buffer)
  (cons (kbd "C-x o") 'yc/switch-window)
  (cons (kbd "C-x O") (lambda ()(interactive) (yc/switch-window t)))))

;; string functions..
(use-package s
  :commands (s-contains?
             s-ends-with? s-ends-with-p
             s-starts-with? s-blank? s-split))

(use-package super-save
  :ensure t
  :commands (super-save-mode)
  :hook ((emacs-startup . super-save-mode))
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil))

;; Tabs and spaces
(use-package ws-butler
  :ensure t
  :commands (ws-butler-mode)
  :hook ((prog-mode .  ws-butler-mode))
  :custom
  (tab-always-indent 'complete)
  (tab-width 4)
  (c-basic-offset 4)
  (indent-tabs-mode nil))


(use-package undo-tree
  :defer t
  :ensure t
  :commands (global-undo-tree-mode undo-tree-undo undo-tree-visualize undo-tree-redo)
  :bind (:map undo-tree-map
              (;; (kbd "C-x U")
               "U" . 'undo-tree-visualize)
              (;; ,(kbd "\C-x u")
               "u". 'undo-tree-undo)
              (;; ,(kbd "\C-x M-u")
               [24 134217845]. 'undo-tree-redo))
  :bind ((;(kbd "C-x U")
          "U" . 'undo-tree-visualize)
         (;; ,(kbd "\C-x u")
          "u". 'undo-tree-undo)
         (;; ,(kbd "\C-x M-u")
          [24 134217845]. 'undo-tree-redo))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)

  :hook (after-init . global-undo-tree-mode)
)

;; Preview when `goto-line`

(use-package goto-line-preview
  :preface
  (defvar-local preview-hl-overlay nil "Nil.")
  :ensure t
  :bind ([remap goto-line] . goto-line-preview)
  :config
  (defun yc/remove-preview-overlay ()
    "Remove preview overlay."
    (when preview-hl-overlay
      (delete-overlay preview-hl-overlay))
    (setq preview-hl-overlay nil))

  (defadvice! yc/goto-line-preview--do-adv (line-num)
    "Highlight LINE-NUM to improve focus.
ORIG-FUNC is called with ARGS."
    :after  #'goto-line-preview--do
    (yc/remove-preview-overlay)
    (setq preview-hl-overlay (make-overlay
                              (line-beginning-position)
                              (1+ (line-end-position))))
    (overlay-put preview-hl-overlay 'face 'swiper-line-face))

  :hook ((goto-line-preview-after . yc/remove-preview-overlay)))


(use-package layout-restore
  :commands (layout-save-current layout-restore))


(use-package which-key
  :ensure t
  :commands (which-key-mode)
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 15)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :hook ((after-init . which-key-mode)))

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-user-data-dir "~/.config/rime"))


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
(provide '03-fundamental-mode)
;;; 03-fundamental-mode.el ends here
