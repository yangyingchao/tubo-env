;;; 081-org-mode.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(use-package ox-plus
  :commands (yc/ditaa-path yc/plantuml-path uml/parse-stringfied-nodes))

;; PlantUML,
(use-package flycheck-plantuml
  :commands (flycheck-plantuml-setup))

(use-package plantuml-mode
  :preface
  (defun company-plantuml--candidates (prefix)
    "Return candiates for `PREFIX'."
    (all-completions prefix plantuml-kwdList))

  (defun company-plantuml (command &optional arg &rest ignored)
    "`company-mode' completion backend for plantuml.
plantuml is a cross-platform, open-source make system."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-plantuml))
      (init (unless (equal major-mode 'plantuml-mode)
              (error "Major mode is not plantuml-mode")))
      (prefix (and (not (company-in-string-or-comment))
                   (company-grab-symbol)))
      (candidates (company-plantuml--candidates arg))))


  :mode (rx "." (or "plantuml" "puml" "plu" "pu") eow)
  :hook ((plantuml-mode .
                        (lambda ()
                          (flycheck-plantuml-setup)
                          (uml/parse-stringfied-nodes t))))
  :config
  (setq plantuml-jar-path (yc/plantuml-path))
  (aif (executable-find "dot")
      (setenv "GRAPHVIZ_DOT" it)
    (warn "plantUML depends on graphviz, which is not installed."))
  (yc/add-company-backends 'plantuml-mode 'company-plantuml)

  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4))

(use-package ffap
  :commands (ffap-url-p)
  :custom
  (ffap-machine-p-known 'reject)
  )

(use-package org-download
  :preface
  (defun yc/org-download-annotate-func (link)
    "Annotate LINK with the time of download."
    (let ((lable (format "fig:%s"
                         (replace-regexp-in-string
                          (rx (+ space)) "_"
                          (file-name-sans-extension (file-name-nondirectory link)) t t))))

      (kill-new (format "[[%s]]" lable))
      (concat "#+CAPTION: \n"
              (format "#+NAME: %s\n" lable)
              (if (ffap-url-p link)
                  (format "#+DOWNLOADED: %s @ %s\n"
                          link
                          (format-time-string "%Y-%m-%d %H:%M:%S"))
                ""))))

  (defun yc/get-image-width (filename)
    "Returns width of file FILENAME in pixel."
    (unless (file-exists-p filename)
      (error "File %s does not exist!" filename))
    (with-temp-buffer
      (insert-image-file filename)
      (car (image-size
            (image-get-display-property) t))))

  :commands (org-download-image org-download-screenshot)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl 0)
  (org-download-timestamp nil)
  (org-download-image-html-width 960)
  (org-download-image-org-width 960)

  :config
  (setq-default
    org-download-screenshot-method
    (cond
     ((executable-find "screencapture") "screencapture -i %s")
     ((executable-find "scrot") "scrot -s %s")
     ((executable-find "gnome-screenshot") "gnome-screenshot -a -f %s")
     (t "")))
  (setq org-download-annotate-function 'yc/org-download-annotate-func
        org-download-file-format-function 'identity)

  (defadvice! yc/org-download-screenshot-adv (orig-func &rest args)
    "Assign file name for screenshot before calling ORIG-FUNC with ARGS."
    :around #'org-download-screenshot
    (let ((org-download-screenshot-file
           (expand-file-name (format-time-string "screenshot@%Y-%m-%d_%H:%M:%S.png")
                             temporary-file-directory)))
      (apply orig-func args))
    )

  (defadvice! yc/org-download-insert-link-adv (orig-func link filename)
    "Set image width to proper value before calling ORIG-FUNC is called with ARGS.
This will add proper attributes into org file so image won't be too large."
    :around #'org-download-insert-link
    (let* ((width (if (fboundp 'image-size)
                      ;; if function `image-size' is avaiable,  we can do some calculation.
                      (if (file-exists-p filename)
                          ;; if file exists, calculate width to be used.
                          (let ((actual-width (yc/get-image-width filename)))
                            (if (> actual-width 960)
                                960 0))
                        ;; or, set to -1, and update it later.
                        -1)
                    ;; otherwise, return 0 to disable this feature.
                    0))
           (org-download-image-html-width width)
           (org-download-image-org-width width))
      (PDEBUG "width: " width)
      (funcall orig-func link filename)))

  (defadvice! yc/org-download--image/url-retrieve-adv (link filename)
    "Retrieve LINK and save as FILENAME."
    :override #'org-download--image/url-retrieve
    (url-retrieve
     link
     (lambda (status filename buffer)
       (org-download--write-image status filename)
       (cond ((org-download-org-mode-p)
              (with-current-buffer buffer
                (org-download--display-inline-images)))
             ((eq major-mode 'dired-mode)
              (let ((inhibit-message t))
                (with-current-buffer (dired (file-name-directory filename))
                  (revert-buffer nil t)))))
       (with-current-buffer buffer
         (let* ((width (round (yc/get-image-width filename)))
                (width-str (concat (number-to-string (if (> width 960) 960 width)) "px")) )
           (save-excursion
             (let ((end (point)))
               (forward-line -4)
               (PDEBUG "W:" (point) end (buffer-substring-no-properties (point) end))
               (narrow-to-region (point) end)
               (while (search-forward "-1px" nil t)
                 (replace-match width-str nil t)))
             (widen)))

         (org-download--display-inline-images)
         )
       )
     (list
      (expand-file-name filename)
      (current-buffer))
     nil t))

  (defadvice! yc/org-download--dir-2-adv ()
    "Use base-name of current buffer as seccond part of directory name.
Final image name looks like 'images/org_file_name/xxx.png'.
This makes it easier to move org file (and associated images) to other directory."
    :override  #'org-download--dir-2
    (file-name-base (buffer-file-name))))




;; *************************** Org Mode ********************************
(use-package org-indent
  :commands (org-indent-mode))

(use-package ol
  :bind (("\C-cl" . org-store-link)))

(use-package org-agenda
  :custom
  (org-agenda-files (list (expand-file-name "~/Work/org/")))
  (org-agenda-dim-blocked-tasks (quote invisible))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)

  :bind(("\C-ca" . org-agenda))
  :config
  (dolist (item (directory-files org-directory t))
    (unless (or (not (file-directory-p item))
                (member (file-name-base item) '(".git" "slides" "images" "assets" "references" "." "..")))
      (add-to-list 'org-agenda-files item))))

(use-package org-capture
  :bind (("<M-S-f10>" . org-capture))
  :custom
  (org-capture-templates
   `(
     ("p" "Protocol" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	   ("L" "Protocol Link" entry (file+headline ,(concat org-directory "gtd.org") "Inbox")
      "* %? [[%:link][%:description]] \nCaptured On: %U")
     ("t" "Todo" entry (file+headline ,(expand-file-name "gtd.org" org-directory)  "Tasks")
      "\n* TODO %?\n  %i\n  %a")
     ("P" "Project" entry (file+headline ,(expand-file-name "gtd.org" org-directory) "Project")
      "\n** NOTE %?\n %i\n %a" )
     ("n" "Note" entry (file+headline ,(expand-file-name "gtd.org" org-directory) "Note")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("i" "Idea" entry (file+headline ,(expand-file-name "gtd.org" org-directory) "Idea")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("m" "Mail" entry (file+headline ,(expand-file-name "~/tmp/mail.org") "Mail")
      "* %?\nEntered on %U\n  %i\n  %a")
     ))
  :config
  (require 'org-protocol)
  )

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode))
  :custom
  (org-superstar-headline-bullets-list '( "●"  "◎" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

(use-package ox-latex
  :custom
  (org-latex-compiler "xelatex")
  (org-latex-default-figure-position "htbp";; "H"
                                     )
  (org-latex-prefer-user-labels t)
  (org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks={true},
CJKbookmarks={true},
linkcolor={black},
urlcolor={blue},
menucolor={blue}}
")
  :config
  (defadvice! yc/org-latex-special-block-adv (orig-func &rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :around #'org-latex-special-block

    (if (string= (org-element-property :type (car args)) "NOTES")
        (cadr args)
      (apply orig-func args))))

(use-package ox-publish
  :commands (org-publish-needed-p)
  :config
  (defadvice! yc/org-publish-needed-p-adv (orig-func &rest args)
    "Save current execution before calling ORIG-FUNC with ARGS."
    :around  #'org-publish-needed-p
    (save-excursion
      (apply orig-func args))))

(use-package org
  :commands (org-load-modules-maybe)
  :preface
  (defun yc/open-gtd ()
    "Open my gtd file."
    (interactive)
    (find-file (expand-file-name "gtd.org" org-directory)))

  (defun auto-insert--org-mode (&optional fn)
    "Update for `org-mode'.
 Update file name, replace '-' or '_' with whitespace."
    (interactive)
    (auto-update-defaults)

    (let* ((fn (or fn (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (bname (if (string-match
                       (rx bol
                           (repeat 4 digit) "-"   ;; year
                           (repeat 1 2 digit) "-" ;; month
                           (repeat 1 2 digit)     ;; day
                           "-" (group (* anything)) eol)
                       fn)
                      (match-string 1 fn)
                    fn))
           (replacement (replace-regexp-in-string (regexp-quote "-") " " bname t t )))

      (yc/auto-update-template "FILE_NO_EXT_MINUS" replacement))


    ;; create directory for images...
    (unless (file-directory-p "images")
      (mkdir "images"))

    (if (executable-find "latex")
        (yc/auto-update-template "TEX_PREVIEW" "latexpreview")
      (yc/auto-update-template "TEX_PREVIEW" "")))

  (defun yc/org-view-exported-file ()
    "View exported file, either with eww, or browser."
    (interactive)
    (let ((file (concat (file-name-sans-extension (buffer-file-name))  ".html")))
      (unless (file-exists-p file)
        (error "File %s does nboit exist" file))

      (if current-prefix-arg
          (browse-url (concat "file://" file))
        (eww (concat "file://" file)))))


  ;; hide blocks marked as :hidden
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))


  ;; Auto set CUSTOM_ID...
  (defun yc/org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id))
          (if (called-interactively-p)
              (message "ORG-ID: %s" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          (if (called-interactively-p)
              (message "ORG-ID: %s" id))
          id)))))

  (defun yc/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file which do not already have one."
    (interactive)
    (when (and buffer-file-name
               (file-exists-p buffer-file-name)
               (eq major-mode 'org-mode))
      (delete-trailing-whitespace)

      (unless (member (file-name-base buffer-file-name) '("elfeed" "gtd"))
        (save-excursion
          (org-map-entries (lambda () (yc/org-custom-id-get (point) 'create)) nil 'file)))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun yc/org-update-yank ()
    "Update yanked text, removing or adding proper spaces."
    (interactive)
    (save-excursion
      ;; 测试 AAA --> 测试 AAA
      (goto-char (point-min))
      (while (search-forward-regexp (rx (group (not ascii))  (* blank) (group  ascii)) nil t)
        (replace-match "\\1 \\2" nil nil))

      ;;  AAA测试 -->  AAA 测试
      (goto-char (point-min))
      (while (search-forward-regexp (rx  (group  ascii) (* blank) (group (not ascii))) nil t)
        (replace-match "\\1 \\2" nil nil))

      ;; remove leading space
      (goto-char (point-min))
      (while (search-forward-regexp (rx  bol (+ blank)) nil t)
        (replace-match "")))

    ;; remove trailing space
    (delete-trailing-whitespace))

  :custom
  (org-image-actual-width nil)
  (org-confirm-babel-evaluate nil)
  (org-directory (expand-file-name "~/Documents/Database/org/"))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-export-time-stamp-file nil)
  (org-hide-leading-stars t)
  (org-html-head-include-default-style nil)
  (org-html-head-include-scripts nil)
  (org-log-done 'time)
  (org-pretty-entities t)
  (org-preview-latex-image-directory "/tmp/")
  (org-publish-list-skipped-files nil)
  (org-refile-targets (quote ((nil :maxlevel . 2))))  (org-html-postamble nil)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-use-property-inheritance t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-display-remote-inline-images 'download) ;; download and display remote images.

  ;; WAITING: Assigned to others, and waiting for their report.
  ;; PENDING: Pending for some reason, maybe scheduled but not started because task dependency.
  (org-todo-keywords (quote ((sequence "TODO(t)" "WAITING(w)" "DOING(g)"
                                       "DONE(d)" "CANCELED(c)" "PENDING(p)" ))))
  (org-tag-alist '(("question" . ?q) ("noexport" . ?n)))
  (org-tag-faces
   '(("HIGH" . (:foreground "red" :weight bold)) ("MEDIUM" . org-warning)
     ("LOW" . (:foreground "blue" :weight bold))))

  (org-return-follows-link t) ;; make RET follow links
  (org-html-inline-image-rules
   '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")
     ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")
     ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|tiff\\|svg\\)\\'")))

  :hook ((org-after-todo-statistics . org-summary-todo)
         (before-save . yc/org-add-ids-to-headlines-in-file)
         (org-mode
          .
          (lambda ()
            (if window-system
                (org-display-inline-images))
            (setq show-trailing-whitespace t)
            (individual-visibility-source-blocks))))

  :config
  (progn
    (require 'ox-plus)

    ;; some variables are customized after package loaded.
    (custom-set-variables
     '(org-ditaa-jar-path (yc/ditaa-path))
     '(org-plantuml-jar-path (yc/plantuml-path)))

    (sp-with-modes 'org-mode
      (sp-local-pair "$" "$")
      (sp-local-pair "（" "）")
      (sp-local-pair "“" "“"))

    (setq org-comment-string "ORG_COMMENT")

    (org-indent-mode 1)

    (font-lock-add-keywords
     'org-mode
     `((,(rx bow (group "TODO "))
        (1 font-lock-comment-face))
       (,(rx bow (group "DONE "))
        (1 font-lock-builtin-face))
       (,(rx bow (group "DOING "))
        (1 font-lock-function-name-face))))

    (defadvice! yc/org-html-paragraph-adv (orig-func &rest args)
      "ORIG-FUNC is called with ARGS.
Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to html."
      :around #'org-html-paragraph
      (let ((orig-contents (cadr args))
            (reg-han "[[:multibyte:]]"))
        (setf (cadr args) (replace-regexp-in-string
                           (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                           "\\1\\2" orig-contents))
        (apply orig-func args)))

    (defadvice! yc/org-open-at-point-adv (orig-func &rest args)
      "Save current location before open new file/location.
ORIG-FUNC is called with ARGS."
      :around #'org-open-at-point
      (let ((m (point-marker)))
        (progn
          (apply orig-func args)
          (yc/push-stack m))))

    (defadvice! yc/org-ctrl-c-ctrl-c-adv (&rest args)
      "Redisplay buffer.
ORIG-FUNC is called with ARGS."
      :after #'org-ctrl-c-ctrl-c
      (PDEBUG "ENTER2: " (current-buffer))

      (when window-system
        (org-redisplay-inline-images)))


    (advice-add 'org-edit-special :before #'layout-save-current)
    (advice-add 'org-edit-src-exit :after #'layout-restore)

    (defadvice! yc/org-comment-line-break-function-adv (orig-func &rest args)
      "Docs
ORIG-FUNC is called with ARGS."
      :around #'org-comment-line-break-function
      (condition-case var
          (apply orig-func args)
        (error nil)))

    (defadvice! yc/org-publish-file-adv (filename &rest args)
      "Don't publish it if this is gtd file.
ORIG-FUNC is called with ARGS."
      :before-while #'org-publish-file
      (string-match "gtd.org" filename))

    (substitute-key-definition
     'org-cycle-agenda-files  'backward-page org-mode-map)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (ditaa . t)
       (dot . t)
       (plantuml . t)
       (gnuplot . t)
       (shell . t)))
    )

  :bind (:map org-mode-map
              ("" . org-meta-return)
              ("C-c j" . org-meta-return)

              (;;(kbd "M-m")
               [134217837] . yc/show-methods-dwim)

              ("C-x ds" . org-download-screenshot)
              ("C-x du" . org-download-image))

  :bind (([C-f1] . yc/open-gtd))
  )

(use-package tnote
  :commands (tnote
             tnote/dispatch-file tnote/dispatch-directory
             tnote/find-note)
  :bind (:map ctl-x-map
              ("nn" . tnote)
              ("nf" . tnote/find-note)))

(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/Database/org")
  :commands (org-roam-mode org-roam-find-file)
  )

(use-package org-roam-server
  :preface
  (defun yc/turn-on-org-roam ()
    "Turn on org-roam-mode & org-roam-server mode."
    (unless org-roam-server-mode
      (org-roam-mode t)
      (org-roam-server-mode t)))
  :custom
  (org-roam-server-host  "127.0.0.1")
  (org-roam-server-port 9090)
  (org-roam-server-export-inline-images t)
  (org-roam-server-authenticate nil)
  (org-roam-server-network-label-truncate t)
  (org-roam-server-network-label-truncate-length 60)
  (org-roam-server-network-label-wrap-length 20)
  :commands (org-roam-server-mode)
  :hook ((org-mode . yc/turn-on-org-roam))
  )


;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 06-org-mode.el ends here
