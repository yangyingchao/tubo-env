;;; ox-plus.el --- Brief introduction here.-*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(require '01-generics)
(require '02-functions)

 ;; utilities

(defun yc/plantuml-path ()
  "Get path of plantUML."
  (catch 'path-found
    (dolist (path `(,(expand-file-name "~/.local/share/plantuml/lib/plantuml.jar")
                    "/usr/share/plantuml/lib/plantuml.jar"
                    "/usr/local/plantuml/lib/plantuml.jar"
                    "/usr/local/opt/plantuml/libexec/plantuml.jar"))
      (PDEBUG "CHECKING: " path)
      (when (file-exists-p path)
        (throw 'path-found path)))
    ""))

(defun yc/ditaa-path (&optional err-if-not-found)
  "Get path of plantUML."
  (aif (or
        ;; first, try to parse ditaa executable, if exists.
        (when (executable-find "ditaa")
          (let ((content (shell-command-to-string (format "cat %s" (executable-find "ditaa")))))
            (PDEBUG "CONTENT: " content)

            (catch 'p-found
              (dolist (item (s-split " " content))
                (PDEBUG "ITEM: " item)
                (PDEBUG "MATCH: " (string-match (rx "ditaa" (+? nonl) ".jar") item))

                (when (string-match (rx "ditaa" (+? nonl) ".jar") item)
                  (throw 'p-found item)))
              nil)))

        ;; then, try to find in possible locations.
        (catch 'path-found
          (dolist (path `(,(expand-file-name "~/.local/share/ditaa/lib/ditaa.jar")
                          "/usr/share/ditaa/lib/ditaa.jar"
                          "/usr/local/ditaa/lib/ditaa.jar"
                          "/usr/local/opt/ditaa/libexec/ditaa.jar"))
            (PDEBUG "CHECKING: " path)
            (when (file-exists-p path)
              (throw 'path-found path)))
          nil)
        )
      it
    (funcall (if err-if-not-found  #'error #'message) "Failed to find ditaa package!")
    ""))

 ;; publish related
(defcustom my-link-home  "http://localhost"  "Link to homepage."
  :type 'string
  :group 'ox-plus)

(defcustom www-base-dir (expand-file-name "~/Work/yangyingchao.github.io/")
  "Base directory to hold both org files and htdocs."
  :type 'string
  :group 'ox-plus)

(defcustom org-dir (format "%s/org/" www-base-dir) "."
  :type 'string
  :group 'ox-plus)

(defcustom htdoc-dir (format "%s/_posts/" www-base-dir) "."
  :type 'string
  :group 'ox-plus)

(defcustom my-html-head-extra
  "
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/bootstrap.min.css\">
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/style.css\">
<script src=\"/assets/js/jquery-2.1.3.min.js\" type=\"text/javascript\"></script>
<script src=\"/assets/js/org-exported.js\" type=\"text/javascript\"></script>"
  "Extra strings to put into html head."
  :type 'string
  :group 'ox-plus)

(defcustom my-html-head-extra-top
  "
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/bootstrap.min.css\">
<link rel=\"stylesheet\" media=\"all\" href=\"/assets/css/style.css\">
<script src=\"/assets/js/jquery-2.1.3.min.js\" type=\"text/javascript\"></script>
<script src=\"/assets/js/org-exported.js\" type=\"text/javascript\"></script>"
  "Extra strings to put into html head."
  :type 'string
  :group 'ox-plus)


(defun org-compressed-publish-to-html (plist filename pub-dir)
  "Publish an compressed file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (unless (file-directory-p pub-dir)
    (mkdir pub-dir t))

  (PDEBUG "PLIST:" plist)
  (PDEBUG "FILENAME:" filename)
  (PDEBUG "PUBDIR:" pub-dir)

  (let ((base-dir (plist-get plist :base-directory)) )
    (dolist (item (directory-files base-dir t))
      (PDEBUG "ITEM: " item)
      (if (string-match (rx "/" (or "." "..") eol) item)
          (PDEBUG "Skip item:" item)
        (if (file-directory-p item)
            ;; publish directories as attachment..
            (let ((output (expand-file-name (file-name-nondirectory item) pub-dir)))
              (PDEBUG "Publishing DIRECTORY:" item)
              (unless (file-equal-p (expand-file-name (file-name-directory item))
			                        (file-name-as-directory (expand-file-name pub-dir)))
                (copy-directory item output t))
              ;; Return file name.
              output)

          (PDEBUG "Publishing FILE:" item)
          (cond
           ((string-match ".*\.org$" item)
            (org-html-publish-to-html plist item pub-dir))
           ((string-match (rx "." (or "7z" "rar" "gz" "bz2" "tar") eol) item)
            (let ((default-directory pub-dir))
              (shell-command (format "~/.emacs.d/tools/unpack.sh %s" item))))
           (t (error "Unhandled file: %s" item))))))))

(autoload 'tblog/--convert-item "tblog")


(defun org-top-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (when (string-match "sitemap.org" filename)
    (org-html-publish-to-html plist filename pub-dir)))


(defun org-publish-jekyll (plist filename pub-dir)
  "Publish an org file to HTML, with tblog/--convert-item.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.


Return output file name."

  (if nil
      (list plist pub-dir))
  ;; special handling for my github page...
  (unless (string-match-p "yangyingchao.github.io" filename)
    (error "Should be used against github page only: %s" filename))

  (tblog/--convert-item filename t))

(defun add-to-project-list (dir &optional static-only)
  "Description."
  (interactive)
  (PDEBUG "Adding DIR: " dir)
  (when (file-directory-p dir)
    (let ((bdir (file-name-base dir))
          (auto-sitemap (not (file-exists-p (concat dir "/.no-auto-sitemap")))))
      (unless static-only
        (add-to-list 'org-publish-project-alist
                     `(,(format "org-%s" bdir)
                       :base-directory ,dir
                       :base-extension "org"
                       :publishing-directory ,(concat htdoc-dir "/" bdir)
                       :recursive t
                       :exclude ".*gtd.org"
                       :publishing-function org-html-publish-to-html
                       :section-numbers nil
                       :with-toc t
                       :makeindex nil
                       :sitemap-title ,(format "Index For org-%s" bdir)
                       :auto-sitemap ,auto-sitemap
                       :sitemap-filename "index.org"
                       :html-preamble t
                       :html-head-extra ,my-html-head-extra
                       :html-link-up "index.html"
                       :html-link-home ,my-link-home
                       )))

      (add-to-list 'org-publish-project-alist
                   `(,(format "org-%s-static" bdir)
                     :base-directory ,dir
                     :publishing-directory ,(concat htdoc-dir "/" bdir)
                     :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf" "txt"
                                              "gz" "html" "pdf"))
                     :recursive t
                     :publishing-function org-publish-attachment)
                   t))))


(defun yc/fetch-field (field)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp
           (format (rx bol "#+%s:" (group (+? nonl)) eol) field) nil t)
      (s-trim (match-string 1)))))

;; export org to mail..
(defun org-html-export-to-html-mail (&rest args)
  "Export current buffer to a HTML file, with `ARGS'.
Finally, title will be removed."
  (interactive)
  (let ((to (yc/fetch-field "EMAIL_TO"))
        (cc (yc/fetch-field "EMAIL_CC"))
        (title (yc/fetch-field "TITLE"))
        (filename (expand-file-name (apply 'org-html-export-to-html args))))

    (PDEBUG "FILENAME:" filename)
    (with-temp-file filename
      (insert-file-contents filename)
      (goto-char (point-min))
      (if (search-forward-regexp (rx bol "<h1" (+? anything) "/h1>" eol) nil t)
          (delete-region (match-beginning 0) (match-end 0))))

    (aif (or (executable-find "thunderbird-bin")
             (executable-find "thunderbird"))

        (let* (
               (cmd (format "%s -compose to='%s',cc='%s',subject='%s',message=%s"
                           it (or to "") (or cc "") title
                           filename)))

          (PDEBUG "CMD: " cmd)
          (shell-command cmd))

      (if (eq system-type 'darwin)
          (shell-command (format "open -a  Safari \"%s\"" filename))
          (browse-url-generic filename)))))


(defun yc/org-reload-projects ()
  "Reload all projects..."
  (interactive)

  (when (file-exists-p www-base-dir)

    (if (string-match-p "yangyingchao.github.io" www-base-dir)
        ;; special handling for my github page...
        (setq org-publish-project-alist
              `(
                ;; Org top level, to generate index.org
                ("github"
                 :base-directory ,org-dir
                 :base-extension "org"
                 :publishing-directory ,htdoc-dir
                 :recursive t
                 :publishing-function org-publish-jekyll
                 :section-numbers nil
                 :with-toc t
                 :makeindex nil
                 :auto-sitemap nil
                 :sitemap-filename "index.org"
                 :html-preamble t)))

      (setq org-publish-project-alist nil)

      (when (file-directory-p org-dir)
        ;; Org top level, to generate index.org
        (push
         `("org"
          :base-directory ,org-dir
          :base-extension "org"
          :publishing-directory ,htdoc-dir
          :recursive t
          :exclude ".*gtd.org"
          :publishing-function org-top-publish-to-html
          :section-numbers nil
          :with-toc t
          :makeindex nil
          :auto-sitemap t
          :sitemap-title ,(format "Top Index")
          :html-head-extra ,my-html-head-extra-top
          :html-link-home ,my-link-home
          :sitemap-filename "index.org"
          :html-preamble t)
         org-publish-project-alist)

        (push
         ;; Org top level, to export index.org
         `("org-index"
           :base-directory ,org-dir
           :base-extension "org"
           :publishing-directory ,htdoc-dir
           :recursive nil
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :with-toc t
           :makeindex nil
           :auto-sitemap nil
           :html-head-extra ,my-html-head-extra-top
           :html-link-home ,my-link-home
           :sitemap-filename "index.org"
           :html-preamble t)
         org-publish-project-alist))

      (when (file-directory-p (format "%s/assets" org-dir))
        (push
         `("org-assets"
           :base-directory ,(format "%s/assets" org-dir)
           :publishing-directory ,(format "%s/assets" htdoc-dir)
           :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf"))
           :recursive t
           :publishing-function org-publish-attachment)
         org-publish-project-alist))

      (when (file-directory-p (format "%s/images" org-dir))
        (push
         `("org-images"
           :base-directory ,(format "%s/images" org-dir)
           :publishing-directory ,(format "%s/images" htdoc-dir)
           :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf"))
           :recursive t
           :publishing-function org-publish-attachment)
         org-publish-project-alist))

      ;; references
      (when (file-directory-p (format "%s/references" org-dir))
        (push `("org-refs"
                 :base-directory ,(format "%s/references" org-dir)
                 :publishing-directory ,(format "%s/references" htdoc-dir)
                 ;; :base-extension ".*"
                 :recursive nil
                 :publishing-function org-compressed-publish-to-html)
              org-publish-project-alist))

      ;; Slides.
      (when (file-directory-p (format "%s/slides" org-dir))
        (push `("org-slides"
                 :base-directory ,(format "%s/slides" org-dir)
                 :base-extension "org"
                 :publishing-directory ,(format "%s/slides" htdoc-dir)
                 :recursive t
                 :publishing-function org-slides-publish-to-html
                 :section-numbers nil
                 :with-toc nil
                 :makeindex nil
                 :auto-sitemap t
                 :sitemap-filename "index.org"
                 :html-preamble t)
              org-publish-project-alist
              )
        (push `("org-slides-static"
                 :base-directory ,(format "%s/slides" org-dir)
                 :publishing-directory ,(format "%s/slides" htdoc-dir)
                 :base-extension ,(rx (or "css" "js" "png" "jpg" "gif" "swf" "html"))
                 :recursive t
                 :publishing-function org-publish-attachment)
              org-publish-project-alist)))


    ;; add everything in base-dir.
    (dolist (item (directory-files www-base-dir))
      (unless (or (not (file-directory-p (format "%s/%s" www-base-dir item)))
                  (member item '(".DS_Store" "org"  "." "..")))
        (add-to-project-list (concat www-base-dir "/" item) t)))

    (dolist (item (directory-files org-dir))
      (unless (or (not (file-directory-p (format "%s/%s" org-dir item)))
                  (member item '(".git" "slides" "images" "assets" "references" "." "..")))
        (add-to-project-list (concat org-dir "/" item))))))

(defun yc/org-publish-get-base-files (func &rest args)
  "Advice for `org-publish-get-base-files'.
Call FUNC with ARGS."
  (cl-remove-if (lambda (x)
               (string-match-p "sitemap" x))
             (apply func args)))

(use-package ox-publish
  :config
  (require 'ox-html)
  (require 'ox-odt)
  (require 'ox-md)
  (org-export-define-derived-backend
      'mail 'html
    ;; :export-block '("MW" "MEDIAWIKI")
    ;; :filters-alist '((:filter-parse-tree . org-mw-separate-elements))
    :menu-entry
    '(?h "Export to HTML"
         ((?m "As HTML buffer (for email)" org-html-export-to-html-mail))))

  (yc/org-reload-projects)

  (advice-add 'org-publish-get-base-files :around
              #'yc/org-publish-get-base-files))

(defvar-local yc/ox-revealing nil "Nil.")

(defadvice! yc/org-html-export-to-html (orig-func &rest args)
  "Docs
ORIG-FUNC is called with ARGS."
  :around  #'org-html-export-to-html
  (let ((org-html-head-extra
         (cond
          (yc/ox-revealing "")
          (t
           (aif (or (yc/file-exists-p "./style.css" t)
                    (yc/file-exists-p "./css/style.css" t)
                    (yc/file-exists-p "./assets/css/style.css" t))
               (format "<link rel=\"stylesheet\" href=\"%s\"/>" it)
               (concat "<style type=\"text/css\">"
                       (with-temp-buffer
                         (insert-file-contents (expand-file-name "~/.emacs.d/templates/assets/css/style.css"))
                         (catch 'done
                           (while t
                             (goto-char (point-min))
                             (unless (looking-at-p (rx (+? nonl) "\n"))
                               (throw 'done t))
                             (while (search-forward-regexp (rx (* space) "\n" (* space)) nil t)
                               (replace-match " "))))

                         (buffer-substring-no-properties (point-min) (point-max)))
                       " </style>"))
           ))))
    (PDEBUG "ORG-HTML-HEAD-EXTRA:" org-html-head-extra)
    (apply orig-func args)))


(use-package ox-odt
  ;; org v8 bundled with Emacs 24.4
  :custom
  (org-odt-preferred-output-format "doc")
  :config
  (let ((soffice
         (aif (executable-find "soffice")
             it
           (yc/file-exists-p
            "/Applications/LibreOffice.app/Contents/MacOS/soffice"))))
    (when soffice
      (setq org-odt-convert-processes `(("LibreOffice"
                                         ,(concat  soffice " --headless --convert-to %f%x --outdir %d %i" )))))))


(defun org-publish-get-projects-from-filename (filename &optional up)
  "Return the project that FILENAME belongs to.
A separate static project is returned to."
  (let* ((filename (expand-file-name filename))
         project-name)

    (catch 'p-found
      (dolist (prj org-publish-project-alist)
        (unless (plist-get (cdr prj) :components)
          ;; [[info:org:Selecting%20files]] shows how this is supposed to work:
          (let* ((r (plist-get (cdr prj) :recursive))
                 (b (expand-file-name (file-name-as-directory
                                       (plist-get (cdr prj) :base-directory))))
                 (x (or (plist-get (cdr prj) :base-extension) "org"))
                 (e (plist-get (cdr prj) :exclude))
                 (i (plist-get (cdr prj) :include))
                 (xm (concat "^" b (if r ".+" "[^/]+") "\\.\\(" x "\\)$")))
            (when
                (or (and i
                         (member filename
                                 (mapcar (lambda (file)
                                           (expand-file-name file b))
                                         i)))
                    (and (not (and e (string-match e filename)))
                         (string-match xm filename)))
              (setq project-name (car prj))
              (throw 'p-found project-name))))))
    (when up
      (dolist (prj org-publish-project-alist)
        (if (member project-name (plist-get (cdr prj) :components))
            (setq project-name (car prj)))))
    (list (assoc project-name org-publish-project-alist)
          (assoc (concat project-name "-static") org-publish-project-alist))))

(defun yc/org-publish-current-project (func &optional force async)
  "Advice for `org-publish-current-project'.
Call FUNC with ARGS."
  (interactive "P")
  (if nil
      func)
  (save-excursion
    (dolist (project  (org-publish-get-projects-from-filename (buffer-file-name) 'up))
      (when project
        (message "Exporting project: %s" (car project))
        (org-publish project force async)))))

(advice-add 'org-publish-current-project :around #'yc/org-publish-current-project)


;; ox-reveal
(require 'ox-html)
(try-require 'ox-reveal)

(use-package ox-reveal
  :preface
  (defun yc/reveal-update ()
    "Download reveal.js from github."
    (interactive)
    (unless (fboundp 'json-parse-string)
      (error "Function json-parse-string is required"))

    (let* ((default-directory (yc/make-cache-path))
           (json (with-current-buffer
                     (let ((url-http-extra-headers
                            '(("Accept" . "application/vnd.github.v3+json")
                              ("Authorization" . "token 08f653674b2fdfc3135bc873bb224aa7f459e274"))) )
                       (url-retrieve-synchronously
                        "https://api.github.com/repos/hakimel/reveal.js/releases?per_page=1&&page=1"
                        nil nil)
                       )

                   (goto-char (point-min))
                   (set-buffer-multibyte t)

                   (let ((yc/debug-log-limit -1))
                     (PDEBUG "BUF:" (buffer-string)))

                   (when (not (string-match "200 OK" (buffer-string)))
                     (error "Problem connecting to the server"))
                   (re-search-forward "^$" nil 'move)
                   (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                      :object-type 'hash-table
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)))

           (entry (if (listp json) (car json))))
      (unless entry
        (error "Failed to get latest release"))
      (let* ((version (gethash "name" entry))
             (url (gethash "tarball_url" entry))
             (tar (format "reveal.js-%s.tar.gz" version)))

        (when (and (file-exists-p tar)
                   current-prefix-arg)
          (delete-file tar))

        (unless (file-exists-p tar)
          (message "%s --> %s" url tar)
          (url-copy-file url tar t)
          (delete-directory "reveal.js" t))

        (unless (file-directory-p "reveal.js")
          (shell-command (format "mkdir reveal.js && cd reveal.js && tar xzvf ../%s --strip 1" tar))))

      (unless (file-exists-p (yc/make-cache-path "reveal.js/js/reveal.js"))
        (error "failed to update reveal.js"))))

  (defadvice! yc/org-reveal-export-to-html-adv (orig-func &rest args)
    "Docs
ORIG-FUNC is called with ARGS."
    :around  #'org-reveal-export-to-html

    (unless (file-directory-p "reveal.js")
      (unless (file-directory-p (yc/make-cache-path "reveal.js"))
        (yc/reveal-update))
      (copy-directory (yc/make-cache-path "reveal.js") "reveal.js"))

    (let ((yc/ox-revealing t)
          (org-reveal-extra-css
           (or (yc/file-exists-p "./reveal_local.css" t)
               (yc/file-exists-p "./css/reveal_local.css" t)
               (yc/file-exists-p "./assets/css/reveal_local.css" t)
               "")))
      (apply orig-func args)))

  :config
  (custom-set-variables
   `(org-reveal-init-options
     (mapconcat
      'identity
      '(
        ;; Display presentation control arrows
        "controls: true"

         ;; Help the user learn the controls by providing hints, for example by
         ;; bouncing the down arrow when they first encounter a vertical slide
        "controlsTutorial: false"

        ;; ;; Determines where controls appear, "edges" or "bottom-right"
        ;; "controlsLayout": "'bottom-right'",

        ;; ;; Visibility rule for backwards navigation arrows; "faded", "hidden"
        ;; or "visible"
        ;;  controlsBackArrows: 'faded',

        ;; Display a presentation progress bar
        "progress: true"

         ;; Display the page number of the current slide
         ;; - true:    Show slide number
         ;; - false:   Hide slide number

         ;; Can optionally be set as a string that specifies the number formatting:
         ;; - "h.v":   Horizontal . vertical slide number (default)
         ;; - "h/v":   Horizontal / vertical slide number
         ;; - "c":   Flattened slide number
         ;; - "c/t":   Flattened slide number / total slides

         ;; Alternatively, you can provide a function that returns the slide
         ;; number for the current slide. The function should take in a slide
         ;; object and return an array with one string [slideNumber] or
         ;; three strings [n1,delimiter,n2]. See #formatSlideNumber().
        "slideNumber: true"

        ;; Can be used to limit the contexts in which the slide number appears
        ;; - "all":      Always show the slide number
        ;; - "print":    Only when printing to PDF
        ;; - "speaker":  Only in the speaker view
        "showSlideNumber: 'all'"

        ;; Use 1 based indexing for # links to match slide number (default is zero
        ;; based)
        "hashOneBasedIndex: false"

        ;; Add the current slide number to the URL hash so that reloading the
        ;; page/copying the URL will return you to the same slide
        "hash: true"

        ;; Flags if we should monitor the hash and change slides accordingly
        "respondToHashChanges: true"

        ;; Push each slide change to the browser history.  Implies `hash: true`
        "history: false"

        ;; Enable keyboard shortcuts for navigation
        "keyboard: true"

        ;; Optional function that blocks keyboard events when retuning false
        ;;
        ;; If you set this to 'foucsed', we will only capture keyboard events
        ;; for embdedded decks when they are in focus
        "keyboardCondition: null"

        ;; Disables the default reveal.js slide layout (scaling and centering)
        ;; so that you can use custom CSS layout
        "disableLayout: false"

        ;; Enable the slide overview mode
        "overview: true"

        ;; Vertical centering of slides
        "center: true"

        ;; Enables touch navigation on devices with touch input
        "touch: false"

        ;; Loop the presentation
        "loop: false"

        ;; Change the presentation direction to be RTL
        "rtl: false"

        ;; Changes the behavior of our navigation directions.
        "navigationMode: 'default'"

        ;; Randomizes the order of slides each time the presentation loads
        "shuffle: false"

        ;; Turns fragments on and off globally
        "fragments: true"

        ;; Flags whether to include the current fragment in the URL,
        ;; so that reloading brings you to the same fragment position
        "fragmentInURL: true"

        ;; Flags if the presentation is running in an embedded mode,
        ;; i.e. contained within a limited portion of the screen
        "embedded: false"

        ;; Flags if we should show a help overlay when the question-mark
        ;; key is pressed
        "help: true"

        ;; Flags if it should be possible to pause the presentation (blackout)
        "pause: true"

        ;; Flags if speaker notes should be visible to all viewers
        "showNotes: false"

        ;; Global override for autolaying embedded media (video/audio/iframe)
        ;; - null:   Media will only autoplay if data-autoplay is present
        ;; ";; - true:   All media will autoplay" regardless of individual setting
        ;; ";; - false:  No media will autoplay" regardless of individual setting
        "autoPlayMedia: null"

        ;; Global override for preloading lazy-loaded iframes
        ;; - null:   Iframes with data-src AND data-preload will be loaded when within
        ;;           the viewDistance, iframes with only data-src will be loaded when visible
        ;; - true:   All iframes with data-src will be loaded when within the viewDistance
        ;; - false:  All iframes with data-src will be loaded only when visible
        "preloadIframes: null"

        ;; Can be used to globally disable auto-animation
        "autoAnimate: true"

        ;; Optionally provide a custom element matcher that will be
        ;; used to dictate which elements we can animate between.
        "autoAnimateMatcher: null"

        ;; Default settings for our auto-animate transitions, can be
        ;; overridden per-slide or per-element via data arguments
        "autoAnimateEasing: 'ease'"
        "autoAnimateDuration: 1.0"
        "autoAnimateUnmatched: true"

        ;; CSS properties that can be auto-animated. Position & scale
        ;; is matched separately so there's no need to include styles
        ;; like top/right/bottom/left, width/height or margin.
        ;;  autoAnimateStyles: [
        ;;    'opacity',
        ;;    'color',
        ;;    'background-color',
        ;;    'padding',
        ;;    'font-size',
        ;;    'line-height',
        ;;    'letter-spacing',
        ;;    'border-width',
        ;;    'border-color',
        ;;    'border-radius',
        ;;    'outline',
        ;;    'outline-offset'
        ;;  ],

        ;; Controls automatic progression to the next slide
        ;; - 0:      Auto-sliding only happens if the data-autoslide HTML attribute
        ;;           is present on the current slide or fragment
        ;; - 1+:     All slides will progress automatically at the given interval
        ;; ;; - false:  No auto-sliding" even if data-autoslide is present
        "autoSlide: 0"

        ;; Stop auto-sliding after user input
        "autoSlideStoppable: true"

        ;; Use this method for navigation when auto-sliding (defaults to navigateNext)
        "autoSlideMethod: null"

        ;; Specify the average time in seconds that you think you will spend
        ;; presenting each slide. This is used to show a pacing timer in the
        ;; speaker view
        "defaultTiming: null"

        ;; Enable slide navigation via mouse wheel
        "mouseWheel: false"

        ;; Opens links in an iframe preview overlay
        ;; Add `data-preview-link` and `data-preview-link="false"` to customise each link
        ;; individually
        "previewLinks: false"

        ;; Exposes the reveal.js API through window.postMessage
        "postMessage: true"

        ;; Dispatches all reveal.js events to the parent window through postMessage
        "postMessageEvents: false"

        ;; Focuses body when page changes visibility to ensure keyboard shortcuts work
        "focusBodyOnPageVisibilityChange: true"

        ;; Transition style
        "transition: 'slide'" ;; none/fade/slide/convex/concave/zoom

        ;; Transition speed
        "transitionSpeed: 'default'" ;; default/fast/slow

        ;; Transition style for full page slide backgrounds
        "backgroundTransition: 'fade'" ;; none/fade/slide/convex/concave/zoom

        ;; The maximum number of pages a single slide can expand onto when printing
        ;; to PDF, unlimited by default
        "pdfMaxPagesPerSlide: Number.POSITIVE_INFINITY"

        ;; Prints each fragment on a separate slide
        "pdfSeparateFragments: true"

        ;; Offset used to reduce the height of content within exported PDF pages.
        ;; This exists to account for environment differences based on how you
        ;; print to PDF. CLI printing options, like phantomjs and wkpdf, can end
        ;; on precisely the total height of the document whereas in-browser
        ;; printing has to end one pixel before.
        "pdfPageHeightOffset: -1"

        ;; Number of slides away from the current that are visible
        "viewDistance: 3"

        ;; Number of slides away from the current that are visible on mobile
        ;; devices. It is advisable to set this to a lower number than
        ;; viewDistance in order to save resources.
        "mobileViewDistance: 2"

        ;; The display mode that will be used to show slides
        "display: 'block'"

        ;; Hide cursor if inactive
        "hideInactiveCursor: true"

         ;; Time before the cursor is hidden (in ms)
        "hideCursorTime: 5000"
        )
      ", "
      ))
   '(org-reveal-hlevel 2)
   '(org-reveal-plugins   '(classList markdown zoom notes))
   '(org-reveal-default-frag-style "appear")
   )

  )


 ;; org-markdown enhancements.
(defun yc/org-md-template-adv (func contents info)
  "Advice for 'org-md-template'.
Call FUNC which is 'org-md-template with ARGS. Adding GEN-TITLE as mark of
title."
  (if nil func)
  (aif (plist-get info :title)
      (concat "<!-- GEN-TITLE: " (org-export-data it info) " -->\n\n[[_TOC_]]\n" contents)
    contents))
(advice-add 'org-md-template :around #'yc/org-md-template-adv)

;;; markdown, adding language tag if possible...
(defun yc/org-md-example-block-adv (f b c i)
  "Advice for 'org-md-example-block'.
Call FUNC which is 'org-md-example-block with ARGS."

  (if (and (string= (symbol-name (car b)) "src-block")
           (plist-get (cadr b)  :language))
      (concat "```"
              (plist-get (cadr b)  :language) "\n"
              (org-remove-indentation
               (org-export-format-code-default b i))
              "\n```")
    (funcall f b c i)))


(advice-add 'org-md-example-block :around #'yc/org-md-example-block-adv)

(provide 'ox-plus)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ox-plus.el ends here
