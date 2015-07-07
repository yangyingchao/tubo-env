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

(defun yc/ditaa-path ()
  "Get path of plantUML."
  (cond
   ((executable-find "ditaa")
    (progn
      (let ((content (shell-command-to-string (format "cat %s" (executable-find "ditaa")))))
        (PDEBUG "CONTENT: " content)

        (catch 'p-found
          (dolist (item (s-split " " content))
            (PDEBUG "ITEM: " item)
            (PDEBUG "MATCH: " (string-match (rx "ditaa" (+? nonl) ".jar") item))

            (when (string-match (rx "ditaa" (+? nonl) ".jar") item)
              (throw 'p-found item)))
          nil))))
   (t
    (warn "yc/ditaa-package not setup")
    nil))
  (catch 'path-found
    (dolist (path `(,(expand-file-name "~/.local/share/plantuml/lib/plantuml.jar")
                    "/usr/share/plantuml/lib/plantuml.jar"
                    "/usr/local/plantuml/lib/plantuml.jar"
                    "/usr/local/opt/plantuml/libexec/plantuml.jar"))
      (PDEBUG "CHECKING: " path)
      (when (file-exists-p path)
        (throw 'path-found path)))
    ""))


(defun uml/parse-stringfied-nodes (&optional silent)
  "Parse all stringfied nodes.
If `silent' is nil, print collected nodes before exit."
  (interactive)
  (setq uml/stringified-nodes nil)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx bol (* space)
                (or "enum" "class" "interface")
                (+ space)
                (group (+? nonl))
                (* space)
                "{" eol) nil t)
      (add-to-list 'uml/stringified-nodes (match-string-no-properties 1))))
  (unless silent
    (message "Stringfied notes: %s" (s-join ", " uml/stringified-nodes))))


(defun company-plantuml--candidates (prefix)
  "Return candiates for `PREFIX'."
  (all-completions prefix plantuml-kwdList))

;;;###autoload
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
    (org-html-publish-to-html plist filename pub-dir))
  )


(defun org-publish-jekyll (plist filename pub-dir)
  "Publish an org file to HTML, with tblog/--convert-item.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.


Return output file name."

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
  (save-excursion
    (dolist (project  (org-publish-get-projects-from-filename (buffer-file-name) 'up))
      (when project
        (message "Exporting project: %s" (car project))
        (org-publish project force async)))))

(advice-add 'org-publish-current-project :around #'yc/org-publish-current-project)


;; ox-reveal
(require 'ox-html)

(use-package ox-reveal
  :config
  (advice-add 'org-reveal-export-to-html :around #'yc/org-reveal-export-to-html-adv))

(defun yc/org-reveal-export-to-html-adv (func &rest args)
  "Advice for 'org-reveal-export-to-html'.
Call FUNC which is 'org-reveal-export-to-html with ARGS.

Check js/slides.js exist or not, if not exist, re-fetch resource."

  ;; check required resources...
  (let* ((version "3.8.0")
         (package-name "reveal.js")
         (package-name-with-version (format "%s-%s" package-name version))
         (package-full-path (format "%s/%s.tar.gz" yc/emacs-tools-dir
                                    package-name-with-version))
         (final-target-dir (concat default-directory package-name))
         (final-target-file (concat final-target-dir "/js/reveal.js"))

         (basename (format "reveal.js-%s.tar.gz" version))
         (package (concat yc/emacs-tools-dir basename))
         (targetdir (concat default-directory "reveal.js"))
         (targetbase (file-name-directory targetdir)))


    (when (or (not (file-exists-p final-target-file)) ;; target does not exists
              (with-temp-buffer
                (insert-file-contents final-target-file)
                (goto-char (point-min))

                (if (search-forward-regexp
                     (rx "var" (+ space) "VERSION" (+ space) "=" (+ space)
                         "'" (group (+ (or digit "."))) "'" (* space) ";") nil t)
                    (let ((v2 (match-string 1)))
                      (PDEBUG "source version" v2)
                      (PDEBUG "target version" version)
                      (not (string= version v2)))
                  t)))

      ;; clear existing files before further operation.
      (let* ((cmd (format "rm -rf %s; rm -rf %s; tar xzf %s && mv %s %s"
                          package-name-with-version final-target-dir
                          package-full-path package-name-with-version final-target-dir)))
        (PDEBUG "COMMAND: " cmd)
        (shell-command cmd))))

  ;; backup existing file....

  ;; do export

  ;; rename to "FILE-slide.html"

  ;; revert backuped file (if exists...)
  )


 ;; org-markdown enhancements.
(defun yc/org-md-template-adv (func contents info)
  "Advice for 'org-md-template'.
Call FUNC which is 'org-md-template with ARGS. Adding GEN-TITLE as mark of title."
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
