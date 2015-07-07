;;; tblog.el --- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(require 's)
(require 'ivy)

(defgroup tblog nil
  "Helper tool to create/commit posts to github.io.")

(defcustom tblog-repository (expand-file-name "~/Work/yangyingchao.github.io/")
  "Path of blog repository."
  :type 'string
  :group 'tblog)

(defcustom tblog-media-object-suffix-list
  '("jpg" "jpeg" "png" "gif" "mp4" "zip" "gz" "bz2" "tiff")
  "希望处理的媒体文件类型"
  :group 'tblog
  :type 'list)

(defcustom tblog-categories nil
  "Categories of posts."
  :type 'list
  :group 'tblog)

(defcustom tblog-tags nil
  "tags of posts."
  :type 'list
  :group 'tblog)

(defvar tblog-prepare-quiet nil "Nil.")


(defvar tblog--b2p-method-alist
  '((org-mode tblog-b2p-org)
    (nxml-mode tblog-b2p-html)
    (html-mode tblog-b2p-html))
  "Buffer to Post method alist.")

(defvar tblog--buffer-content nil "Content of buffer.")


(defun tblog--spit-string (str &optional SPLIT)
  "Split STR into list."
  (if (stringp str)
      (split-string str (or SPLIT "[ \f\t\n\r\v,]+"))
    (error "Input should be a string.")))

(defun tblog-make-media-object-file-data (media-path)
  "根据给出的文件路径返回相应的FileData，文件不存在返回nil"
  (if (file-exists-p media-path)
      (progn
        (unless
            (string= (expand-file-name media-path)
                     (expand-file-name
                      (format "%s/assets/img/%s" tblog-repository
                              (file-name-nondirectory media-path))))
          (copy-file media-path (format "%s/assets/img/" tblog-repository) t))
        (format "../../../../assets/img/%s"
                (file-name-nondirectory media-path))
        )

    nil))

(defun tblog-replace-media-object-location (buf-str)
  "处理BUF-STR中的媒体文件，返回处理后的字符串"
  (mapc (lambda (suffix)
          (let ((regexp
                 (concat "[file]*[:]?[/\\]*[a-z]?[:]?[^:*\"?<>|#]+."
                         suffix))
                (current 0))
            (while (string-match regexp buf-str current)
              (let* ((media-path (match-string 0 buf-str))
                     (media-url
                      (save-match-data
                        (and (file-exists-p media-path)
                             (tblog-make-media-object-file-data
                              media-path)))))

                (if media-url
                    (progn
                      (setq current
                            (+ (match-beginning 0)
                               (length media-url)))
                      (setq buf-str
                            (replace-match media-url
                                           t
                                           t
                                           buf-str)))
                  (setq current
                        (match-end 0)))))))
        tblog-media-object-suffix-list)

  ;; special handling for hyper links of org files...

  (replace-regexp-in-string
   (rx "<a href=\""
       (group
        (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit) ;; date
        "-"
        (group (+? nonl) ".html" (? "#" (+ nonl)))
        )
       "\">"
       (group (+? anything))
       "</a>"
       )
   (lambda (input)
     (let ((yc/debug-log-limit -1)
           (org (match-string 1 input))
           (url (match-string 2 input))
           (txt (match-string 3 input))
           )
       (PDEBUG "INPUT: " input)
       (format "<a href=\"%s\"> %s </a>"
               url
               (if (string= org txt)
                   url txt))))

   buf-str))

(defun escape-title (title)
  "Replace characters in `TITLE'."
  (let ((maps '((" " . "-")
                ))
        (result (s-trim title)))
    (s-replace-all maps result)))

;;;###autoload
(defun tblog/new-post ()
  "Start a new post."
  (interactive)
  (let* ((name (expand-file-name
                (format "%s/org/%s-%s.org"
                        tblog-repository
                        (format-time-string  "%Y-%m-%d" (current-time))
                        (escape-title (completing-read "Title: " '("Unmaed"))))))
         )
    (find-file name)))

;;;###autoload
(defun tblog/find-file ()
  "Open existing file."
  (interactive)
  (let* ((org-dir (format "%s/org/" tblog-repository))
         (file
          (ivy-read "Find file: "
                    (mapcar 'file-name-nondirectory
                            (directory-files org-dir t ".*\\.org"))
                   :caller 'tblog)
         ))

    (when file
      (find-file (concat org-dir file)))))

(defvar blog-escape-alist
  '(
    (?: . "&#58;")
    (?< . "&#60;")
    (?= . "&#61;")
    (?> . "&#62;")
    (?  . "&#32;")
    )
  "escape table")

(defun tblog-escape-html-characters (input)
  "Escape special characters in INPUT."
  (when (stringp input)
    (apply 'concat (mapcar (lambda (x)
                             (let ((kv (assoc x blog-escape-alist)))
                               (if kv (cdr kv) (string x))))
                           input))))

(defun tblog--fetch-field (regex &optional SPLIT)
  "Fetch all fields, and return a concatenated string.
REGEX: format used by regex.
FUNC: function to be called."
  (save-excursion
    (goto-char (point-min))
    (let ((result (if (string-match regex tblog--buffer-content)
                      (match-string 1 tblog--buffer-content))))
      (if (string-match "(nil)" result)
          (setq result nil))
      (when (and SPLIT result)
        (print (string-split result SPLIT))
        (setq result (mapconcat 'identity (string-split result SPLIT) " ")))
      result)))

(defun tblog-b2p-html ()
  (cons (list
         ;; title
         (cons "title"
               (or (car (tblog--fetch-field (rx (or "<title>" "<TITLE>") (group (+? anything))
                                             (or "</title>" "</TITLE>"))))
                   "Unamed"))

         ;; categories
         (cons "categories"
               (let ((categories-list
                      (tblog-categories-string-to-list
                       (car (tblog--fetch-field "CATEGORIES")))))
                 (or
                  categories-list
                  '("Copies"))))

         ;; tags
         (cons "mt_keywords"
               (or
                (tblog--fetch-field "KEYWORDS")
                ""))

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (tblog-replace-media-object-location
                (buffer-substring-no-properties
                 (tblog-point-template-head-end)
                 (point-max)))))))

(defun tblog-b2p-other ()
  (delq nil
        (list
         ;; title
         (cons "title" (buffer-name))

         ;; categories
         (cons "categories" "Unknown")

         ;; tags
         (cons "keywords" "")

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (let* ((bf (htmlize-buffer))
                      (content (with-current-buffer bf
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                 (kill-buffer bf)
                 content)))))

(defun tblog--current-buffer-to-post ()
  (set (make-variable-buffer-local 'tblog--buffer-content)
        (buffer-substring-no-properties (point-min) (point-max)))
  (let ((func (cadr (assoc major-mode tblog--b2p-method-alist))))
    (if func
        (funcall func)
      (tblog-b2p-other))))

(defmacro tblog-mkfield (x)
  `(rx bol ,x (* space) (group (+? nonl) eol)))

(defun tblog--format-category (cat)
  "Format CAT properly."
  (if cat
      (mapconcat (lambda (x) (capitalize x))
                 (tblog--spit-string cat "-")
                 "-")
    nil))

(defun tblog--get-category ()
  "Update category.
If CAT is specified, add it to database and return it unchanged.
Or, select/create one from DB and return a new one."
  (tblog--get-category-list t)
  (let* ((cat (or (tblog--choose-or-create "Category" tblog-categories) "未分类"))
         (cat-list (tblog--spit-string cat)))
    (when (> (length cat-list) 1)
      (message "Warning: multiple category detected, will use the 1st one.")
      (setq cat (car cat-list)))

    (setq cat (tblog--format-category cat))

    ;; First character of Category should be capitalized.
    (add-to-list 'tblog-categories cat)
    (tblog--db-write (tblog--get-category-db-path) tblog-categories)
    cat))

(defun tblog--format-tag (tag)
  "Format TAG properly."
  (mapconcat (lambda (x) (capitalize x))
             (tblog--spit-string cat "-")
             "-"))

(defun tblog--get-tag ()
  "Update tags.
If TAG is specified, add it to database and return it unchanged.
Or, select/create one from DB and return a new one."
  (tblog--get-tag-list t)
  (let* ((tags-str (tblog--choose-or-create "Tag" tblog-tags))
         (tags (split-string tags-str "[ \f\t\n\r\v,]+"))
         ret)
    (dolist (tag tags)
      (let ((ltag (downcase tag)))
        (add-to-list 'tblog-tags ltag)
        (add-to-list 'ret ltag t)))
    (tblog--db-write (tblog--get-tag-db-path) tblog-tags)
    (mapconcat 'identity ret " ")))

(defun tblog--choose-or-create (name candidates)
  "Choose one candidate for NAMAE from CANDIDATES for create a new one."
  (interactive)
  (ivy-read (format "Choose or create %s: " name) candidates
            ))

(defun tblog-b2p-org ()
  "Post org-buffer to blog."
  (interactive)
  (setq tblog--buffer-content
        (buffer-substring-no-properties (point-min) (point-max)))

  (save-excursion
    (let ((category    (tblog--fetch-field (tblog-mkfield "#+CATEGORY:")))
          (tag         (tblog--fetch-field (tblog-mkfield "#+KEYWORDS:")
                                           "[ \f\t\n\r\v,]+"))
          (description (tblog--fetch-field (tblog-mkfield "#+DESCRIPTION:")))
          (title       (s-replace-all
                        '((":" . "--"))
                         (tblog--fetch-field (tblog-mkfield  "#+TITLE:")))
   ))

      ;; Check Category
      (if (and (not category)
               (not tblog-prepare-quiet)
               (yes-or-no-p "Category is not specified, add one?"))
          (setq category (tblog--get-category)))

      (setq category (or (tblog--format-category category)  "未分类"))
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp (tblog-mkfield "#+CATEGORY:") nil t)
        (replace-match (format "#+CATEGORY: %s" category)))

      ;; Check Tag
      (if (and (not tag)
               (not tblog-prepare-quiet)
               (yes-or-no-p "Tags are not specified, add them?"))
        (setq tag (tblog--get-tag)))
      (when tag
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp (tblog-mkfield "#+KEYWORDS:") nil t)
          (replace-match (format "#+KEYWORDS: %s" tag))))

      (save-buffer)

      (cons (list
             (cons "title"  title)
             (cons "description" description)
             (cons "categories" category)
             (cons "tags" tag))

            ;; Contents.
            (with-current-buffer
                (let ((org-html-head-extra nil))
                  (org-export-to-buffer 'html "*Org HTML Export*" nil nil t t))
              (let ((buf-str
                     (tblog-replace-media-object-location
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max)))))
                (kill-buffer)
                buf-str))))))

(defun tblog-get-post-name (bfn)
  "BFN: base file name."
  (concat (if (string-match
               (rx bol
                   (repeat 4 digit) "-"   ;; year
                   (repeat 1 2 digit) "-" ;; month
                   (repeat 1 2 digit)     ;; day
                   "-")  bfn)
              bfn
            (concat
             (format-time-string  "%Y-%m-%d" (current-time)) "-" bfn))
          ".html"))

(defun tblog/--convert-buffer (&optional skip)
  "Convert current buffer into html file and prepare send to github.
SKIP: skip running magit."
  (let* ((fn (buffer-file-name))
         (ofn (file-name-nondirectory fn))
         (bfn (file-name-sans-extension ofn))
         (hfn (tblog-get-post-name bfn))
         (dir (file-name-directory fn))
         (content (tblog--current-buffer-to-post))
         (final-content ))
    (with-temp-file (format "%s/_posts/%s" tblog-repository hfn)
      (goto-char (point-min))
      (insert "---
layout : post
")
      (dolist (item (car content))
        (if (and (car item) (cdr item))
            (insert (concat (car item) " : " (cdr item) "\n"))))
      (insert (concat  "---\n" (cdr content))))

    (message "Post prepared %s..！" buffer-file-name)))

(defun tblog/--convert-item (item &rest quiet)
  "Convert single `ITEM'."
  (PDEBUG "CONV: " item)

  (find-file item)
  (condition-case var
      (if quiet
          (let ((tblog-prepare-quiet (car quiet)) )
            (tblog/--convert-buffer t))
        (tblog/--convert-buffer t))

    (error "AAA: %S" var))
  (kill-buffer))

;;;###autoload
(defun tblog/export-all ()
  "Export all posts...."
  (interactive)
  (let ((tblog-prepare-quiet t))
    (mapc #'tblog/--convert-item (directory-files (concat tblog-repository "/org/" ) t
                                                ".*\.org"))))

;;;###autoload
(defun tblog/export (&optional arg)
  "Export any new files and modified files."
  (interactive "P")
  (if (or arg
          (not buffer-file-name))
      (let* ((tblog-org-dir (concat tblog-repository "/org/" ))
             (default-directory tblog-org-dir)
             (result (shell-command-to-string "git status . -s"))
             (r-match-entry (rx (? space) (or "M" "??") (+ space)
                                (group (+ nonl))))
             (tblog-prepare-quiet t)
             (pos 0)
             file-list )
        (PDEBUG "RESULT: " result)

        (while (string-match r-match-entry result pos)
          (add-to-list 'file-list (concat tblog-org-dir (match-string 1 result)))
          (setq pos (1+ (match-end 0))))

        (PDEBUG "FLIST: " file-list)
        (mapc #'tblog/--convert-item file-list))

    (tblog/--convert-buffer)))


;; Operations related to categories and tags

(defun tblog--get-category-db-path ()
  "Return path of category database."
  (concat tblog-repository "/category.db"))

(defun tblog--get-tag-db-path ()
  "Return path of tag database."
  (concat tblog-repository "/tags.db"))

(defun tblog--get-category-list (&optional force)
  "Get a list of categories.
It only read files from database when FORCE is t or tblog-categories is nil."
  (when (or force (not tblog-categories))
    (setq tblog-categories (tblog--read-db-as-list (tblog--get-category-db-path))))
  tblog-categories)

(defun tblog--get-tag-list (&optional force)
  "Get a list of tags.
It only read files from database when FORCE is t or tblog-tags is nil."
  (when (or force (not tblog-tags))
    (setq tblog-tags (tblog--read-db-as-list (tblog--get-tag-db-path))))
  tblog-tags)

(defun tblog--read-db-as-list (path)
  "Read and return content of file (specified as PATH), and return as list."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun tblog--db-write (path content)
  "Write CONTENT into file specified as PATH."
  (with-temp-file path
    (insert "(")
    (dolist (i (sort (copy-list content) 'string-lessp)) (pp i (current-buffer)))
    (insert ")\n")))

(provide 'tblog)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; tblog.el ends here
