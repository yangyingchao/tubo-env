;;; tnote.el --- A synchronized, Org-mode, document annotator -*- lexical-binding: t; -*-

;; Copyright (C) 2020, yyc

;;; Commentary:
;;
;; Inspired by interleave & org-noter.

;;; Code:

 ;; required packages.
(require 'use-package)
(require '02-functions)
(require '03-fundamental-mode)

(require 'org)
(require 'org-element)

(require 'cl-lib)
(require 'doc-view)
(require 'image-mode)

(require 'ivy)
(require 'ivy-rich)
(require 'counsel-utils)

(require 's)

(autoload 'ffap-url-p "ffap" ""  nil)
(autoload 'eww-current-url "eww" "get current url."  nil)
(autoload 'dired-get-marked-files "dired" ""  nil)


(declare-function doc-view-goto-page "doc-view")
(declare-function image-display-size "image-mode")
(declare-function image-get-display-property "image-mode")
(declare-function image-mode-window-get "image-mode")
(declare-function image-scroll-up "image-mode")
(declare-function nov-render-document "ext:nov")
(declare-function org-attach-dir "org-attach")
(declare-function org-attach-file-list "org-attach")
(declare-function pdf-info-getannots "ext:pdf-info")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-outline "ext:pdf-info")
(declare-function pdf-info-pagelinks "ext:pdf-info")
(declare-function pdf-util-tooltip-arrow "ext:pdf-util")
(declare-function pdf-view-active-region "ext:pdf-view")
(declare-function pdf-view-active-region-p "ext:pdf-view")
(declare-function pdf-view-active-region-text "ext:pdf-view")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")
(defvar nov-documents-index)
(defvar nov-file-name)

 ;; user variables.
(defgroup tnote nil
  "A synchronized, external annotator"
  :group 'convenience
  :version "25.3.1")

(defcustom tnote/root-directory   (expand-file-name "~/Documents/Database")
  "Root directory of my database."
  :type 'String
  :group 'tnote)


(defcustom tnote/property-doc-file "NOTER_DOCUMENT"
  "Name of the property that specifies the document."
  :group 'tnote
  :type 'string)

(defcustom tnote/property-note-location "NOTER_PAGE"
  "Name of the property that specifies the location of the current note.
The default value is still NOTER_PAGE for backwards compatibility."
  :group 'tnote
  :type 'string)


(defcustom tnote/doc-split-fraction '(0.6 . 0.4)
  "Fraction of the frame that the document window will occupy when split.
This is a cons of the type (HORIZONTAL-FRACTION . VERTICAL-FRACTION)."
  :group 'tnote
  :type '(cons (number :tag "Horizontal fraction") (number :tag "Vertical fraction")))

(defcustom tnote/auto-save-last-location nil
  "When non-nil, save the last visited location automatically; when starting a new session, go to that location."
  :group 'tnote
  :type 'boolean)

(defcustom tnote/hide-other nil
  "When non-nil, hide all headings not related to the command used.
For example, when scrolling to pages with notes, collapse all the
notes that are not annotating the current page."
  :group 'tnote
  :type 'boolean)

(defcustom tnote/always-create-frame t
  "When non-nil, tnote will always create a new frame for the session.
When nil, it will use the selected frame if it does not belong to any other session."
  :group 'tnote
  :type 'boolean)


(defcustom tnote/insert-selected-text-inside-note t
  "When non-nil, it will automatically append the selected text into an existing note."
  :group 'tnote
  :type 'boolean)

(defcustom tnote/closest-tipping-point 0.3
  "Defines when to show the closest previous note.

Let x be (this value)*100. The following schematic represents the
view (eg. a page of a PDF):

+----+
|    | -> If there are notes in here, the closest previous note is not shown
+----+--> Tipping point, at x% of the view
|    | -> When _all_ notes are in here, below the tipping point, the closest
|    |    previous note will be shown.
+----+

When this value is negative, disable this feature.

This setting may be overridden in a document with the function
`tnote/set-closest-tipping-point', which see."
  :group 'tnote
  :type 'number)

(defcustom tnote/default-notes-file-names '("Inbox.org")
  "List of possible names for the default notes file, in increasing order of priority."
  :group 'tnote
  :type 'string)

(cdsq tnote/recursive-ignore-dir-regexp
  (rx  (or "." ".." "images") string-end)
  "Regular expression for subdirectories to be ignored.
This variable is only effective when searching for files
recursively, that is, when `tnote/recursive' is non-nil.")

(defcustom tnote/ignore-file-regexp
  (concat "\\(?:"
          "^$"
          "\\)")
  "Regular expression for files to be ignored."
  :type 'regexp
  :safe 'stringp
  :group 'tnote)

(defface tnote/time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for tnote last modified times."
  :group 'tnote)

 ;; Functions
(defun tnote/get-notes-dir ()
  "Returns note directory.."
  (concat tnote/root-directory "/org"))

(defun tnote/find-files (dir)
  "Return a list of all files in the directory DIR."
  (PDEBUG "Listing files in dir: " dir)
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            result)
        (dolist (file files)
          (cond
           ;; Recurse into subdirectory if `tnote/recursive' is non-nil
           ;; and the directory is not ".", "..", or `tnote/archive-directory'.
           ((file-directory-p file)
            (when (not (string-match tnote/recursive-ignore-dir-regexp file))
              (setq result (append (tnote/find-files file) result))))

           ;; Collect names of readable files ending in `tnote/extension'
           ((and (file-readable-p file)
                 (not (string-match (rx (or "/." (: bol eol))) file))
                 (not (backup-file-name-p file)))
            (setq result (cons file result)))))
        result)))


(defun tnote/path-to-title (file)
  "Get title from FILE."
  (let* ((content
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (buffer-substring (point-min) (point-at-eol)))))
    (PDEBUG "FILE:" file "CONTENT: " content)
    ;; returns either parsed title, or base name.
    (or (s-trim (replace-regexp-in-string
                         (rx bol (* space)
                             (or
                              (+ "%") ;; line beg with %
                              "#+TITLE:" ;; org-mode title
                              (+ (or "#" "*" space)) ;; line beg with #, * and/or space
                              "Title:" ;; MultiMarkdown metadata
                              )
                             (* space))
                         "" content))
        (file-name-base file))))

(defun tnote/title-to-path (title)
  "Convert TITLE into path of new file to be created."

  (concat (file-name-as-directory (tnote/get-notes-dir))
          (downcase
           (replace-regexp-in-string "\/" "-"
                                     (replace-regexp-in-string " " "-" title)))
          ".org"))

(defun tnote/search-via-tags (&optional x)
  "Open org-agenda view, X is ignored."
  (interactive)
  (PDEBUG "X: " x)
  (org-tags-view))

(yc/eval-after-load
  "ivy-rich"
  (plist-put ivy-rich-display-transformers-list 'tnote/find-note
           '(:columns
             ((tnote/path-to-title (:width 0.8))
              (ivy-rich-file-last-modified-time (:face
                                                 font-lock-comment-face)))))

  (ivy-rich-mode -1)
  (ivy-rich-mode 1))

(yc/eval-after-load
  'ivy
  (ivy-add-actions
   'tnote/find-note
   (append  '(("g" counsel-grep-in-dir "Grep in current directory")
              ("t" tnote/search-via-tags "Find via tags"))
            yc/ivy-common-actions)))

(defun tnote/find-or-create-file (file)
  "Find or create new FILE."
  (if (file-exists-p file)
      (find-file file)
    (let* ((file (s-trim file))  ;; remove leading/trailing white spaces.
           (title file)          ;; as title
           (file (tnote/title-to-path title))) ;; real file:
      (PDEBUG "Creating new file" file "with title: " title)

      (find-file file)
      (goto-char (point-min))
      (unless (looking-at-p "^#\\+TITLE:.*$")
        (error "Wrong format, first line should begin with \"#+TITLE:\""))
      (kill-line)
      (insert "#+TITLE: " title)
      (goto-char (point-max))
      (insert "\n")
      (save-buffer)))
  (buffer-file-name))

(defun tnote/find-note-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES."
  (let ((yc/debug-log-limit -1))
    (PDEBUG "REGEXP:" regexp
            "CANDS:" candidates
            "IVY-TEXT:" ivy-text))


  (unless (stringp regexp)
    (error "Regex should be a string"))

  (let (res)
    (dolist (fn candidates)
      (when (or (string-match-p regexp fn)
                (string-match-p regexp (tnote/path-to-title fn)))
        (push fn res)))

    (let ((yc/debug-log-limit -1))
      (PDEBUG "RES:" res))

    (nreverse res)))

;;;###autoload
(defun tnote/find-note ()
  "Find note file.."

  ;; nothing is cached for now, using a hash table may help improving performance.
  (interactive)
  (let ((default-directory (tnote/get-notes-dir))
        (files (tnote/find-files (tnote/get-notes-dir))))
    (ivy-read "Find note: " files
              :action 'tnote/find-or-create-file
              :matcher #'tnote/find-note-matcher
              :caller 'tnote/find-note)))

(defun tnote/do-dispatch-file (item)
  "Dispatch single ITEM, returns file name after dispatch."
  (interactive)
  (unless (file-exists-p item)
    (error "File: %s not accessible" item))

  (let* ((checksum (shell-command-to-string (format "md5sum '%s'" item)))
         (target-dir (concat tnote/root-directory "/"
                             (downcase (file-name-extension item)) "/"
                             (substring checksum 0 1)))
         (note-file (tnote/get-note-file item))
         (target-file (expand-file-name (file-name-nondirectory item) target-dir))
         )

    (unless (directory-name-p target-dir)
      (make-directory target-dir t))

    (PDEBUG
      "FILE: " item
      "\nCHECKSUM: " checksum
      "\nNOTE:" note-file)

    (shell-command (format "mv \"%s\" %s/" item target-dir))

    (setq target-file (tnote/get-relative-name target-file))

    (if (file-exists-p note-file)
        (with-temp-file note-file
          (insert-file-contents note-file)
          (save-excursion
            (goto-char (point-min))
            (when (search-forward-regexp
                   (format (rx bol "#+%s: " (group (+? nonl)) eol) tnote/property-doc-file) nil t)
                (replace-match (concat "#+" tnote/property-doc-file ": " target-file))))

          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp
                    (format (rx bol ":%s:" (+ space) (group (+? nonl) "%s") eol)
                            tnote/property-doc-file
                            (file-name-nondirectory target-file))
                     nil t)
              (replace-match (concat ":" tnote/property-doc-file ": " target-file)))))
      (aif (get-buffer (file-name-nondirectory note-file))
          (with-temp-buffer it
                            (save-excursion
                              (find-file (buffer-file-name))))))

    (message "%s --> %s" item target-dir)
    target-dir))

(defun tnote/dispatch-file ()
  "Dispatch single ITEM.
If this file is currently be visited, reopen it after dispatching."
  (interactive)
  (cond
   (buffer-file-name
    (let ((filename buffer-file-name))
    (kill-buffer)
    (find-file  (tnote/do-dispatch-file filename))))

   ((equal major-mode 'dired-mode)
    (mapc 'tnote/do-dispatch-file (dired-get-marked-files))
    (revert-buffer))

   (t (error "Not handled: %S" major-mode))))


(defun tnote/dispatch-directory (&optional directory)
  "Dispatch all files in DIRECTORY."
  (interactive)
  (let ((directory (or directory default-directory)))
    (unless (file-directory-p directory)
      (error "Directory %s not accessible" directory))

    (mapc 'tnote/do-dispatch-file (directory-files-recursively directory ".*"))))

(defun tnote/get-note-file (input)
  "Return note file for INPUT."
  (let ((pdf-file-name input)
        (org-file-create-dir (expand-file-name "org" tnote/root-directory)))

    (unless (file-directory-p org-file-create-dir)
      (make-directory org-file-create-dir t))

    (expand-file-name (concat (file-name-base pdf-file-name) ".org")
                      org-file-create-dir)))


(defun tnote/doc-approx-location ()
  "Return current location."
  (cond
   ((memq major-mode '(doc-view-mode pdf-view-mode))
    (image-mode-window-get 'page))

   ((eq major-mode 'nov-mode)
    nov-documents-index
    )

   (t (point))))

(defun tnote/check-if-document-is-annotated-on-file (document-path notes-path)
  "Check if doc file (DOCUMENT-PATH) is annotated on note file (NOTES-PATH)."
  (let ((buffer (find-buffer-visiting notes-path)))
    (when buffer (with-current-buffer buffer (save-buffer)))

    (with-temp-buffer
      (insert-file-contents notes-path)
      (catch 'break
        (while (re-search-forward (org-re-property tnote/property-doc-file) nil t)
          (when (file-equal-p (expand-file-name (match-string 3) (file-name-directory notes-path))
                              document-path)
            ;; NOTE(nox): This notes file has the document we want!
            (throw 'break t)))))))

(defun tnote/kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (derived-mode-p 'doc-view-mode)
    (doc-view-kill-proc))
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (kill-buffer (current-buffer))))

(defvar tnote/org-buffer nil
  "Org notes buffer name.")

(defvar tnote/doc-buffer nil
  "Name of PDF buffer associated with `tnote/org-buffer'.")


;;;###autoload
(defvar tnote/pdf-previous-page-fn #'doc-view-previous-page
  "Function to call to display the previous page.")

;;;###autoload
(defun tnote/goto-doc-page (page)
  "Goto PAGE of document.."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)  (pdf-view-goto-page page))
   ((eq major-mode 'doc-view-mode)  (doc-view-goto-page page))

   (t
    (when (>= (point-max) page)
      (forward-char (- page (point)))
      (recenter)))))


;;;###autoload

(defvar tnote/pdf-scroll-up-or-next-page-fn #'doc-view-scroll-up-or-next-page
  "Function to call for line/page scrolling in upward direction." )

;;;###autoload

(defvar tnote/pdf-scroll-down-or-previous-page-fn #'doc-view-scroll-down-or-previous-page
  "Function to call for line/page scrolling in downward direction.")

(defcustom tnote/sort-order 'asc
  "Specifiy the notes' sort order in the notes buffer.

The possible values are 'asc for ascending and 'desc for descending."
  :type '(choice (const  asc)
                 (const  desc))
  :group 'tnote)

(defcustom tnote/split-direction 'vertical
  "Specify how to split the notes buffer."
  :group 'tnote
  :type '(choice (const vertical)
                 (const horizontal)))

(defcustom tnote/split-lines nil
  "Specify the number of lines the PDF buffer should be increased or decreased.

If nil both buffers are split equally.  If the number is positive,
the window is enlarged.  If the number is negative, the window is
shrunken.

If `tnote/split-direction' is 'vertical then the number is
taken as columns."
  :group 'tnote
  :type '(choice integer
                 (const nil)))
;;; suppress "functions are not known to be defined" warnings
(declare-function pdf-view-next-page "pdf-view.el")
(declare-function pdf-view-previous-page "pdf-view.el")
(declare-function pdf-view-goto-page "pdf-view.el")
(declare-function pdf-view-scroll-up-or-next-page "pdf-view.el")
(declare-function pdf-view-scroll-down-or-previous-page "pdf-view.el")

(make-variable-buffer-local
 (defvar tnote/page-marker 0
   "Caches the current page while scrolling"))

(defun tnote/-find-pdf-path (buffer)
  "Search the `tnote_pdf' property in BUFFER and extracts it when found."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (format "^#\\+%s: \\(.*\\)" tnote/property-doc-file) nil :noerror)
          (match-string 1))))))

(defun tnote/-headline-doc-path (buffer)
  "Return the NOTER_DOCUMENT property of the current headline in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((headline (org-element-at-point)))
        (PDEBUG
         "CONTENT: " headline
         "HEADLINE: " (org-element-type headline)
         "FF:" (org-entry-get nil tnote/property-doc-file t))

        (org-entry-get nil tnote/property-doc-file t)))))

(defun tnote/-open-file (split-window)
  "Opens the pdf file in besides the notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let* ((buf (current-buffer))
         (doc-file-name
          (or (tnote/-headline-doc-path buf)
              (tnote/-find-pdf-path buf))))
    (PDEBUG "CURRNET-BUFF:" buf
            "POINT: " (point)
            "HEADLINE-PDF: " (tnote/-headline-doc-path buf)
            "FIND-PDF:" (tnote/-find-pdf-path buf)
            )
    (unless doc-file-name
      (setq doc-file-name
            (read-file-name (format "No %s property found. Please specify path: "
                                    tnote/property-doc-file) nil nil t))

      ;; Check whether we have any entry at point with `org-entry-properties' before
      ;; prompting if the user wants multi-pdf.
      (if (and (org-entry-properties) (y-or-n-p "Is this multi-pdf? "))
          (org-entry-put (point) tnote/property-doc-file (tnote/get-relative-name doc-file-name))
        (save-excursion
          (goto-char (point-min))
          (insert "#+" tnote/property-doc-file ": " (tnote/get-relative-name doc-file-name)))))

    (delete-other-windows)
    (funcall split-window)
    (when (integerp tnote/split-lines)
      (if (eql tnote/split-direction 'horizontal)
          (enlarge-window tnote/split-lines)
        (enlarge-window-horizontally tnote/split-lines)))

    (aif (ffap-url-p doc-file-name)
        (eww it)
        (find-file (expand-file-name doc-file-name)))

    (tnote/doc-mode 1)
    doc-file-name))

(defun tnote/-goto-parent-headline (property)
  "Traverse the tree until the parent headline.

Consider a headline with property PROPERTY as parent headline."
  (catch 'done
    (if (and (eql (org-element-type (org-element-at-point)) 'headline)
             (org-entry-get (point) property))
        (org-element-at-point)
      (condition-case nil
          (org-up-element)
        ('error
         (throw 'done nil)))
      (tnote/-goto-parent-headline property))))

(defun tnote/-goto-search-position ()
  "Move point to the search start position.

For multi-pdf notes this is the outermost parent headline.  For everything else
this is the beginning of the buffer."
  (tnote/-goto-parent-headline tnote/property-doc-file)
  )

(defun tnote/-go-to-page-note (page)
  "Look up the notes for the current pdf PAGE.

Effectively resolves the headline with the tnote_page_note
property set to PAGE and returns the point.
"
  (PDEBUG "PAGE:" page)
  (with-current-buffer tnote/org-buffer
    (let (point
          (window (get-buffer-window (current-buffer) 'visible)))
      (save-excursion
        (widen)
        (tnote/-goto-search-position)
        (when (re-search-forward (format "^\[ \t\r\]*\:noter_page\: %s$"
                                         page)
                                 nil t)
          ;; widen the buffer again for the case it is narrowed from
          ;; multi-pdf notes search. Kinda ugly I know. Maybe a macro helps?
          (widen)
          (org-back-to-heading t)
          (org-show-subtree)
          (org-cycle-hide-drawers t)
          (setq point (point))))
      ;; When narrowing is disabled, and the notes/org buffer is
      ;; visible recenter to the current headline. So even if not
      ;; narrowed the notes buffer scrolls allong with the PDF.
      (when (and point window)
        (with-selected-window window
          (goto-char point)
          (recenter)))
      point)))

(defun tnote/doc-next-page ()
  "Go to next page of document."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-next-page))
   ((eq major-mode 'doc-view-mode)
    (doc-view-next-page))
   (t
    (error "Error: next-page is not implemented for mode %s"
           (symbol-name major-mode)))))

(defun tnote/doc-previous-page ()
  "Go to next page of document."
  (interactive)
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-previous-page))
   ((eq major-mode 'doc-view-mode)
    (doc-view-previous-page))
   (t
    (error "Error: previous-page is not implemented for mode %s"
           (symbol-name major-mode)))))

(defun tnote/go-to-next-page ()
  "Go to the next page in PDF.  Look up for available notes."
  (interactive)
  (tnote/doc-next-page)
  (tnote/-go-to-page-note (tnote/doc-approx-location)))

(defun tnote/go-to-previous-page ()
  "Go to the previous page in PDF.  Look up for available notes."
  (interactive)
  (tnote/doc-previous-page)
  (tnote/-go-to-page-note (tnote/doc-approx-location)))

(defun tnote/scroll-up ()
  "Scroll up the PDF.  Look up for available notes."
  (interactive)
  (setq tnote/page-marker (tnote/doc-approx-location))
  (funcall tnote/pdf-scroll-up-or-next-page-fn)
  (unless (= tnote/page-marker (tnote/doc-approx-location))
    (tnote/-go-to-page-note (tnote/doc-approx-location))))

(defun tnote/scroll-down ()
  "Scroll down the PDF.  Look up for available notes."
  (interactive)
  (setq tnote/page-marker (tnote/doc-approx-location))
  (funcall tnote/pdf-scroll-down-or-previous-page-fn)
  (unless (= tnote/page-marker (tnote/doc-approx-location))
    (tnote/-go-to-page-note (tnote/doc-approx-location))))

(defun tnote/-switch-to-org-buffer (&optional insert-newline-maybe position)
  "Switch to the notes buffer.

Inserts a newline into the notes buffer if INSERT-NEWLINE-MAYBE
is non-nil.
If POSITION is non-nil move point to it."

  (PDEBUG "CURRENT-BUFF:" (current-buffer)
          "EQ" (eq (buffer-name) tnote/org-buffer))
  (if (eq (buffer-name) tnote/org-buffer)
      (switch-to-buffer tnote/org-buffer)
    (switch-to-buffer-other-window tnote/org-buffer)
    )

  (when (integerp position)
    (goto-char position))
  (when insert-newline-maybe
    (save-restriction
      (tnote/-goto-insert-position))
    ;; Expand again. Sometimes the new content is outside the narrowed
    ;; region.
    (org-show-subtree)
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *" (line-beginning-position)))
      (org-return))
    ))

(defun tnote/-switch-to-doc-buffer ()
  "Switch to the pdf buffer."
  (switch-to-buffer-other-window tnote/doc-buffer)
  ;; (if (derived-mode-p 'org-mode)
  ;;     (switch-to-buffer-other-window tnote/doc-buffer)
  ;;   (switch-to-buffer tnote/doc-buffer))
  )

(defun tnote/-goto-insert-position ()
  "Move the point to the right insert postion.

For multi-pdf notes this is the end of the subtree.  For everything else
this is the end of the buffer"
  (prog1
        (tnote/-goto-parent-headline tnote/property-doc-file)
      (org-end-of-subtree)))

(defun tnote/-create-new-note (document page &optional selected-text)
  "Create a new headline for the page PAGE of DOCUMENT."
  (PDEBUG "ENTER: page: " page)
  (unless (or (ffap-url-p document)
              (and document
         (file-exists-p document)))
    (warn "Document not specified..."))

  (let ((title (completing-read "Note: " nil nil nil nil nil
                                (or selected-text (format "Notes for page %d" page))))
        new-note-position)

    (with-current-buffer tnote/org-buffer
      (widen)

      (condition-case var
          (progn
            (outline-show-entry)
            )
        (error (message "%s" var)))

      (org-insert-heading '(4))
      (insert (org-trim (replace-regexp-in-string "\n" " " title)))
      (org-end-of-subtree)
      (org-N-empty-lines-before-current 1)

      (org-entry-put nil tnote/property-doc-file
                     (tnote/get-relative-name (or document
                         (tnote/-headline-doc-path (current-buffer))
                         (tnote/-find-pdf-path (current-buffer)))))
      (org-entry-put nil tnote/property-note-location (number-to-string page))


      (org-N-empty-lines-before-current 2)

      (org-cycle-hide-drawers t)
      (setq new-note-position (point)))

    (tnote/-switch-to-org-buffer t new-note-position)))

(defun tnote/add-note (&optional precise-info)
  "Insert note associated with the current location.

This command will prompt for a title of the note and then insert
it in the notes buffer.  When the input is empty, a default title
will be generated.

If there are other notes related to the current location, the
prompt will also suggest them.  Depending on the value of the
variable `tnote/closest-tipping-point', it may also
suggest the closest previous note.

PRECISE-INFO makes the new note associated with a more
specific location (see `tnote/insert-precise-note' for more
info)."
  (interactive)
  (PDEBUG "INFO: " precise-info)
  (let* ((page (tnote/doc-approx-location))
         ;; (position (tnote/-go-to-page-note page))
         (selected-text
          (cond
           ((eq major-mode 'pdf-view-mode)
            (when (pdf-view-active-region-p)
              (mapconcat 'identity (pdf-view-active-region-text) ? )))

           (t
            (when (region-active-p)
              (buffer-substring-no-properties (mark) (point))))))

         (document-path (tnote/get-document-path)))

    (if (region-active-p)
        (deactivate-mark))

    (PDEBUG "TEXT:" selected-text)
    (PDEBUG "DOCUMENT-PATH:" document-path)

    (if current-prefix-arg
        ;; when current prefix-arg is set, copy page info only..
        (progn
        (kill-new (format ":%s: %s\n:%s: %d\n"
                          tnote/property-doc-file document-path
                          tnote/property-note-location page))
        (message "note info killed."))
        (tnote/-create-new-note document-path page selected-text))))

(define-obsolete-function-alias
  'tnote/-sync-pdf-page-current 'tnote/sync-page-current "1.3.0")

(defun tnote/eww-after-render ()
  "Function to run after eww is rendered."
  (remove-hook 'eww-after-render-hook 'tnote/eww-after-render)
  (tnote/sync-page-current))

(defun tnote/sync-page-current ()
  "Open page for currently visible notes."
  (interactive)
  (tnote/-switch-to-org-buffer)

  (unless (org-entry-get-with-inheritance tnote/property-note-location)
    (error "No page in current section"))

  (let ((page (string-to-number
               (org-entry-get-with-inheritance tnote/property-note-location)))
        (doc-path (tnote/-headline-doc-path (current-buffer))))
    (when (and (integerp page)
               (> page 0)) ; The page number needs to be a positive integer
      (tnote/-switch-to-doc-buffer)

      (PDEBUG "CURNET-BUFF:" (current-buffer)
              "DOC-PATH: " doc-path
              "CURRENT-URL: " (eww-current-url)
              )
      ;; check if we need to update doc content...
      (cond
       ((ffap-url-p doc-path)
        (unless (string-equal doc-path (eww-current-url))
          (add-hook 'eww-after-render-hook 'tnote/eww-after-render)
          (eww doc-path)
          )
        )
       )
      (tnote/goto-doc-page page))))

(defun tnote/sync-pdf-page-previous ()
  "Move to the previous set of notes.

This show the previous notes and synchronizes the PDF to the right page number."
  (interactive)
  (tnote/-switch-to-org-buffer)
  (widen)
  (tnote/-goto-parent-headline tnote/property-note-location)
  (org-backward-heading-same-level 1)
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (let ((page (string-to-number
                   (org-entry-get-with-inheritance tnote/property-note-location))))
    (when (and (integerp page)
               (> page 0)) ; The page number needs to be a positive integer

      (tnote/-switch-to-doc-buffer)
      (tnote/goto-doc-page page))))

(defun tnote/sync-doc-page-next ()
  "Move to the next set of notes.

This shows the next notes and synchronizes the PDF to the right page number."
  (interactive)
  (tnote/-switch-to-org-buffer)
  (widen)
  ;; go to the first notes heading if we're not at an headline or if
  ;; we're on multi-pdf heading. This is useful to quickly jump to the
  ;; notes if they start at page 96 or so. Image you need to skip page
  ;; for page.
  (if (tnote/-goto-parent-headline tnote/property-note-location)
      (org-forward-heading-same-level 1)

    (org-show-subtree)

    (outline-next-visible-heading 1))
  (org-show-subtree)
  (org-cycle-hide-drawers t)
  (let ((pdf-page (string-to-number
                   (org-entry-get (point) tnote/property-note-location))))
    (when (and (integerp pdf-page)
               (> pdf-page 0)) ; The page number needs to be a positive integer
      (tnote/-switch-to-doc-buffer)
      (tnote/goto-doc-page pdf-page))))

(defun tnote/quit ()
  "Quit tnote mode."
  (interactive)
  (with-current-buffer tnote/org-buffer
    (widen)
    (tnote/-goto-search-position)
    (when (tnote/-headlines-available-p)
      (tnote/-sort-notes tnote/sort-order)
      (org-overview))
    (tnote/notes-mode 0))
  (tnote/kill-proc-and-buffer))

(defun tnote/-headlines-available-p ()
  "True if there are headings in the notes buffer."
  (save-excursion
    (re-search-forward "^\* .*" nil t)))

(defun tnote/-sort-notes (sort-order)
  "Sort notes by tnote_page_property.

SORT-ORDER is either 'asc or 'desc."
  (condition-case nil
      (org-sort-entries nil ?f
                        (lambda ()
                          (let ((page-note (org-entry-get nil "tnote_page_note")))
                            (if page-note
                                (string-to-number page-note)
                              -1)))
                        (if (eq sort-order 'asc)
                            #'<
                          #'>))
    ('user-error nil)))

(defun tnote/-select-split-function ()
  "Determine which split function to use.

This returns either `split-window-below' or `split-window-right'
based on a combination of `current-prefix-arg' and
`tnote/split-direction'."
  (let ((split-plist (list 'vertical #'split-window-right
                           'horizontal #'split-window-below))
        (current-split tnote/split-direction))
    (plist-get split-plist
               (if current-prefix-arg
                   (if (eql current-split 'vertical)
                       'horizontal
                     'vertical)
                 current-split))))

;;; tnote
;; Minor mode for the org file buffer containing notes

(defvar tnote/notes-mode-map (make-sparse-keymap)
  "Keymap while command `tnote/mode' is active in the org file buffer.")

;;;###autoload
(define-minor-mode tnote/notes-mode
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'tnoted' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+NOTER_DOCUMENT: /the/path/to/your/pdf.pdf
- Start `tnote/mode' with `M-x tnote/mode'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

The split direction is determined by the customizable variable
`tnote/split-direction'. When `tnote/mode' is invoked
with a prefix argument the inverse split direction is used
e.g. if `tnote/split-direction' is 'vertical the buffer is
split horizontally.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{tnote/doc-mode-map}

Keybindings (org-mode buffer):

\\{tnote/map}"
  :lighter " ≡"
  :keymap  tnote/notes-mode-map
  (if tnote/notes-mode
      (condition-case nil
          (progn
            (setq tnote/org-buffer (buffer-name))
            (tnote/-open-file (tnote/-select-split-function))
            ;; expand/show all headlines
            (with-current-buffer tnote/org-buffer
              (tnote/-goto-search-position)
              ;; (org-show-subtree)
              (org-cycle-hide-drawers 'all))
            (tnote/-go-to-page-note 1)
            (message "tnote enabled"))
        ('quit
         (tnote/notes-mode -1)))
    ;; Disable the corresponding minor mode in the PDF file too.
    (when (and tnote/doc-buffer
               (get-buffer tnote/doc-buffer))
      (tnote/-switch-to-doc-buffer)
      (tnote/doc-mode -1)
      (setq tnote/doc-buffer nil))
    (setq tnote/org-buffer nil)
    (message "tnote mode disabled")))

(defun tnote/get-document-path ()
  "Return path of document visited by this buffer."
  (interactive)
  (let ((document-path
         (cond
          ((eq major-mode 'nove-mode) nov-file-name)
          ((eq major-mode 'eww-mode) (eww-current-url))

          (t (expand-file-name (or buffer-file-name buffer-file-truename))))))

    (if (called-interactively-p 'interactive)
        (PDEBUG "Document path: " document-path))
    document-path))

(defun tnote/get-relative-name (file)
  "Return relative path of FILE (relative to (tnote/get-notes-dir))."
  (if (ffap-url-p file)
      file
    (file-relative-name file (tnote/get-notes-dir))))

(defun tnote ()
  "Start `tnote' session."
  (interactive)
  (if (eq major-mode 'org-mode)
      (tnote/notes-mode)
    ;; Creating the session from the annotated document
    (let* ((document-path
            (or buffer-file-name
                (if (eq major-mode 'nov-mode)
                    (bound-and-true-p nov-file-name))
                (if (eq major-mode 'eww-mode)
                    (eww-current-url)
                  (error "This buffer does not seem to be visiting any file"))))

           (document-name (file-name-nondirectory document-path))
           (document-base (file-name-base document-name))
           (document-directory
            (if buffer-file-name
                (file-name-directory buffer-file-name)
              (if (and document-name  buffer-file-truename)
                  (if (file-equal-p document-name buffer-file-truename)
                      default-directory
                    (file-name-directory buffer-file-truename))
                default-directory)))

           (search-names (append tnote/default-notes-file-names (list (concat document-base ".org"))))
           notes-files-annotating     ; List of files annotating document
           notes-files                ; List of found notes files (annotating or not)
           )

      (PDEBUG "SEARCH-NAMES: " search-names)

      ;; NOTE(nox): Check the search path
      (dolist (name search-names)
          (let ((file-name (expand-file-name name (tnote/get-notes-dir))))
            (when (file-exists-p file-name)
              (push file-name notes-files)
              (when (or
                     ;; file with same name
                     (string= (file-name-base name) (file-name-base document-name))
                     ;; note file contains document-file.
                     (tnote/check-if-document-is-annotated-on-file
                      document-path file-name))
                (PDEBUG "Annotated file:" file-name)
                (push file-name notes-files-annotating)))))

      ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
      ;; and it will end up in the correct order
      (dolist (name search-names)
        (let ((directory (locate-dominating-file document-directory name))
              file)
          (when directory
            (setq file (expand-file-name name directory))
            (unless (member file notes-files) (push file notes-files))
            (when (tnote/check-if-document-is-annotated-on-file document-path file)
              (push file notes-files-annotating)))))

      (setq search-names (nreverse search-names))

      (when (not notes-files-annotating)
        (let* ((file-name document-path)
               (choices (list
                         "  Create new note file."
                         "  Locate note file manually."))
               (action-index (cl-position
                              (completing-read
                               (format "Could not find note file %s, choose an action: "
                                       file-name)
                               choices
                               nil
                               t)
                              choices
                              :test 'equal)))

          (PDEBUG "INDEX: " action-index)

          (cl-case action-index
            (0
             (push (tnote/find-or-create-file
                    (completing-read "Input title of new note: "
                                      nil nil nil document-base))
                     notes-files))
            (1
             (push (read-file-name "Select note file: "
                                   (expand-file-name (tnote/get-notes-dir)))
                   notes-files))
            (t (error "Wrong result"))))

        (when (> (length notes-files) 1)
          (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                   notes-files nil t))))

        (if (member (car notes-files) notes-files-annotating)
            ;; NOTE(nox): This is needed in order to override with the arg
            (setq notes-files-annotating notes-files)
          (with-current-buffer (find-file-noselect (car notes-files))
            (goto-char (point-min))
            ;; (unless (search-forward (concat "#+" tnote/property-doc-file ": ") nil t)
            ;;   (goto-char (point-max))
            ;;   (insert  "#+" tnote/property-doc-file ": " (tnote/get-relative-name document-path))
            ;;   (insert "\n"))
            )
          (setq notes-files-annotating notes-files)))

      (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
        (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                            notes-files-annotating nil t))))

      (PDEBUG "Notes files: " notes-files-annotating)

      (with-current-buffer (find-file (car notes-files-annotating))
        (tnote/notes-mode)))))

;;; tnote PDF Mode
;; Minor mode for the pdf file buffer associated with the notes


(defvar tnote/doc-mode-map (make-sparse-keymap)
  "Keymap while command `tnote/doc-mode' is active in the pdf file buffer.")


;;; Key-bindings

(define-key tnote/notes-mode-map (kbd "M-.") #'tnote/sync-page-current)
(define-key tnote/notes-mode-map (kbd "M-p") #'tnote/sync-pdf-page-previous)
(define-key tnote/notes-mode-map (kbd "M-n") #'tnote/sync-doc-page-next)

(define-key tnote/doc-mode-map (kbd "n")     #'tnote/go-to-next-page)
(define-key tnote/doc-mode-map (kbd "p")     #'tnote/go-to-previous-page)
(define-key tnote/doc-mode-map (kbd "SPC")   #'tnote/scroll-up)
(define-key tnote/doc-mode-map (kbd "S-SPC") #'tnote/scroll-down)
(define-key tnote/doc-mode-map (kbd "DEL")   #'tnote/scroll-down)
(define-key tnote/doc-mode-map (kbd "i")     #'tnote/add-note)
(define-key tnote/doc-mode-map (kbd "q")     #'tnote/quit)
(define-key tnote/doc-mode-map (kbd "M-.")   #'tnote/sync-page-current)
(define-key tnote/doc-mode-map (kbd "M-p")   #'tnote/sync-pdf-page-previous)
(define-key tnote/doc-mode-map (kbd "M-n")   #'tnote/sync-doc-page-next)


;;;###autoload
(define-minor-mode tnote/doc-mode
  "Minor mode for the document buffer.
Keymap:
\\{tnote/doc-mode-map}"
  :lighter " ≡"
  :keymap  tnote/doc-mode-map

  (when tnote/doc-mode
    (setq tnote/doc-buffer (buffer-name))))

(define-key doc-view-mode-map (kbd "i") #'tnote)
(when (boundp 'pdf-view-mode-map)
  (define-key pdf-view-mode-map (kbd "i") #'tnote))




(defun tnote/update-file-path-in-file (file)
  "Convert absolute path (in FILE) into relative path."
  (PDEBUG "Updating: " file)
  (with-temp-file file
    (insert-file-contents file)
    (goto-char (point-min))
    (when (search-forward-regexp
           (format (rx bol "#+%s:" (+ space) (group (+? nonl)) eol) tnote/property-doc-file) nil t)
      (let ((target-file (match-string 1)))
        (unless (ffap-url-p target-file)
          (replace-match (concat "#+" tnote/property-doc-file ": " (tnote/get-relative-name target-file)))
          )
        )
      )

    (goto-char (point-min))


    (while (search-forward-regexp
            (format (rx ":%s:" (+ space) (group (+? nonl)) eol) tnote/property-doc-file) nil t )
      (let ((target-file (match-string 1)))
        (unless (ffap-url-p target-file)
          (replace-match (concat ":" tnote/property-doc-file ": " (tnote/get-relative-name target-file)))          )
        )
      )
    )
  )

(defun tnote/update-file-path ()
  "Convert absolute path into relative path..."
  (interactive)
  (dolist (file   (tnote/find-files (tnote/get-notes-dir)))
    (tnote/update-file-path-in-file file)))

(provide 'tnote)

;;; tnote.el ends here
