;;; counsel-woman.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:
(require 'woman)
(require 'man)

(defun counsel-woman-rebuild-cache ()
  "Rebuild cache."
  (interactive)
  (message "Building list of manual directory expansions...")
  (setq woman-expanded-directory-path
        (woman-expand-directory-path woman-manpath woman-path))
  (message "Building completion list of all manual topics...")
  (setq woman-topic-all-completions
        (woman-topic-all-completions woman-expanded-directory-path))
  (woman-write-directory-cache))

(defun yc/woman-file-name ()
  "Get the name of the UN*X man-page file describing a chosen TOPIC.
When `woman' is called interactively, the word at point may be
automatically used as the topic, if the value of the user option
`woman-use-topic-at-point' is non-nil.  Return nil if no file can
be found.  Optional argument RE-CACHE, if non-nil, forces the
cache to be re-read."
  ;; There is a problem in that I want to offer case-insensitive
  ;; completions, but to return only a case-sensitive match.  This
  ;; does not seem to work properly by default, so I re-do the
  ;; completion if necessary.
  (let ((topic
         (let* ((word-at-point (current-word))
                (default
                  (when (and word-at-point
                             (test-completion
                              word-at-point woman-topic-all-completions))
                    word-at-point)))
           (completing-read
            (if default
                (format "Manual entry (default %s): " default)
              "Manual entry: ")
            woman-topic-all-completions nil 1
            nil
            'woman-topic-history
            default)))
        files)

    ;; Note that completing-read always returns a string.
    (unless (= (length topic) 0)
      (cond
       ((setq files (woman-file-name-all-completions topic)))
       ;; Complete topic more carefully, i.e. use the completion
       ;; rather than the string entered by the user:
       ((setq files (all-completions topic woman-topic-all-completions))
        (while (/= (length topic) (length (car files)))
          (setq files (cdr files)))
        (setq files (woman-file-name-all-completions (car files)))))
      (cond
       ((null files) nil)		; no file found for topic.
       ((null (cdr files)) (car (car files))) ; only 1 file for topic.
       (t
        (completing-read "Manual file: " files nil 1
                         (try-completion "" files) 'woman-file-history))))))

;;;###autoload
(defun counsel-woman ()
  "Browse UN*X man page for TOPIC.
The major browsing mode used is essentially the standard Man mode.
Choose the filename for the man page using completion, based on the
topic selected from the directories specified in `woman-manpath' and
`woman-path'.  The directory expansions and topics are cached for
speed.  With a prefix argument, force the caches to be
updated (e.g. to re-interpret the current directory).

Used non-interactively, arguments are optional: if given then TOPIC
should be a topic string and non-nil RE-CACHE forces re-caching."
  (interactive)
  (unless woman-topic-all-completions
    (counsel-woman-rebuild-cache))

 (let ((file-name (yc/woman-file-name)))
	(if file-name
	    (Man-getpage-in-background file-name)
	  (message
	   "WoMan Error: No matching manual files found in search path")
	  (ding))))

(provide 'counsel-woman)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; counsel-woman.el ends here
