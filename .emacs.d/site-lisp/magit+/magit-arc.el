;;; magit-arc.el --- arc wrapper for magit.

;; Author: YangYingchao <yangyingchao@gmail.com>

;; Copyright (C) 2015 yangyingchao@gmail.com
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.
;;; magit-arc.el --- Brief introduction here.


;;; Commentary:
;; This package provides basic support for integrating arc with magit.
;;
;; When `magit-arc-mode' is turned on, the 'arc'
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-arc-mode)

;; (use-package magit
;;   :hook (
;;          (magit-status-mode .       (lambda ()
;;                                       (when (executable-find "arc")
;;                                         (require 'magit-arc)
;;                                         (magit-arc-mode))
;;                                       (when (try-require 'magit-svn)
;;                                         (define-key magit-mode-map (kbd "N") 'magit-svn-popup))))
;;          )
;;   )

;;; Code:


;; (require 'pp)
;; (require 'magit-mode)
;; (require 'magit)


;; ;;; Options.

;; (defgroup magit-arc nil
;;   "arc support for Magit."
;;   :group 'magit-extensions)

;; (defcustom magit-arc-executable  "arc"
;;   "The Git executable used by Magit."
;;   :group 'magit-arc
;;   :type 'string)

;; (defcustom magit-arc-global-arguments nil
;;   "Global git arguments.

;; The arguments set here are used every time the arc executable is
;; run as a subprocess."
;;   :group 'magit-arc
;;   :type '(repeat string))

;; (defcustom magit-arc-test-plan-regexp   "^Test Plan:\\([[:ascii:]]*\\)Reviewers:"
;;   "Regexp to match Test Plan field."
;;   :type 'string
;;   :group 'magit-arc)

;; (defcustom magit-arc-reviewers-regexp   "^Reviewers:\\([[:ascii:]]*\\)\nSubscribers:"
;;   "Regexp to match Reviewers field."
;;   :type 'string
;;   :group 'magit-arc)

;; (defcustom magit-arc-desc-keywords nil
;;   "Keywords to check for descriptions."
;;   :type '(repeat :tag "Keywords to check"
;;                  (string :tag "Keyword"))
;;   :group 'magit-arc)

;; (defcustom magit-arc-default-test-plan "regression test"
;;   "Default test plan."
;;   :type 'string
;;   :group 'magit-arc)

;; (defcustom magit-arc-finish-query-functions
;;   '(magit-arc--check-keywords
;;     magit-arc--check-test-plan
;;     magit-arc--check-reviewers)
;;   "List of functions called to query before performing commit.

;; The commit message buffer is current while the functions are
;; called.  If any of them returns nil, then the commit is not
;; performed and the buffer is not killed.  The user should then
;; fix the issue and try again.

;; The functions are called with one argument.  If it is non-nil
;; then that indicates that the user used a prefix argument to
;; force finishing the session despite issues.  Functions should
;; usually honor this wish and return non-nil."
;;   :options '(magit-arc--check-test-plan)
;;   :type 'hook
;;   :group 'magit-arc)

;; (defcustom magit-arc-send-arguments nil
;;   "Default arguments used when sending review (arc diff)."
;;   :group 'magit-arc
;;   :type '(repeat (string :tag "Argument")))


;; (defvar magit-arc--rev-alist nil
;;   "AList of commit-rev mapping.
;; It will be loaded from and store into `magit-arc-db'.")

;; ;;; Utilities
;; (defun magit-arc-run (&rest args)
;;   "Call arc synchronously in a separate process, and refresh.

;; Option `magit-arc-executable' specifies the Git executable and
;; option `magit-arc-global-arguments' specifies constant arguments.
;; The arguments ARGS specify arguments to Git, they are flattened
;; before use.

;; After Git returns, the current buffer (if it is a Magit buffer)
;; as well as the current repository's status buffer are refreshed.
;; Unmodified buffers visiting files that are tracked in the current
;; repository are reverted if `magit-revert-buffers' is non-nil.

;; Process output goes into a new section in a buffer specified by
;; variable `magit-process-buffer-name-format'."
;;   (apply 'magit-call-process magit-arc-executable args)
;;   (magit-refresh))

;; (defcustom arc-commit-filename-regexp
;;   (rx "edit." (+? nonl) "/" (or "new-commit" "differential-update-comments") eol)
;;   "Regular expression to match files created by arc."
;;   :type 'string
;;   :group 'magit-arc)

;; (defun arc-commit-setup-check-buffer ()
;;   (and buffer-file-name
;;        (string-match-p arc-commit-filename-regexp buffer-file-name)
;;        (arc-commit-setup)))

;; (defun magit-arc--check-keywords ()
;;   "Check if user specified keywords are included in description."
;;   (if (not magit-arc-desc-keywords)
;;       t
;;     (let ((r-match-brief (rx (group (+ anything)) "
;; Summary:"))
;;           (valid t)
;;           brief missing pos)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (when (re-search-forward r-match-brief)
;;           (setq brief (match-string 1))
;;           (dolist (item magit-arc-desc-keywords)
;;             (unless (string-match (regexp-opt (list item)) brief)
;;               (setq valid nil
;;                     missing (cons item missing))))))
;;       (unless valid
;;         (message "Some fields are missing, please fix them: %s"
;;                  (mapconcat 'identity missing ","))
;;         (goto-char (point-min))
;;         (while missing
;;           (insert (format "%s " (pop missing)))
;;           (unless pos
;;             (setq pos (1- (point)))))
;;         (if pos (goto-char pos)))
;;       valid)))

;; (defun magit-arc--string-empty-p (str)
;;   "Check if STR is empty-line."
;;   (print str)
;;   (backtrace)
;;   (string-match "[^\n 	]" str))

;; (defun magit-arc--check-test-plan ()
;;   "Check if test plan is set or not.
;; If it is not set, it will ask whether it is allowed to set plan to `None'.
;; If it is not allowed, it will return nil so user can continue input correct test  plan."
;;   (let (valid plan pos)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward magit-arc-test-plan-regexp nil t)
;;         (setq plan (match-string 1)
;;               pos (1+ (match-beginning 1)))
;;         (print plan)))
;;     (if (magit-arc--string-empty-p plan)
;;         (setq valid t))
;;     (unless valid
;;       (goto-char pos) ;; pos should never be nil, it is added by arc..
;;       (insert (or magit-arc-default-test-plan "None"))
;;       (setq valid t))
;;     valid))

;; (defun magit-arc--check-reviewers ()
;;   "Check if reviewers is set or not."
;;   (let (valid reviewers)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward magit-arc-reviewers-regexp nil t)
;;         (setq reviewers (match-string 1)
;;               pos (1+ (match-beginning 1)))))
;;     (if (magit-arc--string-empty-p reviewers)
;;         (setq valid t))
;;     (unless valid
;;       (goto-char pos) ;; pos should never be nil, it is added by arc..
;;       (message "Reviewers are not set, sorry I can't do this for you.."))
;;     valid))



;; (defun magit-arc-finish-query-functions (force)
;;   "Run all hooks."
;;   (if (and buffer-file-name
;;            (string-match "new-commit" buffer-file-name))
;;       (run-hook-with-args-until-failure
;;        'magit-arc-finish-query-functions)
;;     t))

;; (defun arc-commit-setup ()
;;   (setq with-editor-show-usage nil)
;;   (with-editor-mode 1)
;;   (add-hook 'with-editor-finish-query-functions
;;             'magit-arc-finish-query-functions nil t)
;;   ;; (add-hook 'with-editor-pre-finish-hook
;;   ;;           'git-commit-save-message nil t)
;;   ;; (add-hook 'with-editor-pre-cancel-hook
;;   ;;           'git-commit-save-message nil t)
;;   ;; (setq with-editor-cancel-message
;;   ;;       'git-commit-cancel-message)
;;   (make-local-variable 'log-edit-comment-ring-index)
;;   ;; (arc-commit-edit-mode 1)
;;   (git-commit-setup-font-lock)
;;   (when (boundp 'save-place)
;;     (setq save-place nil))
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (= (line-beginning-position)
;;              (line-end-position))
;;       (open-line 1)))
;;   (run-hooks 'git-commit-setup-hook)
;;   (set-buffer-modified-p nil))

;; ;;; Commands

;; (magit-define-popup magit-arc-popup
;;   "Popup console for arc commands."
;;   'magit
;;   :man-page "git-arc"
;;   :actions  '(
;;               (?a "Amend Only"   magit-arc-amend)
;;               (?c "Amend & Close"   magit-arc-amend-close)
;;               (?s "Send Review"     magit-arc-send-review)))

;; ;;TODO: update popup based on arguments.

;; (defvar magit-arc--current-commit nil "Nil.")
;; (defvar magit-arc-send-popup
;;   '(:variable magit-arc-send-arguments
;;     :options  ((?e "Set Encoding"          "--encoding=" read-from-minibuffer)
;;                (?r "Set Reviewers"         "--reviewers="  read-from-minibuffer)
;;                (?f "Force Update"          "--update="    read-from-minibuffer)
;;                )
;;     :actions  ((?s "Send review"             magit-arc-do-send-review))
;;     :default-action magit-arc-do-send-review
;;     :max-action-columns 3)
;;   "Parameters for send-poup.")

;; (defun magit-arc-commit-to-revision (commit)
;;   "Find proper revision based on COMMIT."
;;   (let ((r-match-url (rx  "http://" (+ nonl) "/" (group (: "D" (+ digit)))))
;;         revision)
;;     (with-temp-buffer
;;       (magit-insert-revision-message (symbol-name commit))
;;       (goto-char (point-min))
;;       (when (search-forward-regexp r-match-url nil t)
;;         (setq revision (match-string 1))))
;;     revision))

;; (defun magit-arc-get-commit ()
;;   "Find COMMIT at cursor as symbol."
;;   (let ((commit (magit-commit-at-point)))
;;     (if commit (intern commit) nil)))

;; (defun magit-arc-send-arguments (&optional refresh)
;;   (cond ((memq magit-current-popup
;;                '(magit-arc-send-popup))
;;          (let* ((args (magit-popup-export-file-args magit-current-popup-args))
;;                 (arg-1 (car args)) is-update)
;;            (dolist (arg arg-1)
;;              (if (string-match "--update=.+" arg)
;;                  (setq is-update t)))
;;            ;; remove reviewer if it is an UPDATE, since it is not allowed to change
;;            ;; reviewers for this operation.
;;            (when is-update
;;              (setf (car args)
;;                    (cl-remove-if
;;                     (lambda (x)
;;                       (string-match "--reviewers=" x)) arg-1)))
;;            args))
;;         ((derived-mode-p 'magit-log-mode)
;;          (list (nth 1 magit-refresh-args)
;;                (nth 2 magit-refresh-args)))
;;         (refresh
;;          (list magit-log-section-arguments nil))
;;         (t
;;          (-if-let (buffer (magit-mode-get-buffer nil 'magit-log-mode))
;;              (with-current-buffer buffer
;;                (list (nth 1 magit-refresh-args)
;;                      (nth 2 magit-refresh-args)))
;;            (list (default-value 'magit-arc-send-arguments) nil)))))

;; (defun magit-arc--dir (&optional path)
;;   "Return absolute PATH to the control directory."
;;   (magit-git-dir (concat "arc/" path)))

;; (defun magit-arc--delete (file &rest files)
;;   "Delete FILE and FILES, if exists."
;;   (let ((f-list (cons file files)))
;;     (dolist (f f-list)
;;       (if (and (stringp f)
;;                (file-exists-p f))
;;           (delete-file f)))))

;; (defun magit-arc--amend-internal (revision)
;;   "Amend this REVISION."
;;   (magit-arc-run "amend" "--revision" revision)
;;   (when (derived-mode-p 'magit-mode)
;;     (magit-refresh)))

;; ;;;###autoload
;; (defun magit-arc-amend-close (&optional args)
;;   "Amend and close commit message."
;;   (interactive)
;;   (let* ((commit (magit-arc-get-commit))
;;          (revision (magit-arc-commit-to-revision commit)))
;;     (unless revision
;;       (setq revision (completing-read "Input revision number: " nil)))

;;     (if (not revision)
;;         (error "Can't find revision for commit: %s" commit))
;;     (unless (string-match "^D[[:digit:]]+" revision)
;;       (setq commit (concat "D" revision)))
;;     (magit-arc--amend-internal revision)
;;     (magit-arc-run "close-revision" revision)
;;     (message "Revision closed...")))

;; (defun magit-arc-amend (&optional args)
;;   "Amend and close commit message."
;;   (interactive)
;;   (let* ((commit (magit-arc-get-commit))
;;          (revision (magit-arc-commit-to-revision commit)))
;;     (unless revision
;;       (setq revision (completing-read "Input revision number: " nil)))

;;     (if (not revision)
;;         (error "Can't find revision for commit: %s" commit))
;;     (unless (string-match "^D[[:digit:]]+" revision)
;;       (setq commit (concat "D" revision)))
;;     (magit-arc--amend-internal revision)
;;     (message "Amend finished.")))

;; ;;;###autoload
;; (defun magit-arc-send-review (&optional args)
;;   "Send a commit to review with ARGS."
;;   (interactive "P")
;;   (let* ((commit (magit-arc-get-commit))
;;          (revision (magit-arc-commit-to-revision commit)))
;;     (setq magit-arc--current-commit commit)
;;     ;; Show popup and send review, then return a revision id.
;;     (magit-invoke-popup 'magit-arc-send-popup nil args)
;;     (message "Send finished.")))

;; ;;;###autoload
;; (defun magit-arc-do-send-review (&rest args)
;;   "Invoke arc to send review.
;; When `HEAD' is detached or with a prefix argument show log for
;; one or more revs read from the minibuffer."
;;   (interactive)
;;   (let* ((send-args (magit-arc-send-arguments))
;;          (cl (symbol-name magit-arc--current-commit))
;;          (arc-args (list "--head" cl (concat cl "~")))
;;          (logfile (magit-arc--dir "ARC_CMD_OUTPUT"))
;;          (msgfile (magit-arc--dir "create-message")))

;;     (mapc (lambda (x)
;;             (when x
;;               (push x arc-args)))
;;           (car send-args))

;;     (mapc (lambda (x)
;;             (when x
;;               (push x arc-args)))
;;           (cdr send-args))

;;     (push "diff" arc-args)

;;     ;; ignore last message.
;;     (magit-arc--delete msgfile logfile)

;;     (let ((find-file-hook (append find-file-hook '('with-editor-mode))))
;;       (with-editor
;;         (apply 'magit-start-process "arc" nil arc-args)))
;;     (process-put magit-this-process 'logfile logfile)
;;     (set-process-filter magit-this-process 'magit-arc--process-logfile-filter)
;;     (set-process-sentinel magit-this-process 'magit-arc--process-sentinel)))

;; (defun magit-arc--process-logfile-filter (process string)
;;   "Special filter used by `magit-run-git-with-logfile'."
;;   (magit-process-filter process string)
;;   (let ((file (process-get process 'logfile)))
;;     (with-temp-file file
;;       (when (file-exists-p file)
;;         (insert-file-contents file)
;;         (goto-char (point-max)))
;;       (insert string)
;;       (write-region (point-min) (point-max) file))))

;; (defun magit-arc--process-sentinel (process event)
;;   "Sentinel of PROCESS with EVENT to describe change."
;;   (when (memq (process-status process) '(exit signal))
;;     (magit-process-sentinel process event)
;;     (let ((r-match-url (rx (group "http://" (+ nonl) "/" (group (: "D" (+ digit))))))
;;           url revision)
;;       (with-temp-buffer
;;         (insert-file-contents-literally (magit-arc--dir "ARC_CMD_OUTPUT"))
;;         (goto-char (point-min))
;;         (when (search-forward-regexp r-match-url)
;;           (setq url (match-string 1)
;;                 revision (match-string 2))
;;           (magit-arc--amend-internal revision)
;;           (message "Code review sent: %s" url))))))

;; (defvar magit-arc-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "X") 'magit-arc-popup)
;;     map))

;; ;;;###autoload
;; (define-minor-mode magit-arc-mode
;;   "Arc support for Magit."
;;   ;; :lighter magit-arc-mode-lighter
;;   ;; :keymap  magit-arc-mode-map
;;   :group 'magit-arc
;;   (unless (derived-mode-p 'magit-mode)
;;     (user-error "This mode only makes sense with Magit"))
;;   (cond
;;    (magit-arc-mode
;;     (add-hook  'find-file-hook 'arc-commit-setup-check-buffer))
;;    (t
;;     (remove-hook 'find-file-hook 'arc-commit-setup-check-buffer)))
;;   (define-key magit-mode-map (kbd "X") 'magit-arc-popup)
;;   (when (called-interactively-p 'any)
;;     (magit-refresh)))

;; ;;;###autoload
;; (custom-add-option 'magit-mode-hook #'magit-arc-mode)

(provide 'magit-arc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; magit-arc.el ends here
