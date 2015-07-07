;;; 071-miscs-vcs.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:
(use-package mule  :commands (recode-region))

(use-package git-commit
  :defer t
  :config
    (substitute-key-definition
     'kill-buffer  'git-commit-abort git-commit-mode-map)
    (substitute-key-definition
     'ido-kill-buffer  'git-commit-abort git-commit-mode-map)
    (yc/set-company-backends 'git-commit-mode 'company-capf 'company-dabbrev
      'company-ispell)
  (yc/add-safe-local-var '(git-commit-major-mode . git-commit-elisp-text-mode))
  :custom
  (git-commit-summary-max-length 72)
  (git-commit-major-mode 'text-mode))

(use-package git-timemachine
  :ensure t
  :bind (:map ctl-x-map ("gt" . git-timemachine)))

(use-package magit-repos
  :custom
  (magit-repository-directories `((,(expand-file-name "~") . 0)))
  :bind (:map ctl-x-map
              ("gL" . magit-list-repositories)))

(use-package magit-log  :bind (:map ctl-x-map ("gl" . magit-log-buffer-file)))

(use-package magit-counsel
  :bind (:map ctl-x-map
              ("gc" . counsel-magit-checkout)
              ("gU" . counsel-magit-checkout-file)
              ("gw" . magit-find-file-in-other-worktree)))

(use-package magit-auto-revert
  :commands (magit-auto-revert-mode))

(defvar yc/auto-merge-func nil
  "Function try to merge file automatically.
This function accept file name as argument, and return t if file is merged automatically.")

(use-package magit-ediff
  :commands (magit-unmerged-files))

(defun yc/git-add-current-file ()
  "Add curent file to state."
  (interactive)

  (unless (buffer-file-name)
    (error "Not a file"))

  (if (buffer-modified-p)
      (save-buffer))

  (shell-command-to-string (concat "git add " (buffer-file-name)))

  (let* ((default-directory (magit-toplevel))
         (cands (magit-unmerged-files)))
    (if cands
        (let* ((next-file
                (if current-prefix-arg
                    (ivy-read "Choose NextFile: " cands)
                  (car cands))))

          ;; Do smerge-ediff when auto-merge-func is not specified, or failed.
          (when (or (not yc/auto-merge-func)
                    (not (funcall yc/auto-merge-func next-file)))
            (with-current-buffer (find-file next-file)
              (smerge-ediff))))

      (message "All files are cleared in this folder."))))


(use-package magit
  :ensure t
  :commands (magit-blame-addition magit-revision-files magit-toplevel)
  :bind (:map ctl-x-map
              ("gs" . magit-status)
              ("gf" . magit-find-file-other-window)
              ("gb" . magit-blame-addition)
              ("ga" . 'yc/git-add-current-file)
              )

  :bind ((;; (kbd "C-x M-g")
               [24 134217831] . magit-dispatch))
  :hook ((magit-process-mode . goto-address-mode))
  :custom
  (magit-diff-refine-hunk t)  ;; show granular diffs in selected hunk
  (magit-revert-buffers t)
  (magit-save-repository-buffers 'dontask)
  (magit-commit-show-diff nil)
  (magit-push-always-verify nil)
  (magit-revision-show-gravatars nil)
  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (magit-revision-insert-related-refs nil)
  (magit-revision-headers-format "Author:     %aN <%aE>\nDate: %ad\n")
  (magit-no-confirm nil)
  (magit-delete-by-moving-to-trash nil)
  (magit-log-arguments '("-n256" "--graph" "--decorate" "--color"))
  (magit-patch-arguments (quote ("--output-directory=patches")))
  (magit-merge-arguments
   (quote
    ("--strategy=recursive" "--strategy-option=ignore-space-change")))
  (magit-visit-ref-behavior
   (quote (focus-on-ref create-branch checkout-any check-branch)))

  (magit-status-sections-hook
   (quote
    (
     magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpulled-from-upstream
     magit-insert-unpulled-from-pushremote
     magit-insert-unpushed-to-upstream
     magit-insert-unpushed-to-pushremote)))

  :hook ((magit-find-file
          .
          (lambda () ;; Guess proper encoding for files.
            (setq buffer-read-only nil)
            (recode-region (point-min) (point-max) 'undecided 'utf-8)
            (setq buffer-read-only t))))
  :config
  (magit-auto-revert-mode 1))


(defadvice! yc/with-editor-locate-emacsclient-adv (&rest args)
  "Find proper emacsclient for with-editor."
  :before-until  #'with-editor-locate-emacsclient
  (let ((emacs-path (file-truename (format "/proc/%d/exe" (emacs-pid)))))
    (or (yc/file-exists-p (concat (file-name-directory emacs-path) "emacsclient-"
                                  (file-name-nondirectory emacs-path)))
        (yc/file-exists-p (replace-regexp-in-string "/emacs" "/emacsclient" emacs-path)))))



(use-package dsvn
  :commands (svn-status))

(provide '071-miscs-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 071-miscs-vcs.el ends here
