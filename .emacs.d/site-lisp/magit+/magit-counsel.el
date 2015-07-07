;;; magit-counsel.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(require 'magit-git)
(require 'magit-process)

;;;###autoload
(defun counsel-magit-checkout ()
  "Run magit-checkout with counsel."
  (interactive)
  (ivy-read "Checkout " (magit-list-refnames)
            :action (lambda (rev)
                      (interactive)
                      (magit-run-git "checkout" rev))

            :caller 'counsel-magit))

(defun counsel-magit-checkout-file ()
  "Checkout current file from REV."
  (interactive)

  (let* ((file (magit-current-file)))

    (ivy-read "Checkout " (magit-list-refnames)
              :action (lambda (rev)
                        (interactive)
                        (magit-with-toplevel
                          (magit-run-git "checkout" rev "--" file)))
              :caller 'counsel-magit)))


(autoload 'magit-read-file-from-rev "magit-files")
(defun magit-find-file-in-other-worktree ()
  "View file from other worktree, in another window."
  (interactive )
  (--if-let (cl-delete (directory-file-name (magit-toplevel))
                       (magit-list-worktrees)
                       :test #'equal :key #'car)
      (let* ((default-directory (magit-completing-read
                                 "Find file from worktree"
                                 it)))
        (find-file-other-window
           (expand-file-name (magit-read-file-from-rev
                              "HEAD" "Find file in other workspace"))))
    (user-error "No other worktree is available")))

(provide 'magit-counsel)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; magit-counsel.el ends here
