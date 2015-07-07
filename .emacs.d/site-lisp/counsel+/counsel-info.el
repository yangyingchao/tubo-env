;;; counsel-info.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:


(defvar counsel/info-node-list nil "List of name for info nodes....")


;;;###autoload
(defun counsel/info (reinit)
  "Info with counsel.
If `REINIT' is not nil, re-initialize info-node-list."
  (interactive "P")

  (when (or reinit (not counsel/info-node-list))
    (setq counsel/info-node-list (list nil))
    (let ((r-match-info-node (rx (group (+? nonl)) ".info" (*? nonl)))
          (r-match-info-dir
           ;; works well for things like "/usr/share/info/emacs-27-next",
           ;; TODO: support multiple versions for other apps: ??
           ;;  - gcc: "/usr/share/gcc-data/x86_64-pc-linux-gnu/8.3.0/info"
           ;;  - bin: "/usr/share/binutils-data/x86_64-pc-linux-gnu/2.32/info"
           (rx (+? nonl) "/info/" (group (+ (not (any "/")))) (? "/")))
          (parsed-dirs nil))

      (dolist (dir Info-directory-list)
        (unless (member dir parsed-dirs)
          (push dir parsed-dirs)
          (let ((prefix (when (string-match r-match-info-dir dir)
                          (match-string 1 dir))))
            (dolist (item (directory-files  dir))
              (when (string-match  r-match-info-node item)
                (let* (
                       (node (match-string 1 item) )
                       (fnode (if prefix (concat prefix "/" node) node)))

                  (PDEBUG "ITEM: " dir " --> " fnode)

                  (unless (member fnode counsel/info-node-list)
                    (push fnode counsel/info-node-list))))))))))

  (PDEBUG "REINIT: " reinit
    "\nLIST:" counsel/info-node-list)

  (ivy-read "Info: " (reverse counsel/info-node-list)
            :action (lambda (x)
                      (info x))))

(provide 'counsel-info)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; counsel-info.el ends here
