;;; counsel-utils.el --- enhancements with counsel. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/yyc>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: August 24, 2020
;; Modified: August 24, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yyc/counsel-utils
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(require 'counsel)
(require '02-functions)

(defun yc/counsel-grep (&optional initial-input)
  "Grep with rg or ag.
When called with prefix arg, don't respect .gitignore.
If INITIAL-INPUT is not nil, use it."
  (interactive)

  (PDEBUG "CURRENT-DIR: " default-directory
          "INITIAL-INPUT:" initial-input)

  (let ((m (point-marker))
        (init (or initial-input (aif (symbol-at-point) (symbol-name it)))))

    (cond
     ;; use rga if specified...
     ((and current-prefix-arg (executable-find "rg"))
      (let ((counsel-rg-base-command
             (concat "rg -uuu " (s-join " " (cdr counsel-rg-base-command)))))
        (PDEBUG "CMD: " counsel-rg-base-command)
        (counsel-rg init default-directory)))

     ;; ;; or, prefer to rg if possible..
     ((executable-find "rg")
      (counsel-rg init default-directory))

     ;; or, the-silver-searcher if possible
     ((executable-find "ag") (counsel-ag init default-directory))

     ;; at last, grep..
     ((executable-find "grep") (counsel-grep init))
     (t (error "Can't find proper grep function")))

    (yc/push-stack m)))

(defun counsel-find-file-as-user (x)
  "Find file X with root privileges."
  (counsel-require-program counsel-root-command)
  (let* ((host (file-remote-p x 'host))
         (user-name (ivy-read "open file as user: " nil))
         (file-name (format "/%s:%s@%s:%s"
                            counsel-root-command
                            user-name
                            (or host "127.0.0.1")
                            (expand-file-name
                             (if host
                                 (file-remote-p x 'localname)
                               x)))))
    (PDEBUG "FILE:" file-name)
    ;; If the current buffer visits the same file we are about to open,
    ;; replace the current buffer with the new one.
    (if (eq (current-buffer) (get-file-buffer x))
        (find-alternate-file file-name)
      (find-file file-name))))

(defun counsel-grep-in-dir (x)
  "Grep in curtent dir X."
  (interactive)
  (PDEBUG "X: " x (file-exists-p x))

  (let* ((x (if (file-name-absolute-p x)
                x
              (expand-file-name x)))
         (default-directory (if (file-directory-p x) x
                              default-directory)))
    (yc/counsel-grep)))

(defun yc/projectile-grep ()
  "Call `counsel-grep-in-dir' in top directory of current project."
  (interactive)
  (counsel-grep-in-dir (projectile-project-root)))

(provide 'counsel-utils)
;;; counsel-utils.el ends here
