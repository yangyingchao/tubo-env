;;; counsel-compile.el --- Select a Makefile target with counsel -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/counsel-compile
;; Package-Version: 0.1.0
;; Version: 0.1
;; Package-Requires: ((counsel "1.5.3") (projectile "0.11.0"))
;; Keywords: makefile

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A call to `counsel-compile' will give you a `counsel' selection of this directory
;; Makefile's targets.  Selecting a target will call `compile' on it.

;;; Code:

(require 'counsel)
(require '02-functions)

(autoload 'yc/get-compiling-threads "yc-utils" ""  t)
(autoload 'projectile-project-root "projectile" ""  nil)
(autoload 's-split "s" ""  nil)

(defgroup counsel-compile nil
  "Select a Makefile target with counsel."
  :group 'convenience)

(defcustom counsel-compile-do-save nil
  "If t, save all open buffers visiting files from Makefile's directory."
  :type 'boolean
  :group 'counsel-compile)

(autoload '-uniq "dash")

(defun counsel-make (&optional makefile no-choose no-execute)
  "Use `counsel' to select a MAKEFILE target and `compile'.
If makefile is specified use it as path to Makefile.
If NO-CHOOSE is t, compile default target.
If NO-EXECUTE is t, don't execute, but return compile command."
  (interactive)
  (PDEBUG "MK: " makefile
          "PFX:" current-prefix-arg)

  (let* ((arg (if current-prefix-arg
                  (if (listp current-prefix-arg)
                      (car current-prefix-arg)
                    current-prefix-arg)
                (yc/get-compiling-threads)))

         ;; special handling when jobs is 0: turn on verbose mode.
         (verbose (if (= 0 arg) 1 0))
         (jobs  (if (= 0 arg) 1 arg))
         (target (if no-choose
                     ""
                   (let* ((targets (remove "" (counsel-compile--probe-make-targets default-directory))))
                     (unless targets
                       (error "No target in %s" default-directory))

                     (PDEBUG "TARGET: " targets)

                     ;; save buffers before calling make
                     (let* ((regex (format "^%s" default-directory))
                            (buffers
                             (cl-remove-if-not
                              (lambda (b)
                                (let ((name (buffer-file-name b)))
                                  (and name
                                       (string-match regex (expand-file-name name)))))
                              (buffer-list))))
                       (mapc
                        (lambda (b)
                          (with-current-buffer b
                            (save-buffer)))
                        buffers))


                     (ivy-read "Targets " (-uniq (sort targets 'string<))
                               :caller 'counsel-make))))
         (command (format "VERBOSE=%d make -j%d %s" verbose jobs target)))


         (if no-execute
             command
           (compile command))))

(defun counsel-ninja ()
  "Use `counsel' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile"
  (interactive)
  (PDEBUG "ENTER.")
  (let ((r-match-targtes (rx bol "build all:" (* space) (? "phony") (group (+? nonl)) eol))
        (targets '("all")))

    (with-temp-buffer
      (insert-file-contents "build.ninja")
      (goto-char (point-min))

      (unless (search-forward-regexp r-match-targtes nil t)
        (error "Error while parsing targets from ninja"))

      (setq targets (append targets (s-split " " (match-string 1)))))

    (unless targets
      (error "No target in %s" default-directory))

    (PDEBUG "TARGET: " targets)

    (ivy-read "Targets " (-uniq (sort targets 'string<))
              :action (lambda (x)
                        (compile (format "ninja %s" x) ))
              :caller 'counsel-ninja)))

;;;###autoload
(defun counsel-compile2 ()
  "Use `counsel' to select a Makefile target and `compile'.
If makefile is specified use it as path to Makefile"
  (interactive)
  (cond
   ((or (file-exists-p "Makefile")
        (file-exists-p "makefile")
        (file-exists-p "GNUmakefile"))
    (counsel-make))
   ((file-exists-p "build.ninja")
    (counsel-ninja))

   (t (error "Compile method not support"))
   ))

;;;###autoload
(defun counsel-compile-projectile ()
  "Call `counsel-compile' for `projectile-project-root'."
  (interactive)
  (require 'projectile)
  (let* ((proj-root (projectile-project-root))
         (dirs (directory-files proj-root t "build.*" t))
         cands)

    (dolist (dir (push proj-root dirs))
      (if (file-exists-p (expand-file-name "Makefile" dir))
          (push dir cands)))

    (unless cands
      (error "Could not find proper makefile"))

    (PDEBUG "CANDS: " cands)
    (let ((dir (ivy-read "Choose Compile Root: " (sort cands 'string-lessp))))
      (if dir
          (let ((default-directory dir))
            (counsel-make))
        (error "Failed to get compile directory")))))

(provide 'counsel-compile2)

;;; counsel-compile2.el ends here
