;;; c-utils.el --- Utilities for c/c++ mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/yyc>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: August 14, 2020
;; Modified: August 14, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yyc/c-utils
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:



(require 'use-package)
(require 'ccls)
(eval-when-compile
  (require 'cl))

(autoload 'yc/remove-empty-lines "prog-utils")
(require '02-functions)

(defvar yc/c-file-mode-mapping
  (list (cons (rx (or "linux-" "kernel" "driver" "samba")) "kernel")
        (cons (rx (or "curl" "emacs" "gnome")) "gnu")
        (cons (rx (or "mysql") (*? nonl) "/") "mysql")
        (cons (rx (or "postgresql" "postgres" "gpdb") (*? nonl) "/") "postgres")
        (cons (rx "/" (or "llvm" "clang")  "/") "llvm.org")
        )
  "List of possible coding styles.")


(defun yc/get-c-style (&optional filename)
  "Guess c-style based on input FILENAME."
  (interactive)

  (let* ((dirpath (cond
                   (filename (file-name-directory filename))
                   (buffer-file-name (file-name-directory buffer-file-name))
                   (t default-directory)))

         (style (catch 'p-found
                  (dolist (kv yc/c-file-mode-mapping)
                    (when (string-match (car kv) dirpath)
                      (PDEBUG "MATCH: " kv)
                      (throw 'p-found (cdr kv))))
                  "tubo")))

    ;; Print style if called interactively.
    (when (called-interactively-p 'interactive)
      (message "Style is %s" style))
    style))


(use-package clang-format
  :commands (clang-format-buffer))

;;;###autoload
(defun yc/format-files ()
  "Format all files in `default-directory'."
  (interactive)
  (save-excursion
    (dolist (fn (directory-files default-directory nil
                                 (rx "." (or "c" "cpp" "cc" "cxx" "h" "hpp") eol)))

      (with-current-buffer (find-file-noselect fn)
        (clang-format-buffer)
        (save-buffer)
        (kill-buffer)))))


(use-package projectile
  :commands (projectile-find-other-file))


;; ================================== My STRING utils ========================
(defun eassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun eassist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))
;; ================================== My STRING utils end ====================

;; ================================== CPP-H switch ===========================
;;;###autoload
(defvar eassist-header-switches '(("h" . ("cpp" "cc" "c" "cxx" "C" "m" "mm"))
				  ("hpp" . ("cpp" "cc" "cxx"))
				  ("cpp" . ("h" "hpp"))
                  ("cxx" . ("h" "hpp"))
				  ("c" . ("h" ))
				  ("C" . ("H"))
				  ("H" . ("C" "CPP" "CC"))
				  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun eassist-switch-h-cpp ()
  "Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (eassist-string-without-last (buffer-name) (length ext)))
         (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (cl-find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (cl-loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
		 when (bufferp (get-buffer b)) return
 		 (if (get-buffer-window b)
 		     (switch-to-buffer-other-window b)
 		   (if (get-buffer-window b t)
 		       (switch-to-buffer-other-frame b)
 		     (switch-to-buffer b))))
           (cl-loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c)))
        (error "There is no corresponding pair (header or body) file.")))
     (t
      (message "It is not a header or body file! See eassist-header-switches
variable.")))))

(defun yc/switch-h-cpp ()
  "Switch between headers and source files."
  (interactive)
      (condition-case error
        (eassist-switch-h-cpp)
      (error (projectile-find-other-file))))

;; ================================== CPP-H switch end =========================

(defun yc/enable-disable-c-block (start end)
  "Enable or disable c blocks(START ~ END) using #if 0/#endif macro."
  (interactive "rp")

  (let* ((if-0-start (concat "#if 0 "
                             (comment-padright comment-start comment-add)
                             "TODO: Remove this if 0!"
                             (comment-padleft comment-end comment-add)
                             "\n"))

         (if-0-end   (concat "#endif "
                             (comment-padright comment-start comment-add)
                             "End of #if 0"
                             (comment-padleft comment-end comment-add)
                             ))

         )
    (save-excursion
      (save-restriction
        (narrow-to-region start end)

        (goto-char (point-min))

        (if (search-forward-regexp
             (rx bol "#if " (+? nonl) "\n"
                 (group (+? anything) "\n")
                 bol "#endif" (+? nonl) eol)
             end t)
            (replace-match "\\1")

          (goto-char end)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert if-0-end "\n")

          (goto-char start)
          (insert if-0-start)))

      (indent-region start end))))

(defun yc/insert-empty-template ()
  "Make header based on srecode."
  (interactive)
  (save-excursion
    (srecode-load-tables-for-mode major-mode)
    (srecode-insert "file:empty")
    (delete-trailing-whitespace)))

(defun yc/header-make ()
  "Make header based on srecode."
  (interactive)
  (progn;save-excursion
    (goto-char (point-min))
    (while (looking-at (or comment-start-skip comment-start))
      (forward-line))
    (condition-case err
        (progn
          (srecode-load-tables-for-mode major-mode)
          (yc/remove-empty-lines (point-min))

          (srecode-insert "file:fileheader")
          (yc/remove-empty-lines (point-max))
          (goto-char (point-max))
          (srecode-insert "file:fileheader_end"))
      (error (srecode-insert "file:filecomment")))
    )
  (delete-trailing-whitespace))


(defun yc/preprocess-file ()
  "Pre-process current file.."
  (interactive)
  (lsp--cur-workspace-check)
  (PDEBUG "default-directory:" default-directory)
  (-when-let* ((mode major-mode)
               (ccls-info (ccls-file-info))
               (args (seq-into (gethash "args" ccls-info) 'vector))
               (working-directory default-directory)
               (new-args (let ((i 0) ret)
                           (while (< i (length args))
                             (let ((arg (elt args i)))
                               (cond
                                ((string= arg "-o") (cl-incf i))
                                ((string-match-p "\\`-o.+" arg))
                                ((string-match-p "\\`--driver-mode=.+" arg) (cl-incf i))
                                ((string-match "\\`-working-directory=\\(.+\\)" arg)
                                 (setq working-directory (match-string 1 arg)))
                                (t (push arg ret))))
                             (cl-incf i))
                           (nreverse ret))))

    (PDEBUG "DIR" (shell-command-to-string "pwd"))

    (with-current-buffer (get-buffer-create
                          (format "*lsp-ccls preprocess %s*" (buffer-name)))
      (pop-to-buffer (current-buffer))
      (with-silent-modifications
        (erase-buffer)
        (insert (format "// Generated by: %s"
                        (combine-and-quote-strings new-args)))
        (insert (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'process-file (car new-args) nil t nil "-E" (cdr new-args)))))
        (delay-mode-hooks (funcall mode))
        (setq buffer-read-only t)))))


;;;###autoload
(defun c++filt-buffer ()
  "Call c++filt for current buffer."
  (interactive)
  (let ((exec (executable-find "c++filt"))
        (pmax (point-max))
        )
    (unless exec
      (error "Can't find c++filt"))

    (save-excursion
      (goto-char (point-max))
      (call-process-region (point-min) pmax exec nil t))
    (delete-region (point-min) pmax)))



(defun yc/cc-c-c++-objc-mode ()
  "Use heuristics to detect `c-mode', `objc-mode' or `c++-mode'.

1. Checks if there are nearby cpp/cc/m/mm files with the same name.
2. Checks for ObjC and C++-specific keywords and libraries.
3. Falls back to `+cc-default-header-file-mode', if set.
4. Otherwise, activates `c-mode'.

This is meant to replace `c-or-c++-mode' (introduced in Emacs 26.1), which
doesn't support specification of the fallback mode and whose heuristics are
simpler."

  (defun +cc--re-search-for (regexp)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp magic-mode-regexp-match-limit t)))))

  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
    (cond ((or (file-exists-p (concat base ".cpp"))
               (file-exists-p (concat base ".cc")))
           (c++-mode))
          ((or (file-exists-p (concat base ".m"))
               (file-exists-p (concat base ".mm"))
               (+cc--re-search-for
                (concat "^[ \t\r]*\\(?:"
                        "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                        "\\|#import +<Foundation/Foundation.h>"
                        "\\|[-+] ([a-zA-Z0-9_]+)"
                        "\\)")))
           (objc-mode))
          ((+cc--re-search-for
            (rx (or
                 (: "#include" (+ space) (or "<" "\"")
                    (or "string" "iostream" "map" "vector")
                    (or ">" "\""))
                 (: bol (* space) (or "template" "class" "using namespace"))
                 (: bol (* space) "namespace" (+? anything) "{")
                 )))
           (c++-mode))
          ((c-mode)))))


(provide 'c-utils)
;;; c-utils.el ends here
