;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logviewer.el --- Simple log viewer.
;;
;; Copyright (C) 2011, Yang, Ying-chao
;;
;; Author:        Yang, Ying-chao <yangyingchao@gmail.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; Commentary:
;;
;;   This is a simple log viewer, with syntax highlight.
;;
;;   To use logviewer, you should put logviewer.el into the top of load-path
;; of emacs, the add following lines into your .emacs:
;; (require 'logviewer)
;;
;;   When log files are huge, it will try to split huge logs into small ones
;; to speed up loading. In that case, you can press "n" & "p" to go to next
;; part (or previous part) to the log file. You can custom variable
;; logviewer-split-line to proper number to control the size of the slice of
;; huge file.
;;
;; TODO:
;;   Add support of log filtering, ie, show logs whos level is higher than
;; some specified one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Code
;; custom hooks
(defvar logviewer-mode-hook nil)

;; default mode map, really simple
(defvar logviewer-mode-map
  (let ((logviewer-mode-map (make-keymap)))
    (define-key logviewer-mode-map "n"
      (lambda () (interactive) (logviewer-next-file t)))
    (define-key logviewer-mode-map "p"
      (lambda () (interactive) (logviewer-next-file nil)))
    logviewer-mode-map)
  "Keymap for PS major mode")

(defvar logviewer-indent-width 4)

(defvar logviewer-font-lock-keywords
  `(
    ;; Date & time.
    (,(rx line-start
          (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit)
          (? "." (+ digit)))
     . font-lock-builtin-face)
    (,(rx symbol-start
          (group (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit) (? "." (+ digit)))
          (1+ space) (group (1+ (or alnum "-" "_"  blank))) (? "["(* digit) "]")":")
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))
    (,(rx symbol-start
          (group (or "ERROR" "FATAL" "error" "fatal" )) ":"
          (group (+ (*? not-newline))) line-end)
    (1 font-lock-warning-face) (2 font-lock-comment-face))
    (,(rx symbol-start
          (group (or "info" "INFO" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-function-name-face) (2 font-lock-doc-face))
    (,(rx symbol-start
          (group (or "DEBUG" "debug" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-keyword-face) (2 font-lock-string-face))
    )
  )

(defvar logviewer-mode-syntax-table (make-syntax-table)
  "Syntax table for Logviewer mode")
;; (modify-syntax-entry ?( "()" logviewer-mode-syntax-table)
;;                      (modify-syntax-entry ?) ")("
;;                      logviewer-mode-syntax-table)

(defvar logviewer-imenu-expressions
  '((nil "^\\(?:[fF]unction\\|Add-Class\\)\\s-+\\([-a-z0-9A-Z_^:.]+\\)[^-a-z0-9A-Z_^:.]" 1))
  "alist of regexp identifying the start of logviewer definitions"
  )


(defvar logviewer-split-line 50000 "Lines when trying to split files.")
(defvar logviewer-current-file nil
  "Log file viewed by logviewer")

(defun logviewer-process-sentinel (process event)
  "description"
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           )

      (condition-case err
          (delete-process process)
        (error
         (let ((err-str (format "Error in process sentinel: %s"
                                 (error-message-string err))))
           (message err-str)))))))


;;;; Overrite function provied by Emacs itself.
(defun abort-if-file-too-large (size op-type filename)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message to user)."
  (let* ((re-log-str (rx (or "LOG" "log" "Log")))
         (log-cache (expand-file-name "~/.emacs.d/log_cache"))
         (cur-file nil)
         (process nil)
         (filename-base (file-name-sans-extension
                         (file-name-nondirectory filename)))
         (out-file-prefix (format "%s/%s" log-cache filename-base)))
    (if (and  large-file-warning-threshold size
              (> size large-file-warning-threshold))
        (if (string-match re-log-str filename) ;; This is logfile.
            (if (y-or-n-p
                 (format "LogFile %s is large (%dMB), really %s? "
                         (file-name-nondirectory filename)
                         (/ size 1048576) op-type))
                (if (and (string= op-type "open")
                         (executable-find "split"))
                    (progn
                      (if (file-exists-p log-cache)
                          nil
                        (mkdir log-cache t))
                      (setq logviewer-current-file
                            (format "%s000" out-file-prefix))

                      (message (format "%s*" out-file-prefix))
                      (call-process-shell-command "rm" nil "*Messages*" nil
                                    "-rf"
                                    (format "%s*" out-file-prefix))
                      (setq process
                            (start-process "Split-process" "*Messages*"
                                           "split"  "--suffix-length=3"
                                           "-d" "-l"
                                           (format "%s" logviewer-split-line)
                                           (expand-file-name filename)
                                           out-file-prefix))
                      (set-process-sentinel process
                                            'logviewer-process-sentinel)

                      (while (not (file-exists-p logviewer-current-file))
                        (sleep-for 0.5))

                      (set-buffer (get-buffer-create filename-base))
                      (toggle-read-only 0)
                      (erase-buffer)
                      (insert-file-contents logviewer-current-file nil)
                      (switch-to-buffer filename-base)
                      (toggle-read-only 1)
                      (add-to-list 'recentf-list filename)
                      (logviewer-mode)
                      (error "See this instead")
                      )
                  nil
                  (error "Aborted")
                  )
              (when  (not (y-or-n-p
                           (format "YC: File %s is large (%dMB), really %s? "
                                   (file-name-nondirectory filename)
                                   (/ size 1048576) op-type)))
                (error "Aborted"))
              )
          )
      )))


(defun get-next-file (cc)
  "Get next file, or previous file.
if direc = t, it returns next file, or it returns previous file"
  (let ((filename-pre nil)
        (filename-sub nil)
        (filename logviewer-current-file)
        (filename-sub-num 0)
        (sub-len 0)
        (new-seq 0)
        (fmt "")
        (pre-rx (rx line-start (group (+? not-newline))
                    (group (+ digit)) line-end)))
    (if (string-match pre-rx filename)
        (progn
          (setq filename-pre (match-string 1 filename))
          (setq filename-sub (match-string 2 filename))
          (setq sub-len (length filename-sub))
          (if cc
              (setq new-seq (1+ (string-to-number filename-sub)))
            (setq new-seq (1- (string-to-number filename-sub)))
            )
          (format (format "%%s%%0%dd" sub-len)
                  filename-pre new-seq))
      (error "Failed to parse filename"))))

(defun logviewer-next-file (dir)
  "view next/previous file"
  (let ((next-file nil))
    (if (string-match "log_cache" logviewer-current-file)
        (progn
          (setq next-file (get-next-file dir))
          (if (file-exists-p next-file)
              (progn
                (message (format "Now viewing: %s" next-file))
                (toggle-read-only 0)
                (erase-buffer)
                (insert-file-contents next-file nil)
                (setq logviewer-current-file next-file)
                (toggle-read-only 1))
            (progn
              (let ((msg nil))
                (if dir
                  (setq msg (concat "Tail of log reached. File "
                                    next-file " does not exist!" ))
                  (setq msg (concat "Head of log reached. File "
                                    next-file " does not exist!" )))
                (error msg))
              )
            ))
      (error "This is the whole file"))))

(defun logviewer-setup-imenu ()
  "Installs logviewer-imenu-expression."
  (require 'imenu t)
  ;; imenu doc says these 3 are buffer-local by default
  (setq imenu-generic-expression logviewer-imenu-expressions)
  (imenu-add-menubar-index)
  )

(defun logviewer-mode ()
  "Major mode for editing Logviewer files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'logviewer-mode)
  (setq mode-name "logviewer")
  (set-syntax-table logviewer-mode-syntax-table)
  (use-local-map logviewer-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(logviewer-font-lock-keywords))
  (toggle-read-only t)
  (set (make-local-variable 'logviewer-current-file)
       (buffer-file-name))
  (run-hooks 'logviewer-mode-hook))

(add-to-list 'auto-mode-alist '("\\.log\\'" .
                                logviewer-mode))
(provide 'logviewer)
