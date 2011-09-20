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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Code
;; custom hooks
(defvar logviewer-mode-hook nil)

;; default mode map, really simple
(defvar logviewer-mode-map
  (let ((logviewer-mode-map (make-keymap)))
    (define-key logviewer-mode-map "n"
      (lambda (&optional arg)
        (interactive "^p")
        (or arg (setq arg 1))
        (logviewer-next-part t arg)))
    (define-key logviewer-mode-map "p"
      (lambda (&optional arg)
        (interactive "^p")
        (or arg (setq arg 1))
        (logviewer-next-part nil arg)))
    (define-key logviewer-mode-map "R" 'logviewer-reload-file)
    (define-key logviewer-mode-map "F" 'logviewer-set-filter)
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
                      (push filename recentf-list )
                      (while (not (file-exists-p logviewer-current-file))
                        (sleep-for 0.5))

                      (set-buffer (get-buffer-create filename-base))
                      (toggle-read-only 0)
                      (erase-buffer)
                      (insert-file-contents logviewer-current-file nil)
                      (switch-to-buffer filename-base)
                      (toggle-read-only 1)
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



(defun logviewer-is-tmpfile ()
  "See whether current file is a temporary file or not."
  (if (string-match "log_cache" logviewer-current-file)
      t
    nil
    )
  )

(defun logviewer-reload-file ()
  "Reload current file."
  (interactive)
  (let ((pt (point)))
    (toggle-read-only 0)
    (erase-buffer)
    (insert-file-contents logviewer-current-file nil)
    (toggle-read-only 1)
    (goto-char pt)
    (message "Readload finished.")))

(defun get-next-slice (num cc)
  "Get next file, or previous file.
if direc = t, it returns next file, or it returns previous file"
  (let ((filename-pre nil)
        (filename-sub nil)
        (filename logviewer-current-file)
        (filename-sub-num 0)
        (sub-len 0)
        (new-seq 0)
        (fmt "")
        (bname (buffer-name))
        (pre-rx (rx line-start (group (+? not-newline))
                    (group (+ digit)) line-end)))
    (if (string-match pre-rx filename)
        (progn
          (setq filename-pre (match-string 1 filename))
          (setq filename-sub (match-string 2 filename))
          (setq sub-len (length filename-sub))
          (if cc
              (setq new-seq (+ (string-to-number filename-sub) num))
            (setq new-seq (- (string-to-number filename-sub) num)))
          (if (string-match (rx (group (+? anything)) "-P" (+ digit)) bname)
              (setq bname (match-string 1 bname))
            )
          (list (format (format "%%s%%0%dd" sub-len)
                        filename-pre new-seq)
                (format "%s-P%d" bname new-seq)))
      (error "Failed to parse filename"))))


(defun logviewer-next-part (dir &optional arg)
  "view next/previous file"
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((n-list (get-next-slice arg dir))
        (next-file nil)
        (bname nil))
    (if (logviewer-is-tmpfile)
        (progn
          (setq next-file (car n-list))
          (setq bname (nth 1 n-list))
          (if (file-exists-p next-file)
              (progn
                (message (format "Now viewing: %s" next-file))
                (toggle-read-only 0)
                (erase-buffer)
                (insert-file-contents next-file nil)
                (setq logviewer-current-file next-file)
                (toggle-read-only 1)
                (rename-buffer bname))
            (progn
              (let ((msg nil))
                (if dir
                  (setq msg (concat "Head of log reached. File "
                                    next-file " does not exist!" ))
                  (setq msg (concat "Head of log reached. File "
                                    next-file " does not exist!" ))
                  )
                (error msg)))))
      (error "This is the whole file")))
  )


(defconst logviewer-levels
  '("FATAL" "ERROR" "WARRNING" "INFO" "DEBUG"))

(defvar logviewer-filter-level 9 "nil")

(defun get-lvl-str (num)
  "description"
  (let ((x (/ num 2))
        (lst nil))
    (while (>= x 0 )
      (setq x (1- x))
      (add-to-list  'lst (nth x logviewer-levels))
      )
    lst
    )
  )


(defun logviewer-get-filter (lvl)
  "Get filter beyond LVL."
  (if (string= lvl "FATAL")
      (progn
        (setq logviewer-filter-level 1)
        (rx bow "FATAL:"))

    (if (string= lvl "ERROR")
      (progn
        (setq logviewer-filter-level 3)
        (rx bow (or "FATAL" "ERROR") ":"))
      (if (string= lvl "WARRNING")
          (progn
            (setq logviewer-filter-level 7)
            (rx bow (or "FATAL" "ERROR" "WARRNING") ":"))
        (if (string= lvl "INFO")
            (progn
              (setq logviewer-filter-level 9)
              (rx bow (or "FATAL" "ERROR" "WARRNING" "INFO") ":")  ))
        )
      )
    )
  )

(defvar logviewer-filter-list '() "nil")

(defun logviewer-iter (reg-str)
  ""
  (if (search-forward-regexp reg-str (point-max) t)
      (progn
        (let ((pos1)
              (pos2))
          (move-beginning-of-line 1)
          (setq pos1 (point))
          (move-end-of-line 1)
          (setq pos2 (point))
          (cons pos1 pos2)
          (add-to-list 'logviewer-filter-list (cons pos1 pos2))
          (logviewer-iter reg-str)
          )
        )
    nil
      )
  )

(defun logviewer-set-filter ()
  "Set and show result of filter lvl"
  (interactive)
  (setq logviewer-filter-list nil)
  (let ((lvl nil)
        (cur-lvl   logviewer-filter-level ))
    (setq lvl (completing-read "Filter Level: " logviewer-levels))
    (if (string= lvl "DEBUG")
        (outline-flag-region (point-min) (point-max) nil)
      (progn
        (let ((logviewer-filter (logviewer-get-filter lvl))
              (content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (if (< cur-lvl logviewer-filter-level)
              (outline-flag-region (point-min) (point-max) nil)
            )

          (goto-char (point-min))
          (logviewer-iter logviewer-filter)

          (outline-flag-region (point-min) (point-max) t)
          (if (> (length logviewer-filter-list) 0)
              (let ((i 0)
                    (len (length logviewer-filter-list))
                    (frange))
                (while (< i len)
                  (setq frange (nth i logviewer-filter-list))
                  (outline-flag-region (car frange) (1+ (cdr frange)) nil)
                  (setq i (1+ i))
                  ))
            )
          )))
    )
  )



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
  (if logviewer-current-file
      nil
    (setq logviewer-current-file (buffer-file-name))
      )
  (run-hooks 'logviewer-mode-hook))

(add-to-list 'auto-mode-alist '("\\.log\\'" .
                                logviewer-mode))
(provide 'logviewer)
