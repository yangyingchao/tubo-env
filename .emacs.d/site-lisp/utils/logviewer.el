;;; logviewer.el --- Simple log viewer.  -*- lexical-binding: t; -*-

;; Author: YangYingchao <yangyingchao@gmail.com>
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
;;; Commentary:
;;
;;   This is a simple log viewer, with syntax highlight.
;;
;;   To use logviewer, you should put logviewer.el into the top of load-path
;; of Emacs, the add following lines into your .emacs:
;; (require 'logviewer)
;;
;;  If log file is too large, suggest to use VLF together with logviewer.
;;

;;; Code:

(require '02-functions)
(require 'hl-line)
(require 'outline)

(defcustom logviewer-fold-long-lines nil
  "Fold a line if it is too long."
  :type '(radio (const :tag "Show all lines." nil)
                (integer :tag "Fold if longer than:" ))
  :group 'logviewer
  )

;; default mode map, really simple
(cdsq logviewer-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x lr") 'logviewer-reload-file)
    (define-key map (kbd "C-x lf") 'logviewer-set-filter)
    (define-key map (kbd "C-x lc") 'logviewer-calibrate)
    (define-key map (kbd "C-x lt") 'logviewer-toggle-long-lines)
    (define-key map (kbd "C-x lq") 'logviewer-quit)
    map)
  "Keymap for logviewer mode.")


(defvar logviewer-font-lock-keywords nil
  "Keywords of logviewer..")

(setq logviewer-font-lock-keywords
   `(
    ;; Date & time.
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "PANIC" "ERROR" "FATAL" "WARNING" "WARN"
                     "panic" "error" "fatal" "warning" "warn"
                     "Panic" "Error" "Fatal" "Warning" "Warn"
                     )) (or ":" "]" ",")
          (group (** 0 256 not-newline)))
     (1 font-lock-warning-face) (2 font-lock-comment-face))

    (,(rx line-start
          (*? (or space alnum "/" "-")) (+ digit) ":" (+ digit) ":" (+ digit)
          (? "." (+ digit)))
     . font-lock-builtin-face)
    (,(rx bol (group (+ digit))  ":" (+ space) (+? nonl) ":" (+ space))
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (** 0 256 not-newline) (+ digit) ":" (+ digit) ":" (+ digit) (? (or "." "-") (+ digit)))
          (1+ space) (group (+? (or alnum "-" "_"  blank))) (? "["(* digit)
                                                               (? "-" (+? nonl)) "]")":")
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))

    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "info" "INFO" "Info")) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-function-name-face))
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "DEBUG" "debug" "Debug"
                     "TRACE" "trace" "Trace")) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-keyword-face) (2 font-lock-doc-face))
    ))

(defun logviewer-calibrate ()
  "Calibrate data-time of logs."
  (interactive)
  (let* ((fmt-tip "%s (format: h:m:s): ")
         (src-time (completing-read (format fmt-tip "From") nil))
         (dst-time (completing-read (format fmt-tip "To") nil))
         (ts (float-time))
         (r-match-date-time
          (rx bol
              (group (** 2 4 digit) "-" (** 1 2 digit) "-" (** 1 2 digit)
                     (+ space)
                     (** 1 2 digit) ":" (** 1 2 digit) ":" (** 1 2 digit))))
         (r-match-time-only
          (rx bol (* space)
              (group (** 1 2 digit) ":" (** 1 2 digit) ":" (** 1 2 digit))))
         (src (if (string-match r-match-time-only src-time)
                  (float-time
                   (date-to-time (concat "2001-01-01 " (match-string 1 src-time))))
                ts))
         (dst (if (string-match r-match-time-only dst-time)
                  (float-time
                   (date-to-time (concat "2001-01-01 " (match-string 1 dst-time))))

                ts))
         (diff (- dst src)))

    ;; calculate diff.
    (if (= 0 diff)
        (error "Invalid time: %s -- %s" src-time dst-time))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward r-match-date-time nil t)
        (replace-match
         (format-time-string "%Y-%m-%d %H:%M:%S"
                             (seconds-to-time
                              (+ diff (float-time (date-to-time (match-string 1)))))))))))

(defun logviewer-reload-file ()
  "Reload current file."
  (interactive)
    (save-excursion
      (find-file (buffer-file-name)))
      (message "Readload finished."))

(defconst logviewer-levels
  '("FATAL" "ERROR" "WARRNING" "INFO" "DEBUG"))

(defvar logviewer-filter-level 9 "nil")

(defvar logviewer--overlays nil "Folded overlays.")

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
  "Set and show result of filter lvl."
  (interactive)
  (setq logviewer-filter-list nil)
  (let ((lvl nil)
        (cur-lvl   logviewer-filter-level ))
    (setq lvl (completing-read "Filter Level: " logviewer-levels))
    (if (string= lvl "DEBUG")
        (outline-flag-region (point-min) (point-max) nil)
      (progn
        (let ((logviewer-filter (logviewer-get-filter lvl))
              ;; (content (buffer-substring-no-properties
              ;;           (point-min) (point-max)))
              )
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
                  (setq i (1+ i))))))))))

(defvar-local logviewer--long-line-hidden nil "Nil.")
(add-to-invisibility-spec '(invs . t))

(defun logviewer-toggle-long-lines ()
  "Hide or show long lines."
  (interactive)
  (unless logviewer--overlays
    (error "No lines are folded"))
  (mapc (lambda (ov)
          (overlay-put ov 'invisible
                       (if logviewer--long-line-hidden
                           nil 'invs)))
        logviewer--overlays)
  (setq logviewer--long-line-hidden (not logviewer--long-line-hidden)))

(defun logviewer-quit ()
  "Quit logviewer."
  (interactive)
  (kill-buffer))

(defun logviewer--hide-by-regex (r)
  "Hide region matching regex R.
R should contains one capture group."
  (save-excursion
    (goto-char (point-min))
    (PDEBUG "PT" (point))
    (while (search-forward-regexp r nil t)
      (PDEBUG "POINT:" (point))
      (let* ((ov (make-overlay (match-beginning 1) (match-end 1))))
        (overlay-put ov 'invisible 'invs)
        (push ov logviewer--overlays)))))

(defun logviewer-special-handling-csv ()
  "Special handling for pg logs of CSV format."
  (interactive)
  (set (make-local-variable 'logviewer--long-line-hidden) t)
  (let ((old-value buffer-read-only)
        (buffer-read-only nil))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              (rx bol "./pg_log/postgresql-" (+? nonl) ".csv:" (+ digit) ":") nil
              t)

        (replace-match "PRI:  ")))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              (rx bol "./pg_log_" (+ digit) "/postgresql-" (+? nonl) ".csv:"
                  (+ digit) ":") nil
              t)

        (replace-match "SEC:  ")))

    (logviewer--hide-by-regex
     (rx (group
          "CST," (+? nonl) " CST," ;; session start timestamp
          (*? nonl) "," ;; virtual transaction id
          (*? nonl) "," ;; transaction id
          )))
    ;; state code
    (logviewer--hide-by-regex
     (rx (or "PANIC" "ERROR" "FATAL" "WARNING" "WARN" "LOG" "DEBUG")
         ","
         (group (+? nonl) ",")
         "\""))

    (logviewer--hide-by-regex
     (rx (group (+ ",")"\"\"") eol))

    (setq buffer-read-only old-value)))

;;;###autoload
(define-derived-mode logviewer-mode fundamental-mode "Log-Viewer"
  "Major mode for editing Logviewer files
Key definitions:
\\{logviewer-mode-map}"
   ; Setup font-lock mode.
  (set (make-local-variable 'font-lock-defaults) '(logviewer-font-lock-keywords))
  ;; (set-syntax-table logviewer-mode-syntax-table)
  (when logviewer-fold-long-lines
    (make-local-variable 'logviewer--overlays)
    (set (make-local-variable 'logviewer--long-line-hidden) t)
    (unless (numberp logviewer-fold-long-lines)
      (setq logviewer-fold-long-lines 1024))

    (logviewer--hide-by-regex
     (format ".\\{%s\\}\\(.+?\\)$" logviewer-fold-long-lines)
     ;; (rx (repeat logviewer-fold-long-lines not-newline)
     ;;     (group (+? not-newline)) eol)

     ))

  ;; special handling for pg log (csv).
  (when (and buffer-file-name
             (string-match-p
              (rx buffer-start (+? nonl) "/pg_log" (+? nonl)
                  "." (or "log" "csv")) buffer-file-name))
    (logviewer-special-handling-csv)
    )


  (setq buffer-read-only nil)
  (auto-revert-mode 1)
  (run-hooks 'logviewer-mode-hook)
  (hl-line-mode))

(provide 'logviewer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; logviewer.el ends here
