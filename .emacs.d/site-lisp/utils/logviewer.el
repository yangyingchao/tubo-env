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
(require 'transient)

(defcustom logviewer/fold-long-lines nil
  "Fold a line if it is too long."
  :type '(radio (const :tag "Show all lines." nil)
                (integer :tag "Fold if longer than:" ))
  :group 'logviewer)

;; default mode map, really simple
(cdsq logviewer-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'logviewer/reload-file)
    (define-key map (kbd "f") 'logviewer/filter-levels)
    (define-key map (kbd "l") 'logviewer/toggle-long-lines)
    (define-key map (kbd "q") 'logviewer/quit)
    (define-key map (kbd "/") 'logviewer/filter-lines)
    (define-key map (kbd "c") 'logviewer/undo)
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
          (group (or "info" "INFO" "Info" "LOG" "log")) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-function-name-face))
    (,(rx bol (** 0 256 not-newline) symbol-start
          (group (or "DEBUG" "debug" "Debug"
                     "TRACE" "trace" "Trace")) ":"
          (group (+ (*? not-newline))))
     (1 font-lock-constant-face) (2 font-lock-doc-face))


    ;; prove...
    (,(rx bol (group (or "Bail out!")) (group (** 0 256 not-newline)))
     (1 font-lock-warning-face) (2 font-lock-comment-face))

    (,(rx bol (group (or "ok") (+ space) (+ digit)) )
     (1 font-lock-builtin-face))

    ))


(defvar logviewer/filter-prefix ">>> ")
(defconst logviewer/filter-result-sign
  "------------------------------- filter result -------------------------------"
  "Line indicate filter is started.")

(defun logviewer/-filter-by-regex (regexp)
  "Filter LogContent by REGEXP."
  (unless (string-empty-p regexp)
    (let ((inhibit-read-only t)
          (start
           (or (save-excursion
                 (goto-char (point-min))
                 (search-forward logviewer/filter-result-sign nil t))
               (point-min))))

      (PDEBUG "REGEXP:" regexp
              "START:" start)

      (if (string-prefix-p "!" regexp)
          (flush-lines (substring regexp 1) start (point-max))
        (keep-lines regexp start (point-max)))
      (save-excursion
        (goto-char (point-min))
        (let ((item (propertize (format "[%s]" regexp) 'face 'ivy-current-match)))
          (if (looking-at logviewer/filter-prefix)
              (progn
                (goto-char (line-end-position))
                (insert item))
            (insert logviewer/filter-prefix item "\n"
                    logviewer/filter-result-sign "\n"))))
      (set-buffer-modified-p nil))))

(defconst logviewer-levels
  '("FATAL" "ERROR" "WARNING" "INFO" "LOG" "DEBUG"))

(defun logviewer/filter-levels ()
  "Set and show result of filter level..."
  (interactive)
  (let* ((target-level (completing-read "Filter Level: " logviewer-levels))
         (valid-levels
          (catch 'p-found
            (let (levels)
              (dolist (level logviewer-levels)
                (push level levels)
                (when (string= level target-level)
                  (throw 'p-found levels)))
              levels)))
         (regexp (concat "\\(?:" (s-join "\\|" valid-levels )"\\)")))

    (logviewer/-filter-by-regex regexp)))

(defun logviewer/filter-lines ()
  "Filter log by lines."
  (interactive)
  (let ((regexp (read-regexp "Regexp(! for flush)")))
    (logviewer/-filter-by-regex regexp)))

(defun logviewer/undo ()
  "Undo."
    (interactive)
    (let ((inhibit-read-only t))
      (if (save-excursion
            (goto-char (point-min))
            (looking-at logviewer/filter-prefix))
          (undo)
        (user-error "Filter stack is empty"))))

(defun logviewer/reload-file ()
  "Reload current file."
  (interactive)
    (save-excursion
      (find-file (buffer-file-name)))
      (message "Readload finished."))

(defvar-local logviewer--overlays nil "Folded overlays.")


(add-to-invisibility-spec '(invs . t))

(defvar-local logviewer/-long-lines nil
  "CAR: on or off, CDR: overlays.")

(defun logviewer/-show-long-lines (&optional show)
  "Description.
If SHOW is t, always display invisible lines.
If SHOW is nil, toggle lines based on current status."
  (let* ((status (car logviewer/-long-lines) )
         (overlays (cdr logviewer/-long-lines))
         (prop (if (or show status status)  nil 'invs)))

    (unless overlays
      (error "No lines are folded"))

    (mapc (lambda (ov)
            (overlay-put ov 'invisible prop))
          overlays)
    (setf (car logviewer/-long-lines) (not status))))

(defun logviewer/-hide-long-lines ()
  "Hide long lines, if asked."
  (unless (numberp logviewer/fold-long-lines)
    (setq logviewer/fold-long-lines 120))

  ;; fold full file path into its base name.
  (let (overlays)

    (dolist (regex (list
                    (rx (+ space) (group "/" (+ nonl) "/")
                        (+? (not space)) "." (or "c" "cpp")
                        ":" (+ digit) (+ space) "--")

                    ;; (rx (+ space) (group "CST" (+ space)))
                     ))

      (push (logviewer--hide-by-regex regex) overlays))

    (let ((yc/debug-log-limit -1))
    (PDEBUG "OVERLAYS: " overlays))

    (setq logviewer/-long-lines (cons t (apply #'append overlays)))))


(defun logviewer/toggle-long-lines ()
  "Hide or show long lines."
  (interactive)

  (when (and logviewer/-long-lines
             (cdr logviewer/-long-lines)
             current-prefix-arg)
    (logviewer/-show-long-lines t)
    (setq logviewer/-long-lines nil))

  (if (not logviewer/-long-lines)
      (logviewer/-hide-long-lines)
    (logviewer/-show-long-lines)))


(defun logviewer/-demangle-function-names ()
  "Call c++filt to demangle function names."
  (let ((exec (executable-find "c++filt"))
        (pmax (point-max)))
    (when exec
      (save-excursion
        (goto-char (point-max))
        (call-process-region (point-min) pmax exec nil t))
      (delete-region (point-min) pmax))))

(cdsq logviewer/display-hook
    '(logviewer/-demangle-function-names logviewer/-special-handling-csv)
  "Hooks to run after log file is loaded.." )

(defun logviewer/quit ()
  "Quit logviewer."
  (interactive)
  (kill-buffer))

(defun logviewer--hide-by-regex (r)
  "Hide region matching regex R.
R should contains one capture group."
  (let (overlays)
    (save-excursion
      (goto-char (point-min))
      (PDEBUG "PT" (point))
      (while (search-forward-regexp r nil t)
        (PDEBUG "POINT:" (point))
        (let* ((ov (make-overlay (match-beginning 1) (match-end 1))))
          (overlay-put ov 'invisible 'invs)
          (push ov overlays))))
    overlays))

(defun logviewer/-special-handling-csv ()
  "Special handling for pg logs of CSV format."
  (interactive)
  (when (and buffer-file-name
             (string-match-p
              (rx buffer-start (+? nonl) "/" (+? nonl)"_log/" (+? nonl)
                  "." (or "csv")) buffer-file-name))
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

      (setq buffer-read-only old-value))))


;;;###autoload
(define-derived-mode logviewer-mode fundamental-mode "Log-Viewer"
  "Major mode for editing Logviewer files
Key definitions:
\\{logviewer-mode-map}"
   ; Setup font-lock mode.
  (set (make-local-variable 'font-lock-defaults) '(logviewer-font-lock-keywords))

  ;; hide long file name...
  (run-hooks 'logviewer/display-hook)

  (setq buffer-read-only t)
  (set-buffer-modified-p nil)

  (run-hooks 'logviewer-mode-hook)
  (hl-line-mode)

  (if (member 'vlf-mode minor-mode-list)
      (progn
        (auto-revert-mode -1)
        (local-set-key "n" #'vlf-next-batch)
        (local-set-key "p" #'vlf-prev-batch)
        (local-set-key "s" #'vlf-re-search-backward)
        (local-set-key "[" 'vlf-beginning-of-file)
        (local-set-key "]" 'vlf-end-of-file))
    (auto-revert-mode 1)))


(provide 'logviewer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; logviewer.el ends here
