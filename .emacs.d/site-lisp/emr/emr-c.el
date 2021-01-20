;;; emr-c.el --- Refactoring commands for C  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Refactoring commands for C and C-Based modes.

;;; Code:

(require 'emr-lsp)
(require 's)
(require 'dash)
(require 'thingatpt)
(autoload 'c-mode-map "cc-mode")

(defcustom emr-clang-format-style 'Google
  "Style used to format codes with clang.
Refer to http://clang.llvm.org/docs/ClangFormatStyleOptions.html for more
detailed descriptions."
  :type '(radio (const :tag "Format with style suggested by Google." Google)
                (const :tag "Format used by LLVM project." LLVM)
                (const :tag "Format used by Chromium project." Chromium)
                (const :tag "Format used by Mozilla project." Mozilla)
                (const :tag "Format used by Webkit project." WebKit)
                (const :tag "Load style configuration from file." file)
                (repeat :tag "Customized alist." (cons (regexp :tag "Tag")
                                                       (directory :tag "Format"))))

  :group 'emr)

(defvar emr-c-format-fallback-func 'indent-region
  "Function to indent a buffer region.
Will be passed start and end positions of region to be formatted.")

; ------------------

;;; EMR Declarations

(autoload 'clang-format-region "clang-format" ""  t)
(autoload 'clang-format-buffer "clang-format" ""  t)

(defun emr-clang-available? ()
  "Return whether clang-format is available."
  (executable-find "clang-format"))

(defun emr-cc-get-style ()
  "Return style as a string."
  (cond
   ((stringp emr-clang-format-style) emr-clang-format-style)
   ((listp emr-clang-format-style)
    (concat "{"(mapconcat (lambda (x)
                            (format "%s: %s" (car x) (cdr x)))
                          emr-clang-format-style ", ") "}"))
   ((symbolp emr-clang-format-style) (symbol-name emr-clang-format-style))
   (t nil)))

(defun emr-cc-format-region (start end)
  "Format region (START/END).
Uses either clang-format, if available, or `emr-c-format-fallback-func'."
  (interactive "rp")
  (if (emr-clang-available?)
      (clang-format-region start end (emr-cc-get-style))
    (funcall emr-c-format-fallback-func start end)))

(defun emr-cc-format-buffer ()
  "Format region (START/END).
Uses either clang-format, if available, or `emr-c-format-fallback-func.'"
  (interactive)
  (if (emr-clang-available?)
      (clang-format-buffer (emr-cc-get-style))
    (funcall emr-c-format-fallback-func (point-min) (point-max))))


(defvar emr-cc-surround-var-hist nil
  "A collection of variables used by if-defs..")

(defun emr-cc-surround-if-end (start end)
  "Surround region between START & END with if-def."
  (interactive "rp")
  (let ((content (buffer-substring-no-properties start end))
        (var (completing-read "Variable Name: " emr-cc-surround-var-hist
                              nil nil nil 'emr-cc-surround-var-hist)))
    (save-excursion
      (delete-region start end)
      (insert (format "#ifdef %s\n" var))
      (insert content)
      (insert (format "\n#endif /*%s*/" var))
      (emr-cc-format-region start (point)))))

(defun emr-cpp-try-catch (start end)
  "Surround region between START & END with try-catch."
      (interactive "rp")
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
        (insert "try {\n")
        (insert content)
        (insert
         "}\ncatch (exception& e) {\n")
        (insert "throw ;\n}\n")
        (emr-cc-format-region start (point)))))

(defun emr-region-active? ()
  "Return t if a valid region is active."
  (and mark-active (not (equal (mark) (point)))))
(defun emr-region-inactive? ()
  "Return nil if a valid region is active."
  (not (emr-region-active?)))


(defun emr-cc-concat (start end)
  "Surround region between START & END with if-def."
  (interactive "rp")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (if (looking-at-p (rx (+? nonl) "\\" eol))
          (forward-line 1)
        (goto-char (point-at-eol))
        (insert "\\")
        (forward-char 1)))
    (emr-cc-format-region start end)))

(emr-declare-command 'emr-cc-concat
  :title "concat lines"
  :description "by adding `\\' at eol"
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))

(defvar emr-cpp-namespace nil "List of namespaces.")

(defun emr-cpp-move-to-namespace (start end)
  "Move content between START & END into a namespace."
  (interactive "rp")
  (let ((content (buffer-substring-no-properties start end))
        (ns (completing-read "namespace: " emr-cpp-namespace)))
    (unless ns
      (error "No namespace provide"))

    (add-to-list 'emr-cpp-namespace ns)

    (save-excursion
      (delete-region start end)
      (insert (format "namespace %s {" ns))
      (insert content)
      (insert "}")
      (emr-cc-format-region start (point)))))

(emr-declare-command 'emr-cpp-move-to-namespace
                     :title "with namespace"
                     :description "move to namespace"
                     :modes '(c++-mode)
                     :predicate (lambda ()
                                  (and mark-active (not (equal (mark) (point))))))
; ------------------

;;; EMR Declarations

(emr-declare-command 'emr-cc-format-region
  :title "format region"
  :description (if (emr-clang-available?)
                   "with clang"
                 "with the value of emr-c-format-fallback-func")
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (and (not (emr--lsp-support-formatting))
                    (emr-region-active?))))

(emr-declare-command 'emr-cc-format-buffer
  :title "format buffer"
  :description (if (emr-clang-available?)
                   "with clang"
                 "with the value of emr-c-format-fallback-func")
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (and (not (emr--lsp-support-formatting))
                    (not (emr-region-active?)))))

(emr-declare-command 'emr-cc-surround-if-end
  :title "surround"
  :description "with if-endif"
  :modes '(c++-mode c-mode)
  :predicate 'emr-region-active?)

(emr-declare-command 'emr-cpp-try-catch
  :title "surround"
  :description "with try-catch"
  :modes '(c++-mode)
  :predicate 'emr-region-active?)


 ;; clang-format on/off over region...
(defun yc/add-clang-format-control (start end)
  "Surround region between START & END with if-def.
With prefix: OFF -- CODE --ON
Without out prefix-args:  ON -- CODE -- OFF, then add OFF at begging of file."
  (interactive "rp")
  (let ((clf-start (if current-prefix-arg "off" "on"))
        (clf-end (if current-prefix-arg "on" "off")))
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert comment-start "clang-format " clf-start comment-end "\n")
      (goto-char (point-max))
      (insert comment-start "clang-format " clf-end comment-end "\n")
      (widen)

      (while (looking-at-p "\n\n\n")
        (delete-char 1))

      (goto-char (point-min))

      (unless current-prefix-arg
        ;; If there is no MARKER before start point, add one after include directive.
        (unless (search-forward "clang-format off" start t)
          (goto-char (point-min))
          (while (search-forward-regexp (rx bol "#include" (+? nonl) "\n") nil t)
            (PDEBUG "PT: %d" (point)))

          (while (looking-at (rx "#endif\n"))
            (goto-char (match-end 0)))

          (PDEBUG "PT2: %d" (point))
          (insert "\n" comment-start "clang-format off" comment-end)

          (while (looking-at-p "\n\n\n")
            (delete-char 1))))

      (if (emr--lsp-support-formatting)
          (progn
            (lsp) ;; force refresh...
            (emr-lsp-format-buffer))
        (emr-cc-format-buffer)))))

(emr-declare-command 'yc/add-clang-format-control
  :title "clang-format on"
  :description "Turn on clang-format on region."
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))

(defun yc/enable-disable-c-block (start end)
  "Enable or disable c blocks(START ~ END) using #if 0/#endif macro."
  (interactive "rp")

  (let* ((if-0-start (concat "#if 0 "
                             (comment-padright comment-start comment-add)
                             "TODO: "
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

(emr-declare-command 'yc/enable-disable-c-block
  :title "toggle: #if 0"
  :description "Enable or disable block."
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))






(provide 'emr-c)

;;; emr-c.el ends here
