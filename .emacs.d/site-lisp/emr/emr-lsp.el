;;; emr-lsp.el --- Common refactoring commands via LSP.  -*- lexical-binding: t; -*-

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

;; Common refactoring commands for all programming modes.

;;; Code:

(require 'emr)
(require 'lsp)

(defun emr--lsp-support-formatting ()
  "Check is lsp is enabled."
  (and
   (bound-and-true-p lsp-mode)
   (lsp--capability "documentFormattingProvider")))

(defun emr--lsp-support-range-formatting ()
  "Check is lsp is enabled."
  (and
   (bound-and-true-p lsp-mode)
   (lsp--capability "documentRangeFormattingProvider")))

(defun emr--lsp-support-rename ()
  "Check is lsp is enabled."
  (and
   (bound-and-true-p lsp-mode)
   (lsp--capability "renameProvider")))

(defun emr-lsp-format-buffer ()
  "Format buffer"
  (interactive)
  (lsp-format-buffer))

(defun emr-lsp-format-region (start end)
  "Format region (START END)."
  (interactive "rp")
  (lsp-format-region start end))

(emr-declare-command 'emr-lsp-format-region
  :title "format region"
  :description "with lsp"
  :modes 'prog-mode
  :predicate (lambda ()
               (and
                (emr--lsp-support-range-formatting)
                (bound-and-true-p lsp-mode)
                mark-active (not (equal (mark) (point))))))

(emr-declare-command 'emr-lsp-format-buffer
  :title "format buffer"
  :description "with lsp"
  :modes 'prog-mode
  :predicate (lambda ()
               (and
                (emr--lsp-support-formatting)
                (not mark-active))))


;; lsp...
(defun emr-lsp-rename (newname)
  "Rename with lsp."
  (interactive (list (read-string "Rename to: " (thing-at-point 'symbol))))
  (lsp-rename newname))

(emr-declare-command 'emr-lsp-rename
  :title "rename"
  :description "with lsp"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and (bound-and-true-p lsp-mode)
                    (lsp--capability "renameProvider")
                    (emr-looking-at-symbol?)
                    (not mark-active))))


(provide 'emr-lsp)

;;; emr-lsp.el ends here
