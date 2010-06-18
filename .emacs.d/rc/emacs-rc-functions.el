;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;; emacs-rc-functions.el begins ---

(defvar fname nil "File name string")
(require 'sourcepair)

(defun open-mylist ()
  (interactive)
  (find-file "~/Work/Orgs/work.org"))

(defun reload-file ()
  (interactive)
  (find-file (buffer-name)))


(defun edit-emacs ()
  (interactive)
  (find-file "~/.emacs"))

(defun edit-project ()
  (interactive)
  (find-file "~/.emacs.d/projects/my-project.el"))

(defun edit-rcs ()
  "Jump to directory where rc files located"
  (interactive)
  (find-file "~/.emacs.d/rc/")  )


(defun edit-template ()
  "Dired into template."
  (interactive)
  (find-file "~/.emacs.d/templates/yas-snippets/text-mode"))

(defun load-this-file ()
  (interactive)
  (load-file (buffer-name))
  )

(defun compile-this-file ()
  (interactive)
  (byte-compile (buffer-name))
  )



;;;; dos-unix
(defun dos-unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix-dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))


;;;; 自动编译

(setq compilation-window-height 16)
(setq compilation-scroll-output t)

(setq compilation-finish-functions
      (lambda (buf str)
        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, F11 to goto next error.")

          ;;no errors, make the compilation window go away in 0.5 seconds
          ;;        (run-at-time 5.0 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))


(defun make-command()
  (if   (or (file-exists-p "makefile")
            (file-exists-p "Makefile"))
      "make"
    (let ((file (file-name-nondirectory buffer-file-name)))
      (if (or (equal (file-name-extension buffer-file-name) "cc")
              (equal (file-name-extension buffer-file-name) "cpp")
              )
          (progn
            (format "%s %s %s -o %s"
                    (or (getenv "CC") "g++")
                    (or (getenv "CPPFLAGS")"-Wall -g") "*.cc"
                    (file-name-sans-extension file)
                    ))
        (format "%s -o %s %s %s %s %s"
                (or (getenv "CC") "gcc")
                (file-name-sans-extension file)
                (or (getenv "GTKFLAGS") "")
                (or (getenv "CPPFLAGS")"-DDEBUG=9")
                (or (getenv "CFLAGS") "")
                file)
        ))))

(defun do-compile ()
  "save buffers and start compile"
  (interactive)
  (save-some-buffers t)
  (setq compilation-read-command nil)
  ;; (compile (compile-command))
  (compile (make-command))
  (setq compilation-read-command t))

(defun open-makefile ()
  "Open and edit Makefile.description"
  (interactive)
  (setq fname (buffer-file-name))
  (find-file (concat (file-name-directory fname) "/Makefile"))
  )

(global-set-key (kbd "<C-f5>")  (lambda()(interactive)(do-compile)))
(global-set-key (kbd "<C-S-f5>")  (lambda()(interactive)(open-makefile)))
;;;; Add new line before or after current line.
(defun zl-newline nil
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'zl-newline)

(defun zl-newline-up nil
  (interactive)
  (beginning-of-line)
  (newline-and-indent))
(global-set-key [f2] 'zl-newline-up)


(defun my-list-bookmarks nil
  (interactive)
  (split-window-horizontally)
  (list-bookmarks))


;;;;  缩进或者补齐
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)
    ))

;;;;Êó±ê¹öÂÖ£¬Ä¬ÈÏµÄ¹ö¶¯Ì«¿ì£¬ÕâÀï¸ÄÎª3ÐÐ
(defun up-slightly ()
  (interactive) (scroll-up 3))
(defun down-slightly ()
  (interactive) (scroll-down 3))

;;;;shell,gdbÍË³öºó£¬×Ô¶¯¹Ø±Õ¸Ãbuffer
(defun mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-buffer-on-exit)
  '(auto-fill-mode nil))

(defun kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;;; Create ~/.emacs.el to speed up eamcs
(defun autocompile nil
  (interactive)
  (setq fname (buffer-file-name))
  (if (or (string= fname (concat default-directory ".emacs"))
          (string= (file-name-extension fname) "el"))
      (try-compile-file fname)))

(defun try-compile-file(fname)
  (interactive)
  (if (string= fname (concat default-directory ".emacs"))
      nil (load-file fname))
  (byte-compile-file fname))

;;;; Make current-buffer 10 lines higher.
(defun my-adjust-window()
  (interactive)
  (enlarge-window 10))

;; date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defvar skeleton-pair-alist nil)
(defvar skeleton-pair t)
(setq skeleton-pair-alist
      '(
        (?\< ?  _ ">")
        (?\{ ?  _ "}")
        (?\（ ?  _ "）")
        (?\“ ?  _ "”")
        (?\$ ?  _ "$")
        ))

(defun program-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-c-mode-left-brace)
  )

(defun base-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "“") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "（") 'skeleton-pair-insert-maybe)
  )
(add-hook 'text-mode-hook 'base-auto-pair)

(defun setup-program-keybindings()
  (interactive)

  ;;;; Common program-keybindings
  (local-set-key  [(tab)] 'indent-or-complete)
  (local-set-key  [(return)] 'newline-and-indent)

  (xgtags-mode 1) ;; keybindings for xgtags.
  (local-set-key (kbd "M-|") 'align)

  ;;;; "keybindings for sematic"
  (semantic-default-c-setup)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cR" 'semantic-symref)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-ia-show-summary)
  (local-set-key "\C-cd" 'semantic-ia-show-doc)
  (local-set-key "\C-cr" 'semantic-symref-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  ;;;; Others
  (local-set-key "\C-ch" 'sourcepair-load)
  (local-set-key "\C-xh" 'sourcepair-load)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c:" 'uncomment-region)
  )

(defun yyc/show-prog-keywords ()
  (interactive)
  ;; highlight additional keywords
  (font-lock-add-keywords nil
  '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\|HACK\\):" 1
     font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(FIX \\|FIXME \\|TODO \\|BUG \\|XXX \\|HACK \\)" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(defun my-program-hook ()
  ;; Enable hide-ifdef-mode
  (yyc/show-prog-keywords)
  (setup-program-keybindings)
  (program-mode-auto-pair)
  )

(add-hook 'c-mode-common-hook 'my-program-hook)
(add-hook 'c-mode-hook 'my-program-hook)
(add-hook 'c++-mode-hook 'my-program-hook)
(add-hook 'python-mode-hook 'my-program-hook)
(add-hook 'java-mode-hook 'my-program-hook)
(add-hook 'lisp-mode-hook 'my-program-hook)
(add-hook 'emacs-lisp-mode-hook 'my-program-hook)
(add-hook 'shell-script-mode-hook 'my-program-hook)

;;;; functions to setup platform depadent settings.

(defun skeleton-c-mode-left-brace (arg)
  (interactive "P")
  (if  (c-in-literal (c-most-enclosing-brace (c-parse-state)))
      (self-insert-command 1)
    ;; auto insert complex things.
    (let* ((current-line (delete-and-extract-region (line-beginning-position) (line-end-position)))
           (lines (and arg (mark t) (delete-and-extract-region (mark t) (point))))
           (after-point (make-marker)))
       ;;; delete extra blank begin and after the LINES
      (setq lines (and lines
                       (with-temp-buffer
                         (insert lines)
                         (goto-char (point-min))
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (goto-char (point-max))
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (buffer-string))))
      (save-excursion
        (let* ((old-point (point)))
          (insert (if current-line current-line "")  "{\n")
          (and lines (insert lines))
          (move-marker after-point (point))
          (insert "\n}")
          (indent-region old-point (point) nil)))
      (goto-char after-point)
      (c-indent-line))))

(setq yyc/trailing-whitespace-modes '(c++-mode
                                      c-mode
                                      haskell-mode
                                      emacs-lisp-mode
                                      lisp-mode
                                      python-mode
                                      scheme-mode
                                      erlang-mode))
(defun yyc/trailing-whitespace-hook ()
  (when (member major-mode yyc/trailing-whitespace-modes)
    (delete-trailing-whitespace)))

;; clean trailing whitespaces automatically
(add-hook 'before-save-hook 'yyc/trailing-whitespace-hook)

;;;; Untabify
(setq yyc/untabify-modes '(haskell-mode
                           emacs-lisp-mode
                           lisp-mode
                           scheme-mode
                           erlang-mode
                           python-mode
                           clojure-mode))

(defun yyc/untabify-hook ()
  (when (member major-mode yyc/untabify-modes)
    (untabify (point-min) (point-max))))

;;;; untabify some modes
(add-hook 'before-save-hook 'yyc/untabify-hook)

;;;; Shift regiion to left or right quickly.

(defvar shift-indent-offset 4)

(defun shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun shift-region-right (start end &optional count)
  "Shift region of Python code to the right."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (shift-region start end (prefix-numeric-value
                           (or count shift-indent-offset))))

(defun shift-region-left (start end &optional count)
  "Shift region of Python code to the left."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (shift-region start end (- (prefix-numeric-value
                              (or count shift-indent-offset)))))

(global-set-key "\C-c>" 'shift-region-right)
(global-set-key "\C-c<" 'shift-region-left)
;;;; Other Hooks.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'autocompile)
(add-hook 'shell-mode-hook 'mode-hook-func)
(add-hook 'gdb-mode-hook 'mode-hook-func)

(defun yyc/load-w3m ()
  "Load configurations about w3m"
  (interactive)
  (require 'emacs-rc-w3m)
  (w3m)
  )

(defvar bn nil "nil")
(defun yyc/w3m-open-this-page ()
  "Call w3m to open this html file"
  (interactive)
  (setq bn (buffer-file-name))
  (if (string= (file-name-extension bn) "org")
      (setq fname (concat (file-name-sans-extension bn) ".html"
                          ))
    (setq fname bn))
  (require 'emacs-rc-w3m)
  (w3m fname)
  )

(global-set-key (kbd "<C-f8>") 'yyc/load-w3m)
(global-set-key (kbd "<C-S-f8>") 'yyc/w3m-open-this-page)

;;;; Following functions are from Taylor, And this is the license.

;; Copyright (C) 2003 Art Taylor

;; Filename: uniq.el
;; Author: Art Taylor <reeses@astrogoth.com>
;; Version: 1.0
;; Keywords: uniq, duplicates

;; [Commentary]
;;
;; Remove duplicate lines from a region.  uniq-region behaves in a
;; fashion similar to the Unix utility 'uniq', only removing a line
;; duplicating the immediate antecedent.  uniq-remove-dup-lines will
;; remove all duplicate lines from the region, leaving only the first
;; instance.

;; [License]
;;
;; This software is provided 'as-is', without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;;
;; Note that this license is borrowed from zlib via nullsoft.
;;
;; Written 12-Feb-2003, Washington, DC
;;
(defun uniq-region (beg end)
  "Remove duplicate lines, a` la Unix uniq.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "r")
  (let ((ref-line nil))
      (uniq beg end
           (lambda (line) (string= line ref-line))
           (lambda (line) (setq ref-line line)))))

(defun uniq-remove-dup-lines (beg end)
  "Remove all duplicate lines wherever found in a file, rather than
   just contiguous lines."
  (interactive "r")
  (let ((lines '()))
    (uniq beg end
         (lambda (line) (assoc line lines))
         (lambda (line) (add-to-list 'lines (cons line t))))))

(defun uniq (beg end test-line add-line)
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
    (if (funcall test-line (thing-at-point 'line))
        (kill-line 1)
      (progn
        (funcall add-line (thing-at-point 'line))
        (forward-line))))
      (widen))))

(provide 'emacs-rc-functions)
;;; emacs-rc-functions.el ends here
