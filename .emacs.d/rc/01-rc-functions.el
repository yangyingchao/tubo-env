;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;; emacs-rc-functions.el begins ---

(defvar fname nil "File name string")
(defvar txt2png_args " -jar ~/.emacs.d/tools/ditaa.jar " "nil")

(require 'sourcepair)

(defun open-mylist ()
  (interactive)
  (find-file "~/Work/Orgs/to_do_today.org"))
(global-set-key [(control f1)] 'open-mylist)


(defun reload-file ()
  (interactive)
  (find-file (buffer-name)))


 ;; *********** Fuctions for edit special rc-files quickly ************

(defun edit-emacs ()
  (interactive)
  (find-file "~/.emacs"))

(defun edit-project ()
  (interactive)
  (find-file "~/.emacs.d/rc/99-proj.el"))

(defun edit-rcs ()
  "Jump to directory where rc files located"
  (interactive)
  (find-file "~/.emacs.d/rc/"))

(defun edit-functions ()
  "Jump to directory where rc files located"
  (interactive)
  (find-file "~/.emacs.d/rc/01-rc-functions.el"))

(defun edit-prog-mode ()
  "Jump to directory where rc files located"
  (interactive)
  (find-file "~/.emacs.d/rc/11-rc-prog-mode.el"))


(defun edit-template ()
  "Dired into template."
  (interactive)
  (find-file "~/.emacs.d/templates/yas-snippets/text-mode"))

 ;; ****************************** Copy Functions **************************

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end))
  )

(global-set-key (kbd "<M-S-SPC>") 'copy-line)

(defun copy-word (&optional arg)
  "Copy words at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1))
                    (point)))
        (end (progn (forward-word arg) (point))))
    (copy-region-as-kill beg end))
  )


(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
        (end (progn (forward-paragraph arg) (point))))
    (copy-region-as-kill beg end))
  )

 ;; ******************** Others ***************************************

(defun load-this-file ()
  (interactive)
  (load-file (buffer-name))
  )

(defun compile-this-file ()
  (interactive)
  (byte-compile (buffer-name))
  )

(defun lazy-set-key (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

(defun lazy-unset-key (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))

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
                    (or (getenv "CPPFLAGS")"-Wall -g ") file
                    (file-name-sans-extension file)
                    ))
        (if (or (equal (file-name-extension buffer-file-name) "c")
                (equal (file-name-extension buffer-file-name) "C")
                )
            (format "%s -o %s %s %s %s %s"
                    (or (getenv "CC") "gcc")
                    (file-name-sans-extension file)
                    (or (getenv "GTKFLAGS") "-Wall -g ")
                    (or (getenv "CPPFLAGS")"-DDEBUG=9  ")
                    (or (getenv "CFLAGS") "-Wall -g  ")
                    file)
          (if (or (equal (file-name-extension buffer-file-name) "tex")
                  (equal (file-name-extension buffer-file-name) "TEX"))
              (format "pdflatex %s" file )
            (if (or (equal (file-name-extension buffer-file-name) "sh")
                    (equal (file-name-extension buffer-file-name) "SH"))
                (format "./%s" file)
              nil
              )
            )
          )
        ))))

(defun do-compile ()
  "save buffers and start compile"
  (interactive)
  (save-some-buffers t)
  (setq compilation-read-command nil)
  ;; (compile (compile-command))
  (if (make-command)
      (compile (make-command))
    (compile compile-command)
    )

  (setq compilation-read-command t))

(defun open-makefile ()
  "Open and edit Makefile.description"
  (interactive)
  (setq fname (buffer-file-name))
  (find-file (concat (file-name-directory fname) "/Makefile"))
  )

(global-set-key (kbd "<f6>")  (lambda()(interactive)(do-compile)))
(global-set-key (kbd "<C-S-f6>")  (lambda()(interactive)(open-makefile)))

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
(global-set-key (kbd "C-S-o") 'zl-newline-up)


(defun my-list-bookmarks nil
  (interactive)
  (split-window-horizontally)
  (list-bookmarks))

(defun up-slightly ()
  (interactive) (scroll-up 3))
(defun down-slightly ()
  (interactive) (scroll-down 3))

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
          (or (string= (file-name-extension fname) "el")
              (and (string= (file-name-extension fname) "gz")
                   (string= (file-name-extension (file-name-sans-extension fname)) "el"))
              ))
      (try-compile-file fname)))

(defun try-compile-file(fname)
  (interactive)
  (message "Compliling ...")
  (if (string= fname (concat default-directory ".emacs"))
      (progn ;; Load it twice.
        (load-file fname)
        (load-file fname))
    (load-file fname))
  (byte-compile-file fname))

;;;; Make current-buffer 10 lines higher.
(defun my-adjust-window ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window 10))

(defun my-adjust-window-horizontal ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window 20 t))

(global-set-key (kbd"C-M-^") 'my-adjust-window)
(global-set-key (kbd "C-M->") 'my-adjust-window-horizontal)

;; date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defvar current-year-format "%Y"
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

(defun insert-current-year ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-year-format (current-time))))

(defun yyc/insert-current-buffername ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (buffer-file-name (current-buffer)))
  )

(defvar skeleton-pair-alist nil)
(defvar skeleton-pair t)
(setq skeleton-pair-alist
      '(
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
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-c-mode-left-brace)
  )

(defun base-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "“") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "（") 'skeleton-pair-insert-maybe)
  )
(add-hook 'text-mode-hook 'base-auto-pair)

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
(add-hook 'after-save-hook
          #'(lambda ()
              (and (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (save-match-data
                         (looking-at "^#!"))))
                   (not (file-executable-p buffer-file-name))
                   (shell-command (concat "chmod u+x " buffer-file-name))
                   (message
                    (concat "Saved as script: " buffer-file-name)))))

;;;; Untabify
(setq yyc/untabify-modes '(haskell-mode
                           scheme-mode
                           erlang-mode
                           python-mode
                           clojure-mode
                           text-mode))

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

(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

(defun yyc/txt-to-png ()
  "Change a txt file into png file using ditaa"
  (interactive)
  (let* ((fname (buffer-file-name))
         (txt2png-buf-name "*txt2png*")
         (png_file (concat (file-name-sans-extension fname) ".png"))
         )
    (if (file-exists-p png_file)
        (progn
          (message "Delete old png file ...")
          (delete-file png_file)))
    (get-buffer-create txt2png-buf-name)
    (pop-to-buffer txt2png-buf-name)
    (start-process "txt-to-png" txt2png-buf-name "java" "-jar"
                   (expand-file-name "~/.emacs.d/tools/ditaa.jar") fname)
    (message "This may take for a while, refer to *txt2png* to see whether it has been finished or not"))
  )

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)

(defun setup-font ()
  (interactive)
  (set-frame-font
   "-unknown-Monaco-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft YaHei" :size 16)))
  )

(defun yyc/comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and
current line is not blank and we are not at the end of the line, then
comment current line. Replaces default behaviour of comment-dwim, when it
inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
	  (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
	(comment-dwim arg)))

(global-set-key "\M-;" 'yyc/comment-dwim-line)

(defvar test_var nil "nil")

(defun test111 ()
  "description"
  (interactive)
  (setq test_var (expand-file-name "~/.emacs.d/tools/ditaa.jar"))
  (message test_var)
  )


(defvar num nil "nil")
(defvar fmt nil "nil")
(defun yyc/insert-line-num (prefix)
  "description "
  (interactive "sInput Prefix")
  (if (= (string-to-char prefix) 0)
      (setq fmt "\%d ")
    (setq fmt (concat (format "<%s" prefix) "%d> "))
    )
  (setq num 1)
  (backward-page)
  (insert (format fmt num))
  (while
      (< (forward-line 1) 1)
    (message (format "NUM: %d" num))
    (setq num (+ 1 num))
    (insert (format fmt num))
    )
  (message "Done!")
  )

(defun auto-rename-buffer ()
  "Rename current buffer to the basename"
  (interactive)
  (let ((newname (concat (buffer-name) "-" (format-time-string current-time-format (current-time)))))
         (rename-buffer newname))
  )

(require 'ediff)
(setq-default ediff-ignore-similar-regions t)
(setq ediff-split-window-function 'split-window-horizontally)

(defvar cfile nil "nil")
(defvar cbuffer nil "nil")
(defun ediff-quit-hook-func ( )
  ;; (delete-other-windows)
  (kill-buffer (file-name-nondirectory cfile))
  (if (file-exists-p cfile)
      (delete-file cfile))
  )

(defun my-ediff-revision (&optional file startup-hooks)
  "Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file entered at the prompt.
Default: the file visited by the current buffer.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
  ;; if buffer is non-nil, use that buffer instead of the current buffer
  (interactive "P")
  (save-excursion
    (setq cbuffer (current-buffer))
    (setq file (buffer-file-name cbuffer))
    (setq cfile (format "%s.~BASE~" file))
    (find-file file)
    (if (and (buffer-modified-p)
             (y-or-n-p (format "Buffer %s is modified. Save buffer? "
                               (buffer-name))))
        (save-buffer (current-buffer)))
    (add-hook 'ediff-quit-hook
              (lambda ()
                (remove-hook 'ediff-quit-hook 'ediff-quit-hook-func)
                (ediff-quit-hook-func)))
    (let (rev1 rev2)
      (ediff-load-version-control)
      (funcall
       (intern (format "ediff-%S-internal" ediff-version-control-package))
       "BASE" "" startup-hooks)))
  )

(defun yyc/list-attentions ()
  "List lines that need attentions, such as lines which include XXX or FIXME et.al."
  (interactive)
  (let ((wds "\"FIX|FIXME|TODO|BUG|XXX|YYC|HACK|zq|jimmy\""))
    (eshell-command (concat "egrep -i " wds " . -rI"))
    )
  )


 ;; Function used to add fields of struct into a dot file (for Graphviz).

;;;;; Dot templates

(defconst yyc/dot-head "subgraph cluster_%s {
    node [shape=record fontsize=12 fontname=Courier style=filled];
    color = lightgray;
    style=filled;
    label = \"%s %s\";
    edge[color=\"brown\"];"
  "Header part of dot file.")
(defconst yyc/dot-tail "
}"
  "Tail part of dot")
(defconst yyc/dot-node-head
  "
    node_%s[shape=record
            label=\"<f0>*** %s %s ***|\\"
  "Format of node.")
(defconst yyc/dot-node-tail "
\"];"
  "Format of node.")

(defconst attr_str "
<f%d>+%s : %s\\l|\\" "nil")

(defconst attr_func "
<f%d>-%s() : %s\\l|\\" "nil")

;;;;; Regular expressions to match a field of a struct.

(defconst r_attr_str "[ \t]+\\(.*+\\)[ \t]+\\(.*?\\);\\([ \t]*/[/\\*].]*\\)?$"
  "Regular expression for matching struct fields.")

(defconst r_name "\\_<\\(typedef[ \t]+\\)?\\(class\\|struct\\|union\\)[ \t]+\\(.*\\)?[ \t]*"
  "Regular expression for mating struct name")

(defconst r_func_l "\(.*"
  "Regular expression to match a function")
(defconst r_func_r ".*\)"
  "Regular expression to match a function")

(defconst r_comments "^[ \t/\\*][/\\*]+"
  "Regular expression to match a commentted field.")


(defconst r_struct_func
  "^[ \t]*\\(.+?\\)[ \t]*\(\\*\\(.*?\\)\)[ \t]*(\\(?:.\\|
\\)*?);"
  "Regular expression to match a function decleration in a struct.")

(defconst r_match_semicol
  (rx (+? anything) ";"))

(defconst r_match_attr
  (rx (+? (not (any "(" ")" "{" "}"))) ";"))

(defconst r_match_func
  (rx (+? (or alnum "_" blank)) "(*" (+? (or alnum "_")) ")"
      (zero-or-more blank) "(" (*? anything) ");"))

(defconst r_match_tag
  (rx (zero-or-more blank) (zero-or-one "typedef" (one-or-more  blank))
      (or "struct" "class" "union") (zero-or-more (or alnum "_" blank))
      (zero-or-one "
") (zero-or-more blank) "{"))

(defun get_struct_tag (decleration)
  "Abstract decleration from a string"
  (if (string-match r_name decleration 0)
      (list (match-string 2 decleration)
       (car (split-string (match-string 3 decleration) "{")))

    '("" "")))

(defun skip(msg x)
  (if x
      (message (format "Skip invalid syntax for function: %s." msg))
  (message (format "Skip invalid syntax for struct field: %s." msg))
  )
)

(defun yyc/struct-to-dot (start end)
  "generate c++ function definition and insert it into `buffer'"
  (interactive "rp")
  (let* ((tmp_str "")
         (var-name "")
         (var-type "")
         (counter 0)
         (next-begin 0)
         (pos-cur 0)
         (pname "")
         (ptype "")
         (header-str "")
         (pos-end 0)
         (var-defination (buffer-substring-no-properties start end))
         (item_str "")
         )
    (defun iter (pos)
      (if (string-match r_match_tag var-defination pos) ;; Declerad a struct
          (progn
            (setq pos-end (match-end 0))
            (setq item_str (substring var-defination pos pos-end))
            (setq ptype (car (get_struct_tag item_str)))
            (setq pname (nth 1 (get_struct_tag item_str)))
            (setq header-str
                  (format yyc/dot-head pname ptype pname))
            (setq tmp_str
                  (format yyc/dot-node-head pname ptype pname))
            (setq pos-cur (1+ pos-end))
            (iter pos-cur))
        (progn
          (if (string-match r_match_semicol var-defination pos)
              (progn
                (setq pos-end (match-end 0))
                (setq item_str (substring var-defination pos pos-end))
                (if (string-match r_match_func item_str 0) ;; Function
                    (progn
                      (if (string-match r_struct_func item_str 0)
                          (progn
                            (setq var-type
                                  (match-string 1 item_str))
                            (setq var-name
                                  (match-string 2 item_str))
                            (setq next-begin (match-end 0))
                            (if (string-match r_comments var-type 0) ;Comments
                                nil
                              (progn
                                (setq counter (+ counter 1))
                                (setq tmp_str
                                      (concat tmp_str
                                              (format attr_func
                                                      counter var-name var-type))))))
                        (skip item_str t)
                        )
                      )
                  (progn
                    (if (equal (string-match r_match_attr item_str 0) 0)
                        (progn
                          (if (string-match r_attr_str item_str 0)
                              (progn
                                (setq var-type
                                      (match-string 1 item_str))
                                (setq var-name
                                      (match-string 2 item_str))
                                (if (string-match r_comments var-type 0)
                                    nil
                                  (progn
                                    (setq counter (+ counter 1))
                                    (setq tmp_str
                                          (concat tmp_str
                                                  (format attr_str
                                                          counter var-name
                                                          var-type))))))
                            (skip item_str nil)))
                      (skip item_str nil))))
                (iter pos-end))))))
    (save-excursion
      (iter 0)
      (set-buffer (get-buffer-create "tmp.dot"))
      (graphviz-dot-mode)
      (goto-char (point-max))
      (insert  header-str tmp_str )
      (goto-char (point-max))
      (delete-char -1)
      (insert "<f999>\\"yyc/dot-node-tail yyc/dot-tail)
      )
    (if (one-window-p)
        (split-window-vertically))
    (switch-to-buffer-other-window "tmp.dot")
    (goto-char (point-min))
    )
  (message "Finished, please refer to *tmp.dot* buffer."))

(provide '01-rc-functions)
;;; emacs-rc-functions.el ends here