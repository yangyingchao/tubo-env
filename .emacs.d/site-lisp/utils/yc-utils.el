;;; yc-utils.el --- Brief introduction here.   -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(require 's)
(require 'use-package)

(require '02-functions)
(require '03-fundamental-mode)

(require 'ivy)
(require 'ivy-rich)


(defvar yc/marker-stack (make-ring 256)
  "List of functions to run to return to previous context.")

(defun yc/push-stack (&optional m)
  "Add marker M at the beginning of `yc/marker-stack'."
  (ring-insert yc/marker-stack (or m (point-marker))))


(defun yc/expand-macro (beg end)
  "Expand macro (from BEG to END) and put to `kill-ring'."
  (interactive "rp")
  (let ((str (buffer-substring-no-properties beg end)) )
    (with-temp-buffer
      (insert (format "(print (macroexpand '%s))"
                      str))
      (kill-new (format "%S" (eval (eval-sexp-add-defvars (elisp--preceding-sexp))))))))


(autoload 'yc/kill-proc "debug-utils" ".." t)

(defmacro yc/declare-kill-command (cmd)
  "Marco to generate commmand to kill process name of CMD."
  (interactive)
  `(defalias (intern ,(format "yc/kill-%s" cmd))
     (function (lambda (sig)
                 ,(format "Wrapper of yc/kill-proc to kill `%s' with signal SIG (as prefix)."
                          cmd)
                 (interactive "P")
                 (unless sig
                   (setq sig 9))
                 (yc/kill-proc sig ,cmd)))))

(yc/declare-kill-command "gdb")


(defun reload-file ()
  (interactive)
  (save-excursion
    (find-file (buffer-file-name))))

(defun reload-all-files ()
  (interactive)
  (save-excursion
    (let ((fn nil)
          (noconfirm t)
          (cur-buffer (current-buffer)))
      (dolist (buffer (buffer-list))
        (setq fn (buffer-file-name buffer))
        (when (and fn;; reload buffer if it is file or dir.
                   (not (verify-visited-file-modtime buffer)))
          (if (not (file-exists-p fn))
              (when (yes-or-no-p (format "File %s does not exist, delete buffer?" fn))
                (kill-buffer buffer))
            (message (format "Reloading file %s ..." (file-name-nondirectory fn)))
            (if (buffer-modified-p buffer)
                (setq noconfirm nil))
            (switch-to-buffer buffer)
            (revert-buffer t noconfirm))
          ))
      (switch-to-buffer cur-buffer)
      (message "All buffer reloaded..."))))

(defun yc/kill-ghost-buffers ()
  "Kill buffers not backed by files.."
  (interactive)
  (let ((buffers))
    (dolist (buffer (buffer-list))
      (when (and (not (buffer-modified-p buffer))
                 (buffer-file-name buffer)
                 (not (file-exists-p (buffer-file-name buffer))))
        (push (buffer-name buffer) buffers)
        (kill-buffer buffer)))

    (if buffers
        (message "Killed %d buffers: %s." (length  buffers)  (s-join ", " buffers))
      (message "No buffer is killed."))))

;; *********** Fuctions for edit special rc-files quickly ************

(defun edit-emacs ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs"))


;; ******************** Others ***************************************

(defun load-this-file ()
  (interactive)
  (load-file (buffer-name)))

(defun compile-this-file ()
  "Function to byte-compile current file."
  (interactive)
  (byte-compile (buffer-name)))

;;;; dos-unix
(defun dos-unix ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "dos2unix"))
        (let ((fn (buffer-file-name)))
          (call-process "dos2unix" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))))

(defun unix-dos ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "unix2dos"))
        (let ((fn (buffer-file-name)))
          (call-process "unix2dos" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))))


(defun yc/expand-color (color)
  "Expand COLOR of form #FFF into #FFFFFF."
  (mapconcat
   (lambda (X) (make-string (if (= X ?#) 1 2 )X)) color ""))

;;;###autoload
(defun yc/syntax-color-hex ()
  "Show color text of form #fffffff in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))
     ("#[abcdef[:digit:]]\\{3\\}\\>"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (yc/expand-color (match-string-no-properties 0))))))))
  (font-lock-flush))

(defun int-to-binary-string (i)
  "Convert an integer (I) into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res)
            i (lsh i -1)))
    (if (string= res "")
        "0" res)))

(defun yc/binary-form ()
  "Show binary format of current symbol."
  (interactive)
  (let* ((symbol (symbol-at-point))
         (str (if symbol (symbol-name symbol)))
         number)
    (unless symbol
      (error "Is cursor on a number?"))

    (when (string-match (rx (? (group "0x")) (group (+ hex))) str)
      (let ((base (if (match-string 1 str) 16 10)) ;; supports hex and dec
            (num (match-string 2 str)))
        (setq number (string-to-number num base))))
    (if number
        (message "Binary format of %s is: %s"
                 str (int-to-binary-string number))
      (error "Is cursor on a number??"))))

(defun yc/eval-and-kill ()
  "Evaluate last form and copy result to `kill-ring'."
  (interactive)
  (let* ((val (eval (eval-sexp-add-defvars (elisp--preceding-sexp))))
         (val-str
          (with-temp-buffer
            (print val (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp "\\`[ \t\n\r]+" nil t)
              (replace-match "" t t))
            (goto-char (point-max))
            (delete-char -1)
            (buffer-substring-no-properties (point-min) (point-max)))))

    (kill-new val-str)
    (message "%s" val-str)
    ))

(defun yc/eval-and-insert ()
  "Evaluate last form and insert result."
  (interactive)
  (let* ((val (eval (eval-sexp-add-defvars (elisp--preceding-sexp))))
         (pos (save-excursion (backward-list) (point)))
         (val-str
          (with-temp-buffer
            (print val (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp "\\`[ \t\n\r]+" nil t)
              (replace-match "" t t))
            (goto-char (point-max))
            (delete-char -1)
            (buffer-substring-no-properties (point-min) (point-max)))))

    (insert "\n\n  " val-str)
    (let ((new-pos (point)) )
      (goto-char pos)
      ;; (insert comment-start)
      (indent-region pos (+ new-pos (length comment-start))))))


(defun yc/eval-and-insert-comment ()
  "Evaluate last form and insert result with comment."
  (interactive)
  (let* ((val (eval (eval-sexp-add-defvars (elisp--preceding-sexp))))
         (pos (save-excursion (backward-list) (point)))
         (val-str
          (with-temp-buffer
            (print val (current-buffer))
            (goto-char (point-min))
            (while (search-forward-regexp "\\`[ \t\n\r]+" nil t)
              (replace-match "" t t))
            (goto-char (point-max))
            (delete-char -1)
            (buffer-substring-no-properties (point-min) (point-max)))))

    (insert "\n  " val-str)

    (let* ((new-pos (point))
           (c-start comment-start)
           (c-add comment-add))

      (goto-char pos)
      (insert (comment-padright c-start c-add ))
      (indent-region pos (+ new-pos (length comment-start))))))

;;;###autoload
(defun yc/pack-env ()
  "Pack everything..."
  (interactive)
  (let (pack-name )
    (defun pack-filter (process output)
      (when (string-match (rx "Final Package:" (+ space) (group (+ nonl)) eol)
                          output)
        (setq pack-name (match-string 1 output)))
      (PDEBUG "PROCESS:" process)
      (message "PACKENV: %s" output))

    (defun pack-sentinel (process event)
      (unless (featurep 's)
        (require 's))
      (message "Process: %s had the event %s.\n%s"
               process (s-join "" (s-split "\n" event))
               (shell-command-to-string (format "/usr/bin/stat %s" pack-name))))

    (let ((process (start-process-shell-command "PACKING" nil
                                                "~/.emacs.d/tools/pack_emacs_env.sh")))
      (set-process-filter process 'pack-filter)
      (set-process-sentinel process 'pack-sentinel))))

;;;###autoload
(defun yc/unpack-env ()
  "Pack everything..."
  (interactive)

  (let* ((fn (completing-read "Package: " nil nil nil
                              (cond
                               ((file-exists-p "~/emacs_packed.tar")  "~/emacs_packed.tar")
                               ((file-exists-p "~/tmp/emacs_packed.tar")  "~/tmp/emacs_packed.tar")
                               (t "~/emacs_packed.tar"))))
         (default-directory temporary-file-directory))

    (defun unpack-filter (process output)
      (PDEBUG "PROCESS:" process)
      (message "UNPACKENV: %s" output))

    (defun install-sentinel (process event)
      (if (string-match-p "finished" event)
          (progn
            (message "Installation finished, going to quit...")
            (sleep-for 3)
            (kill-emacs))
        (error "Process: %s had the event %s" process event)))

    (defun untar-sentinel (process event)
      (if (string-match-p "finished" event)
          (let ((default-directory (format "%s/emacs_packed" temporary-file-directory)) )
            (aif (start-process-shell-command
                  "UNPACKENV" nil
                  (format "/bin/bash %s/emacs_packed/unpack_emacs_env.sh" temporary-file-directory))
                (progn
                  (set-process-filter it 'unpack-filter)
                  (set-process-sentinel it 'install-sentinel))))
        (error "Process: %s had the event %s" process event)))

    (unless (file-exists-p fn)
      (error "File %s is not accessible" fn))

    (if (file-directory-p "emacs_packed")
        (delete-directory "emacs_packed" t))

    (aif (start-process "UNTAR" nil "tar" "xvf" (expand-file-name fn))
        (progn
          (set-process-filter it 'unpack-filter)
          (set-process-sentinel it 'untar-sentinel))
      (error "Failed to start untar process"))))


(defun debug-on ()
  "Turn on debug mode."
  (interactive)
  (update-debug-vars t))

(defun debug-off ()
  "Turn off debug mode."
  (interactive)
  (update-debug-vars nil))

(defun yc/delete-backtrace ()
  "Description."
  (interactive)
  (aif (get-buffer "*Backtrace*")
      (kill-buffer it)))


;;;; Add new line before or after current line.
(defun zl-newline nil
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun zl-newline-up nil
  (interactive)
  (beginning-of-line)
  (newline-and-indent))

(defun up-slightly ()
  (interactive) (scroll-up 3))

(defun down-slightly ()
  (interactive) (scroll-down 3))

(defun yc/insert-line-number ()
  "Insert line number."
  (interactive)
  (let ((number 1))
    (save-excursion
      (when (search-backward-regexp (rx bol (* blank) (group (+ digit)) ".") nil t)
        (setq number (1+ (string-to-number (match-string 1))))))
    (if (and (looking-at "$")
             (not (looking-back "^" nil)))
        (insert  "\n"))
    (insert (format "%d. " number))))

(defun kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun yc/fill-region ()
  "Description."
  (interactive)
  (if (region-active-p)
      (fill-region (region-beginning) (region-end))
    (save-excursion
      (goto-char (point-at-eol))
      (unless (looking-at-p "\n\n")
        (insert "\n")))
    (fill-region (point-at-bol) (point-at-eol)))
  )


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

(defun yc/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun yc/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(defun yc/insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun yc/insert-current-year ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-year-format (current-time))))

(defun yc/insert-current-buffername ()
  "Insert the filename of current buffer."
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(defun yc/copy-file-name ()
  "Copy full path of current file."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


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
(defun sort-uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "r")
  (let ((ref-line nil))
    (sort-lines nil beg end)
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line)))))


(defun uniq-region-internal (beg end)
  "description"
  (let ((ref-line nil))
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line))))
  )
(defun uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "rp")
  (uniq-region-internal beg end))

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

(defun yc/comment-dwim-line (&optional arg)
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

(defun yc/strip-ws-in-string (src)
  "description"
  (let ((str-array (split-string src split-string-default-separators)))
    (mapconcat 'identity str-array "_")))

(defun yc/update-info ()
  "Update info"
  (interactive)
  (when (member system-type '(windows-nt ms-dos)) ;; do this only for Windows, Linux will do it
    ;; automatically.
    (dolist (dir (append Info-directory-list Info-default-directory-list))
      (let ((cmd (format "bash -c \"cd %s && install-info --dir-file=dir \"" dir)))
        (dolist (fn (directory-files dir t))
          (if (file-directory-p fn)
              (message "Skipping directory: %s" fn)
            (message "Processing file: %s" fn)
            (shell-command (concat cmd (file-name-nondirectory fn)))))))))

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

(defun yc/decode-hex-color ()
  "Decode hex color"
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (? (or (: "0" (or "x" "X")) "#"))
                                 (group (= 2 hex)) (group (= 2 hex)) (group (= 2 hex)))))
      (skip-chars-backward "abcdefABCDEF0123456789xX#")
      (when (looking-at r-match-hex-color)
        (message "Color: (%d, %d, %d)"
                 (string-to-number (match-string 1) 16)
                 (string-to-number (match-string 2) 16)
                 (string-to-number (match-string 3) 16))
        ))))

(defun yc/encode-hex-color ()
  "Encode hex color."
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (? (group (+ digit)) "%"))))
      (skip-chars-backward "0123456789, %")
      (when (search-forward-regexp r-match-hex-color)
        (let* ((r (match-string 1))
               (g (match-string 2))
               (b (match-string 3))
               (a (match-string 4))
               (hexcolor (format (concat "#%02X%02X%02X" (if a "%02X" "%s"))
                                 (string-to-number r)
                                 (string-to-number g)
                                 (string-to-number b)
                                 (if (not a) ""
                                   (/ (* 255 (string-to-number a)) 100)))))
          (kill-new hexcolor)
          (message "Original color: %s, HexColor: %s, copied to yank!"
                   (match-string 0)
                   hexcolor))))))

(defun yc/reload-emacs ()
  "reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))



(defun yc/list-dir-transform (str)
  "Description."
  (if (file-directory-p str)
      (ivy-append-face str font-lock-doc-face)
    str))

(defun yc/ivy-rich-file-last-modified-time-and-size (candidate)
  (let* ((candidate (expand-file-name candidate ivy--directory))
         (attrs (file-attributes candidate))
         (size (file-attribute-size attrs)))
    (if (file-remote-p candidate)
        "?"
      (concat (format-time-string "%Y-%m-%d %H:%M:%S"
                                  (file-attribute-modification-time attrs))
              ", "
              (cond
               ((> size 1000000) (format "%.1fM" (/ size 1000000.0)))
               ((> size 1000) (format "%.1fk" (/ size 1000.0)))
               (t (format "%d" size)))
              "B"))))

(yc/eval-after-load
  "ivy-rich"
  (plist-put ivy-rich-display-transformers-list 'yc/list-directory
             '(:columns
               ((yc/list-dir-transform (:width 0.4))
                (yc/ivy-rich-file-last-modified-time-and-size
                 (:face font-lock-comment-face)))))

  (ivy-rich-mode -1)
  (ivy-rich-mode 1))

(ivy-set-actions  'yc/list-directory  yc/ivy-common-actions)


(defun yc/do-list-directory (dir &optional pattern name-only)
  "INTERNAL: List files in DIR with `counsel' that look like PATTERN.
If NAME-ONLY is t, returns full path of selected file, instead of opening it."
  (let ((default-directory dir))
    (ivy-read "Find file: "
              (let (dirs files)
                (dolist (item (directory-files dir))
                  (let ((path (concat (file-name-as-directory dir) item)) )
                    (PDEBUG "ITEM:" item
                            "PATH:" path
                            "DIR:" (file-directory-p path))
                    (cond
                     ((string=  "." item) nil)
                     ((file-directory-p path) (push path dirs))
                     (t
                      (when (or (not pattern)
                                (string-match-p pattern item))
                        (push path files))))))

                (append (sort dirs 'string-lessp) (sort files 'string-lessp)))

              :action (lambda (cand)
                        (interactive)
                        (if (file-directory-p cand)
                            (yc/do-list-directory cand pattern name-only)
                          (if name-only
                              (throw 'p-found cand)
                            (find-file cand))))

              :caller 'yc/list-directory)))

(defun yc/list-directory (dir &optional pattern name-only)
  "List files in DIR with `counsel' that look like PATTERN.
If NAME-ONLY is t, returns full path of selected file, instead of opening it."
  (catch 'p-found
    (yc/do-list-directory dir pattern name-only)))


(defun yc/choose-directory (&optional dir)
  "Choose directory (with DIR as default one)."
  (interactive)
  (let* ( (suggestion (or dir default-directory))
          (choices (list
                    (format "Choose directory %s" suggestion)
                    "Choose by selecting directory interactively."))
          (action-index (cl-position
                         (completing-read (format "%s is not part of any project. Select action: "
                                                  (buffer-name))
                                          choices
                                          nil
                                          t)
                         choices
                         :test 'equal))
          (project-root (cl-case action-index
                          (0 suggestion)
                          (1 (read-directory-name "Select workspace folder to add: "
                                                  (or suggestion default-directory)
                                                  nil
                                                  t))
                          (t nil))))
    project-root))

(defun edit-rcs ()
  "Edit rc files.."
  (interactive)
  (yc/list-directory "~/.emacs.d/rc" "^[0-9]+.*?\.el"))

(defun edit-zsh ()
  "Edit rc files.."
  (interactive)
  (yc/list-directory "~/.zshrc.d" "^[0-9]+.*?sh"))

(defun edit-sway ()
  "Edit sway configuration files.."
  (interactive)
  (yc/list-directory "~/.config/sway"))

(defun edit-template ()
  "Edit templates."
  (interactive)
  (yc/list-directory "~/.emacs.d/templates" (rx (or alnum "_"))))

(defun edit-snippets ()
  "Edit snippets."
  (interactive)
  (yc/list-directory (format "~/.emacs.d/templates/yasnippets/%s"
                             major-mode)))

(defun edit-peda ()
  "Edit peda.py."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/tools/peda/peda.py")))


(defun yc/add-subdirs-to-load-path ()
  "Description."
  (interactive)
  (let ((pwd (substring default-directory 0 -1)))
    (unless (member (file-name-base pwd) '(".git" ".svn" "CVS"))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

 ;; xmind

(defun xmind/trans-node (node level)
  "Translate a node."
  (let ((yc/debug-log-limit -1))
    (PDEBUG "NODE: " node
      "CHILDREN: " (gethash "children" node)))

  (let* ((result nil)
         (title (gethash "title" node))
         (tags (gethash "labels" node))
         (children (aif (gethash "children" node)
                       (gethash "attached" it)))
         (txt-indent (make-string  (if (> level 2) (* (- level 2) 2) 0) ? )))

    (PDEBUG "children: " (append children))

    ;; header...
    (push (concat (if (> level 2)
                      (concat (make-string (* (- level 3) 2) ? ) "-")
                    (make-string level ?*))
                  " " title
                  (when tags
                    (concat "		:" (mapconcat 'identity tags ":") ":")))
          result)

    (awhen (gethash "notes" node)
      (push
       (mapconcat (lambda (x)
                    (concat txt-indent x))
                  (s-split "\n" (gethash "content" (gethash "plain" it))) "\n")
  result))

    (seq-doseq (child  children)
      (push (xmind/trans-node child (1+ level)) result))

    (mapconcat 'identity (nreverse result) "\n\n")))

(defun xmind/convert-to-org (&optional input)
  "Description."
  (interactive)
  (let* ((input (or input
                    (yc/list-directory default-directory ".*\.xmind" t)))
         (temp-dir (concat (temporary-file-directory) (make-temp-name "xmind_")))
         (default-directory temp-dir)
         (json-file (concat temp-dir "/content.json" )))

    (condition-case var
        (progn

          (unless (executable-find "unzip")
            (error "Tool unzip is needed"))

          (make-directory temp-dir)
          (shell-command-to-string (format "unzip \"%s\"" input ))
          (unless (file-exists-p json-file)
            (error "Can't find file: %s" json-file))

          (let* ((json
                  (with-temp-buffer
                    (insert-file-contents  json-file)
                    (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                       :object-type 'hash-table
                                       :null-object nil
                                       :false-object nil)))
                 (root (gethash "rootTopic" (elt json 0)))
                 (title (or (gethash "title" root) "Untitled"))
                 (children (aif (gethash "children" root)
                               (gethash "attached" it)))
                 (org-file (concat (file-name-directory input) title ".org")))

            (unless (= (length json) 1)
              (error "Does not support multiple layers for now"))


            (with-temp-file org-file
              ;; TITLE & Headers
              (insert "#+TITLE: " title "\n")
              (insert "#+OPTIONS:  ^:nil H:7 num:t toc:2 \\n:nil ::t |:t -:t f:t *:t tex:t d:(HIDE) tags:not-in-toc\n")
              (insert "#+STARTUP:  align nodlcheck oddeven lognotestate\n")
              (insert "#+SEQ_TODO: TODO(t) INPROGRESS(i) WAITING(w@) | DONE(d) CANCELED(c@)\n")
              (insert "#+TAGS:     noexport(n) HIGH(h) MEDIUM(m) LOW(l)\n")
              (insert "#+EXCLUDE_TAGS: noexport\n")

              (insert (concat "#+HTML_HEAD: <style type=\"text/css\">"
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "~/.emacs.d/templates/assets/css/style.css"))
                                (catch 'done
                                  (while t
                                    (goto-char (point-min))
                                    (unless (looking-at-p (rx (+? nonl) "\n"))
                                      (throw 'done t))
                                    (while (search-forward-regexp (rx (* space) "\n" (* space)) nil t)
                                      (replace-match " "))))

                                (buffer-substring-no-properties (point-min) (point-max))
                                )

                              " </style>\n\n"
                              ))


              (insert (mapconcat
                       (lambda (x)
                         (xmind/trans-node x 1))
                       children
                       "\n\n"))
              (message "File save to: %s" org-file)))

          )
      (error (message "Faild: %s" var)))

    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))))

 ;; http

(defun http/get-body (url)
  "Return http body from URL."
  (let* ((buffer (url-retrieve-synchronously url))
         (r-match-header
          (rx "HTTP/" digit "." digit (+ space)          ;; HTTP/1.1
              (group (+ digit)) (+ space) (+? nonl) "\n" ;; 200 OK
              (group (+? anything))                      ;; All fields
              "\n\n")))

    (with-current-buffer buffer
      (goto-char (point-min))
      (if (search-forward-regexp r-match-header)
          (let* ((code (string-to-number (match-string 1)))
                 (body-begin (+ (match-end 2) 2)))
            (unless (= code 200)
              (error "Http code is not 200, but: %d!, url: %s" code url))

            (buffer-substring-no-properties body-begin (point-max)))
        (error "Failed to parse from contents: %s"
               (buffer-substring (point-min) (min 512 (point-max))))))))

(defun http/parse-kvps (content)
  "Parse CONTENT into hash table."
  (PDEBUG "Got content: " content)
  (let ((ht (make-hash-table :test 'equal))
        (r-match-kv
         (rx (group (+ (not space))) "="
             (group (or "'" "\""))
             (group (+? nonl) )
             (backref 2)))
        (pos 0))

    (while (string-match r-match-kv content pos)
      (let ((key (match-string 1 content))
            (val (match-string 3 content))
            (end (match-end 0)))

        (PDEBUG "TYPE: " key
          "VAL: " val)
        (puthash key val ht)

        (setq pos end)))
    ht))

(defun http/fix-url (url main-url)
  "Fix url."
  (let* ((r-match-url (rx bol  (group
                                (or "http" "https")
                                "://" (+? nonl) (? ":" (+ digit)))
                          (? "/" (* nonl)) eol))
         (base-url (if (string-match r-match-url main-url)
                       (match-string 1 main-url))))

    (cond
     ((string-match-p (rx bol (or "http" "https") "://") url) url)
     ((s-starts-with-p "/" url)  (concat base-url url))
     ((s-starts-with-p "." url)  (concat (file-name-directory main-url) "/" url))
     (t (error "url: %s" url))
     )
    )

  )


(defun yc/http-save-page (&optional url)
  "Description."
  (interactive)
  (let* ((url (or url
                  (read-from-minibuffer "url: ")))
         (r-match-tag
          (rx (* space) "<"
              (group (or "img" "link"))
              (group (+? nonl))  (? "/") ">"))
         filename)

    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (http/get-body url))

      ;; decide file name
      (goto-char (point-min))
      (if (search-forward-regexp (rx "<title>" (group (+? nonl)) "</title") nil t)
          (setq filename (concat (match-string 1) ".html"))
        (setq filename "yc_tmp_file.html"))


      ;; Embed something if possible...
      (goto-char (point-min))
      (while (search-forward-regexp r-match-tag nil t)
        (PDEBUG "PT: " (point))
        (let* ((begin (match-beginning 0))
               (end (match-end 0))
               ;; (f (PDEBUG "KKK " (buffer-substring-no-properties begin end)))
               (type (match-string 1))
               (content (match-string 2))
               (ht (http/parse-kvps content))
               to-insert)


          (PDEBUG "HANDLING: " type)

          (cond
           ((string= type "link")
            ;; parse link and replace contents

            (unless (looking-at-p "\n")
              (insert "\n"))
            (let* ((rel (gethash "rel" ht)))
              (PDEBUG "REL: " rel)

              (setq to-insert
                    (cond
                     ((string= rel "stylesheet")
                      (let ((css-url (http/fix-url (gethash "href" ht) url)))
                        (format "<style>\n%s\n</style>\n"
                                (http/get-body css-url))))
                     (t
                      (PDEBUG
                        "Unhandled rel: " rel
                        "link: " (gethash "href" ht))
                      nil)))))

           ;; download and embedded it.
           ((string= type "img")
            (let* ((img-url (gethash "src" ht))
                   ;; (ff (PDEBUG "IMG" img-url))
                   (ext (file-name-extension img-url)))

              (unless (or (string-match-p "data:image/.*?;base64" img-url)
                          (not (member (intern ext) '(png jpg gif))))

                (push "<img" to-insert)
                (push (format "src=\"data:image/%s;base64, %s\""
                              ext
                              (base64-encode-string
                               (http/get-body (http/fix-url img-url url))))
                      to-insert)
                ;; append other attributes.
                (maphash (lambda (k v)
                           (unless (string= k "src")
                             (push (format "%s=\"%s\"" k v) to-insert)))
                         ht)

                (push "/>" to-insert)

                (setq to-insert
                      (mapconcat 'identity (nreverse to-insert) " ")))))
           (t

            (progn
              (PDEBUG "Unhandled type: " type)
              nil
              )))

          (when to-insert
            (delete-region begin end)
            (goto-char begin)
            (insert to-insert))
          ))

      (write-file filename))

    (message "File saved to: %s" filename)))

 ;; function to set up compilers.

(cdsq yc/compiler-env
  '(("clang" . (("CC"  . "clang")
                ("CXX" . "clang++")
                ("CPPFLAGS" . nil)
                ("LDFLAGS" . nil)))
    ("gcc"   . (("CC"  . "gcc")
                ("CXX" . "g++")
                ("CPPFLAGS" . nil)
                ("LDFLAGS" . nil)))
    ("clang-asan" . (("CC"  . "clang")
               ("CXX" . "clang++")
               ("CPPFLAGS" . "-fsanitize=address -fno-omit-frame-pointer -DVALGRIND")
               ("LDFLAGS" . "-fsanitize=address -lgcc_s")))

    ("gcc-asan" . (("CC"  . "gcc")
                     ("CXX" . "g++")
                     ("CPPFLAGS" . "-fsanitize=address -fno-omit-frame-pointer -DVALGRIND")
                     ("LDFLAGS" . "-fsanitize=address -lasan")
                     )))
  "Environment variables for compilers.")

(defun yc/choose-compiler ()
  "Choose proper compiler."
  (interactive)
  (ivy-read "Compiler: " yc/compiler-env
            :action
            (lambda (x)
              (dolist (pair (cdr x))
                  (PDEBUG "ENV: " pair)
                  (setenv (car pair) (cdr pair))))
            :require-match 'confirm-after-completion
            :caller 'counsel-find-file))

(defun yc/show-compilers ()
  "Display selected compilers.."
  (interactive)
  (message "CC: %s, CXX: %s, CPPFLAGS: %s, LDFLAS: %s"
           (getenv "CC") (getenv "CXX")
           (getenv "CPPFLAGS") (getenv "LDFLAGS")))

(defun yc/update-env-from ()
  "Collect and update environment variables..."
  (interactive)
  (let ((target (read-file-name "Select folder or file to add: "
                                     default-directory nil t)))

    (cond
     ((file-directory-p target)
      (let ((env-list '(("PATH"            . ("bin" "sbin"))
                        ("LD_LIBRARY_PATH" . ("lib" "lib64"))
                        ("PKG_CONFIG_PATH" . ("lib/pkgconfig" "lib64/pkgconfig"))))
            updated)
        (if (s-ends-with-p "/" target)
            (setq target (substring-no-properties target 0 -1)))
        (dolist (p env-list)
          (awhen (mapcar
                  (lambda (x)
                    (yc/file-directory-p (concat target "/" x)))
                  (cdr p))
            (setenv (car p) (concat (mapconcat 'identity it ":") ":" (getenv (car p))))
            (push (car p) updated)))

        (message "Updated variables: %s" (mapconcat 'identity (nreverse updated) ", "))))
     ((file-exists-p target)
      (yc/load-shell-env-from-file target))
     (t (error "??"))))
  (yc/update-exec-path)
)

(defalias 'yc/uef 'yc/update-env-from)


(defvar-local diff-file-regions nil)
(defvar yc/current-diff-buffer nil "Nil.")

(defun yc/ediff-cleanup-and-restore ()
  "Clean up buffers and restory layout..."
  (interactive)

  ;; First, remove it from hook to avoid killing other buffers..
  (remove-hook 'ediff-quit-hook 'yc/ediff-cleanup-and-restore)

  ;; copy file name
  (yc/ediff-copy-file-name-A)

  ;; kill buffers..
  (save-some-buffers t)
  (kill-buffer ediff-buffer-A)
  (kill-buffer ediff-buffer-B)
  (kill-buffer ediff-buffer-C)

  (ws-butler-global-mode 1)

  (widen)
  ;; display original buffer.
  (layout-restore))

(defun yc/view-with-ediff (&optional clear &rest backward)
  "Description."
  (interactive "P")

  (if clear
      (setq diff-file-regions nil))

  ;; For now, simply scan whole file.
  ;; TODO: if file is too large, scan part of it....
  (unless diff-file-regions
    (save-excursion
      (goto-char (point-min))
      (let ((r-match-file
             (rx
              (: bol
                 (= 3 (or "*" "+" "-")) space
                 (group (+? nonl)) ;; file
                 space
                 (: (+? nonl)
                    space
                    (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)  ;; time
                    (? "." (+ digit))
                    space (+? nonl)
                    )
                 "
"                 (= 3 (or "*" "+" "-")) space
                 (group (+? nonl)) ;; file
                 space
                 (: (+? nonl)
                    space
                    (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)  ;; time
                    (? "." (+ digit))
                    space (+? nonl)
                    )
                 "
"
                 ))))

        (while (search-forward-regexp r-match-file nil t)
          (let ((start (match-beginning 0))
                (file-A (match-string-no-properties 1))
                (file-B (match-string-no-properties 2)))
            (PDEBUG "FILES: " file-A " " file-B)

            (when diff-file-regions
              (setf (caaar diff-file-regions) (1- start)))

            (push (cons (cons nil start) (cons file-A file-B)) diff-file-regions)))

        (setf (caaar diff-file-regions) (1- (point-max)))))

    (unless diff-file-regions
      (error "Failed to parse regions"))

    (PDEBUG "DIFF-FILE-REGIONS:" diff-file-regions))

  (let* ((ediff-ignore-similar-regions nil)
         (pt (point))
         (item (catch 'p-found
                 (dolist (item diff-file-regions)
                   (let ((region (car item)))
                     (when (and
                            (>= pt (cdr region))
                            (<= pt (car region)))
                       (throw 'p-found item)))))))

    (if item
        (let* ((region (car item))
               (files (cdr item))
               (file-A (car files))
               (file-B (cdr files))

               skip-A
               skip-B)


          (save-excursion
            (narrow-to-region (car region) (cdr region))

            ;; parse A
            (goto-char (point-min))
            (let ((start 0))
              (while (search-forward-regexp
                      (rx bol "***" (+ space)
                          (group (+ digit)) "," (group (+ digit))
                          (+ space) "***") nil t)
                (let ((diff-start (match-string 1))
                      (diff-end (match-string 2)))
                  (push (cons start (string-to-number diff-start)) skip-A)
                  (setq start (string-to-number diff-end))))
              (push (cons start (point-max)) skip-A))


            ;; parse B
            (goto-char (point-min))
            (let ((start 0))
              (while (search-forward-regexp
                      (rx bol "---" (+ space)
                          (group (+ digit)) "," (group (+ digit))
                          (+ space) "---") nil t)
                (let ((diff-start (match-string 1))
                      (diff-end (match-string 2)))
                  (push (cons start (string-to-number diff-start)) skip-B)
                  (setq start (string-to-number diff-end))))
              (push (cons start (point-max)) skip-B))

            (widen))

          ;; move cursor to next section if possible...
          (goto-char
           (if backward
               (1- (cdar item))
             (1+ (caar item))))

          (ws-butler-global-mode -1 )
          (add-hook 'ediff-quit-hook 'yc/ediff-cleanup-and-restore)
          (PDEBUG "skips:" skip-A skip-B)
          (let ((buffer-A (find-file-noselect file-A))
                (buffer-B (find-file-noselect file-B)))

            ;; (with-current-buffer buffer-A
            ;;   (goto-char (point-min))
            ;;   (dolist (region skip-A)
            ;;     (overlay-put (make-overlay (line-beginning-position (car region))
            ;;                                (line-end-position (cdr region)))
            ;;                  'invisible 'invs)))


            ;; (with-current-buffer buffer-B
            ;;   (goto-char (point-min))
            ;;   (dolist (region skip-B)
            ;;     (overlay-put (make-overlay (line-beginning-position (car region))
            ;;                                (line-end-position (cdr region)))
            ;;                  'invisible 'invs)))

            (ediff-buffers  buffer-B buffer-A)))

      (error "Can't find proper files to compare at point: %d" (point)))))

(defun yc/get-cpu-cores ()
  "Return number of core."
  (cl-case system-type
    ((gnu/linux cygwin)
     (with-temp-buffer
       (ignore-errors
         (when (zerop (call-process "bash" nil t nil "-c" "cat /proc/cpuinfo| grep '^processor' | wc -l"))
           (string-to-number (buffer-string))))))
    ((gnu/kfreebsd darwin)
     (with-temp-buffer
       (ignore-errors
         (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
           (string-to-number (buffer-string)))))
     )
    ((windows-nt)
     (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
       (when number-of-processors
         (string-to-number number-of-processors))))))

(defun yc/get-compiling-threads ()
  "Return proper number of threads."
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    (let ((threads (yc/get-cpu-cores)))
      (if (> threads 16)
          (truncate (* threads 0.75)) ;; leave some cores for other users...
        threads))))

(defun yc/get-env (env &optional func &rest backups)
  (aif (getenv env)
      it
    (when (not func) (setq func 'identity))
    (catch 'p-found
      (dolist (var backups)
        (when (funcall func var)
          (throw 'p-found var)))
      nil)))

(defun yc/kill-file-ln ()
  "Copy filename and line number."
  (interactive)
  (aif (buffer-file-name)
      (kill-new (format "%s:%d" (file-name-nondirectory it)
                        (line-number-at-pos)))))

(defun yc/touch-file ()
  "Execute touch command to selected file."
  (interactive)
  (yc/list-directory
   default-directory nil
   (lambda (x)
     (interactive)
     (shell-command (format "touch \"%s\"" x))
     (message "File %s touched." x))))

(defun yc/open-with-external-app (&optional file)
  "Open FILE with external app."
  (interactive)
  (let ((flist
         (cl-case major-mode
           ('dired-mode (dired-get-marked-files))
           (t (list (or file (buffer-file-name))))))
        (app
         (cond
          ((eq system-type 'darwin) "open")
          ((eq system-type 'gnu/linux) "xdg-open")
          (t nil))))
    (unless flist
      (error "Not file to operate"))
    (if app
        (dolist (fn flist)
          (PDEBUG "Open with cmd: " app fn)
          (start-process "xdg-open" nil app fn))
      (error "Can't find proper app to open file %s" file))))

(autoload 'magit-git-string "magit-git")

(defun yc/git-copy-file-path ()
  "Copy path of current visited file."
  (interactive)

  (let* ((remote-info (s-split "/" (magit-git-string "rev-parse" "--abbrev-ref"
                                                    "HEAD@{upstream}")))
        (branch (cadr remote-info))
        (remote (car remote-info))
        (url (magit-git-string "config" (concat "remote." remote ".url")))
        (root (magit-toplevel)))

    (PDEBUG "REMOTE-INFO:" remote-info)
    (unless url (error "Not in a git repo"))

    (when (s-ends-with-p ".git" url)
      (setq url (substring url 0 -4)))

    (when (string-match (rx "git@" (group (+? nonl))":" (group (+? nonl)) eol)
                    url)
      (let* ((host (match-string 1 url))
             (proj (match-string 2 url))
             (protol (if (s-starts-with-p "192.168." host) "http" "https")))

        (setq url (concat protol "://" host "/" proj)))
      )

    (let ((ret (concat url "/blob/" branch "/"
                       (and root (file-relative-name buffer-file-name root))
                       (if current-prefix-arg (format "#L%d" (line-number-at-pos))))))

      (kill-new ret)
      ret)))

(defun yc/git-copy-proj-url (&optional dir)
  "Copy path of current visited file or input DIR."
  (interactive)

  (let* ((default-directory (or dir default-directory))
         (url (magit-git-string "config" "remote.origin.url"))
         (commit (magit-git-string "rev-parse" "--short" "--revs-only"  "HEAD" )))

    (unless url (error "Not in a git repo"))

    (when (string-match (rx "git@" (group (+? nonl))":" (group (+? nonl)) eol)
                    url)
      (let* ((host (match-string 1 url))
             (proj (match-string 2 url))
             (protol (if (s-starts-with-p "192.168." host) "http" "https")))

        (setq url (concat protol "://" host "/" proj))))

    (when (called-interactively-p '(interactive))
      (kill-new commit)
      (kill-new url))

    (concat url "#commit=" commit)))

(defun yc/command-output-to-string (&rest args)
  "Execute a command and return result as string.
args should be a list, but to make caller's life easier, it can accept one atom instead of a
  list."
  (let* ((cmd (car args))
         (args (cdr args))
         (cmd-output (with-output-to-string
                       (with-current-buffer standard-output
                         (apply #'process-file
                                cmd
                                nil (list t t) nil
                                (if (listp (car args))
                                    (car args)
                                  args))))))
    (s-trim (ansi-color-apply cmd-output))))


(defun yc/adjust-window-calc-lines ()
  "Calculate lines to adjust."
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    10))

;;;; Make current-buffer 10 lines higher.
(defun yc/enlarge-window ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window (yc/adjust-window-calc-lines)))

(defun yc/shrink-window ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window (- (yc/adjust-window-calc-lines))))

(defun yc/enlarge-window-horizontal ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window (yc/adjust-window-calc-lines) t))

(defun yc/shrink-window-horizontal ()
  "Adjust window quickly."
  (interactive)
  (enlarge-window (- (yc/adjust-window-calc-lines)) t))

(defun yc/open-eshell ()
  "DOCSTRING."
  (interactive)
  (let ((ebuffer (get-buffer "*eshell*"))
        (dir (expand-file-name default-directory)))
    (if ebuffer
        (progn
          (set-buffer ebuffer)
          (goto-char (point-max))
          (insert "cd " dir)
          (eshell-send-input)
          (pop-to-buffer ebuffer)
          (goto-char (point-max)))
      (eshell))))

(defun yc/exec-command-via-eshell ()
  "Open eshell and execute command."
  (interactive)
  (let ((command (read-shell-command "Shell command: "
                                         nil nil
			                 (let ((filename
			                        (cond
				                 (buffer-file-name)
				                 ((eq major-mode 'dired-mode)
				                  (dired-get-filename nil t)))))
			                   (and filename (file-relative-name filename))))))
    (with-current-buffer (yc/open-eshell)
      (insert command)
      (eshell-send-input))))

 ;; yasnippet.

(defun yc/new-snippet (name)
  "Create snippet for current mode."
  (interactive "sSnippet Name: ")
  (let* ((mode-mapping
          (list (cons 'lisp-interaction-mode 'emacs-lisp-mode)
                (cons 'c-mode 'cc-mode)))
         (mode (or (cdr (assq major-mode  mode-mapping)) major-mode))
         (priv (if (y-or-n-p "Is this public snippet? ") "" "-private"))
         (dirname (expand-file-name
                   (format "~/.emacs.d/templates/yasnippets%s/%s" priv (symbol-name mode))))
         (filename (concat dirname "/" name)))
    (unless (file-directory-p dirname) (mkdir dirname t))
    (find-file filename)))



(defun yc/html-remove-tags (start end)
  "Remove tags from region (START - END)."
  (interactive "rp")
  (when (region-active-p)
    (let ((orig (buffer-substring-no-properties start end))
          (r-match-tag (rx
                        (or (: "<" (? "/") (group (+? nonl)) ">")
                            (: bol (group (+ space)))))))
      (with-temp-buffer
        (insert orig)
        (goto-char (point-min))
        (while (search-forward-regexp r-match-tag nil t)
          (replace-match ""))

        (fill-region (point-min) (point-max))
        (kill-new (buffer-substring-no-properties (point-min) (point-max)))))
    (deactivate-mark)))

(defun yc/html-to-org ()
  "Turn region from html to org."
  (interactive)
  (when (region-active-p)
    (let ((orig (buffer-substring-no-properties (region-beginning) (region-end)))
          (r-match-tag (rx
                        (or (: "<" (? "/") (group (+? nonl)) ">")
                            (: bol (group (+ space)))))))
      (with-temp-buffer
        (insert orig)
        (goto-char (point-min))

        ;; handle TAGS
        (while (search-forward-regexp r-match-tag nil t)
          (let* ((tag (match-string 1))
                 (replace
                  (cond
                   ((member tag '("varname" "command"))
                    "=")
                   ((member tag '("emphasis"))
                    "*")
                   (t ""))))

            (replace-match replace)))

        ;; handle special characters
        (dolist (p '(("&copy;" . "(c)")
                     ("&amp;" . "&")
                     ("&lt;" . "<" )
                     ("&gt;" . ">" )))
          (goto-char (point-min))
          (while (search-forward (car p) nil t)
            (replace-match (cdr p) nil t)))

        ;; fill region and put into kill-ring
        (fill-region (point-min) (point-max))
        (kill-new (buffer-substring-no-properties (point-min) (point-max)))))
    (deactivate-mark)))

(defun yc/make-file-writable ()
  "Make file writable.
Should be run from find-file-hook, change write permissions."
  (interactive)
  (when (and buffer-file-name
             (file-exists-p buffer-file-name))
    (let ((attr (file-attributes buffer-file-name))
          (msg (format "Make file: %s writable... " buffer-file-name)))
      ;; Change file mode if this file belongs to me, and it is writeable.
      (when (and (not (car attr))
                 (= (user-uid) (caddr attr))
                 (not (file-writable-p buffer-file-name)))
        (cond
         ((executable-find "chmod")
          (progn
            (call-process (executable-find "chmod") nil nil nil "+w"
                          buffer-file-name)))
         (t (chmod buffer-file-name
                   (file-modes-symbolic-to-number
                    "u+w" (file-modes buffer-file-name)))))
        (setq msg (concat msg (if (file-writable-p buffer-file-name)
                                  "Succeeded\n" "Failed\n" )))
        (message msg)))))

(defun yc/list-attentions ()
  "List items needs attention.
If `current-prefix-arg' is given, search for all files under default-folder."
  (interactive)
  (let ((r-match-attentions (rx bow (or "xxx" "yyc" "todo" "bug") ":")))
  (if (eq major-mode 'dired-mode)
      (yc/counsel-grep "'\\b(bug:\|todo:\|xxx:\|yyc:)'")
    (counsel-grep-or-swiper r-match-attentions))))

(defun yc/get-avaiable-port ()
  "Description."
  "Return a port unused."
  (with-temp-buffer
    (ignore-errors
      (when (zerop (call-process "python" nil t nil "-c"
                                 "import socket; s=socket.socket(); s.bind(('', 0)); print(s.getsockname()[1]); s.close()"))
        (string-to-number (buffer-string))))))

(defun yc/open-fds ()
  "Open /proc/PID/fds..."
  (interactive)
  (let* ((name (buffer-name (current-buffer)))
         (pid (if (string-match (rx "*gdb" (+ space)
                                    (group (+ digit)) (+ space)
                                    "shell*")
                                name)
                  (match-string 1 name)
                (read-from-minibuffer "Input PID: "))))
    (PDEBUG "NAME:" name  "PID: " pid)
    (awhen (yc/file-directory-p (concat "/proc/" pid "/fd"))
      (find-file it))
    )
  )

(defun yc/start-http-server ()
  "Start http server on directory."
  (interactive)
  (let ((default-directory (yc/choose-directory))
        (port (or current-prefix-arg 8000)))

    (start-process-shell-command
     "http-server"
     (get-buffer-create "http-server")
     (concat (format "python -m SimpleHTTPServer %d" port)" || python3 -m http.server")
     )))

(defun yc/clean-shared-memory ()
  "Remove all shared memory owned by me."
  (interactive)
  (shell-command "ipcs -m | grep yyc | awk '{print $2}' | xargs ipcrm shm {} \;"))

(defun yc/hex-to-dec (input)
  "Prints the decimal value of a hexadecimal string under cursor.

Samples of valid input:

  ffff → 65535
  0xffff → 65535
  #xffff → 65535
  FFFF → 65535
  0xFFFF → 65535
  #xFFFF → 65535

more test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600

URL `http://ergoemacs.org/emacs/elisp_converting_hex_decimal.html'
Version 2020-02-17"
  (let (tempStr)
    (let ((case-fold-search nil))
      (setq tempStr (replace-regexp-in-string "\\`0x" "" input )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr )) ; CSS …
      )
    (string-to-number tempStr 16)))

(defun auto-rename-buffer ()
  "Rename current buffer."
  (interactive)
  (let ((newname (concat (buffer-name) "-" (format-time-string  "%H:%M:%S" (current-time)))))
    (rename-buffer newname)))


(provide 'yc-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yc-utils.el ends here
