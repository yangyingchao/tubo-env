;;; 02-functions.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;; Functions that are needed by other init files.
;;; Utility functions, can be added to yc-util.el.
;;; Code:

 ;; Macros
(defmacro yc/defmacro (name &rest args)
  "Define macro with NAME and ARGS.
This is macro to generate macro, and put it to proper indentation."
  (declare (doc-string 3) (indent 2))
  `(progn
     (put (quote ,name) 'lisp-indent-function 'defun)
     (defmacro ,name ,@args)))

(defmacro cdsq (sym val &optional doc)
  "Customize, Define or Set value of SYM to VAL, with DOC as document."
  (declare (doc-string 3) (indent 2))
  `(if (boundp ',sym)
       (setq ,sym ,val)
     (defvar ,sym ,val ,doc)))

(defmacro aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(yc/defmacro awhen (test &rest body)
  "When TEST is non-nil, execute BODY, with it bound to result of TEST."
  `(let ((it ,test))
     (when it
       ,@body)))

(yc/defmacro yc/eval-after-load (name &rest args)
  "Macro to set expressions in `arg` to be executed after `name` is loaded."
  `(eval-after-load ,name
     ',(append (list 'progn
                     `(let ((ts (current-time)))
                        (message "Loading configuration for: %s..." ,name)
                        ,@args
                        (message "Configuration for %s finished in %.2f seconds" ,name
                                 (float-time (time-since ts ))))))))

;; copy from doom emacs
(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'.  WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(put 'defadvice! 'lisp-indent-function 'defun)

(yc/defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom--resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is.  If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro yc/add-safe-local-var (&rest vars)
  "Add VARS as `safe-local-variable-values'."
  `(setq safe-local-variable-values (append ,(push 'list vars) safe-local-variable-values)))

 ;; Functions
(defun yc/get-key-code (key &optional recursive)
  "Return code of KEY."
  (cond
   ((vectorp key) key)
   ((stringp key) (read-kbd-macro key))
   ((not recursive)   (yc/get-key-code (eval key) t))
   (t (signal 'wrong-type-argument (list 'array key))))
  )

(defun yc/set-keys (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let ((map (if keymap keymap (current-global-map)))
        (prefix (if key-prefix (concat key-prefix " ") "")))

    (dolist (element key-alist)
      (let ((key (yc/get-key-code (car element)))
            (def (cdr element)))
        (define-key map key def)))))

(defun yc/unset-keys (key-list &optional keymap)
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


(define-key ctl-x-map "v" nil)

;;;; functions to setup platform depadent settings.
(defalias 'string-split 'split-string)

(defvar yc/debug-log-limit 1024 "Nil.")
(defvar YC-DEBUG nil "Set this flag to t to enable debug mode.")
(defconst YC-DEBUG-BUF "*YC-DEBUG*" "Debug buffer of my own.")

(defun yc/run-hooks-est-time (func &rest args)
  "Wrapper of run-hooks."
  (let ((hk (mapconcat
             (lambda (x) (symbol-name x)) args " "))
        (start (current-time))
        spent)
    (apply func args)
    (if (> (setq spent (float-time (time-since start))) 0.5)
        (with-current-buffer (get-buffer-create YC-DEBUG-BUF)
          (goto-char (point-max))
          (princ (format "Hook %s for %s: finished in %.02f seconds\n"
                         hk (or (buffer-file-name) "unkown") spent) (current-buffer))))))

(defun update-debug-vars (debug &optional quiet)
  "Update DEBUG variables.
Don't output logs when QUIET is t."
  (if (setq YC-DEBUG debug)
      (progn
        (custom-set-variables
         '(debug-on-error t)
         '(debug-on-quit nil)
         '(use-package-verbose t)
         '(yas-verbosity 4))
        (advice-add 'run-hooks :around #'yc/run-hooks-est-time))

    (custom-set-variables
     '(debug-on-error nil)
     '(debug-on-quit nil)
     '(use-package-verbose nil)
     '(yas-verbosity 0))
    (advice-remove 'run-hooks  #'yc/run-hooks-est-time))
  (unless quiet
    (message "Debug turned %s" (if YC-DEBUG "ON" "OFF"))))

(update-debug-vars (getenv "DEBUG") t)

(defun yc/debug-log (func &rest msgs)
  "Output MSGS with FUNC."
  (let ((buf (get-buffer-create YC-DEBUG-BUF))
        (last  (if msgs
                   (nth (1- (length msgs)) msgs)))
        pos-start)
    (save-excursion
      (with-current-buffer buf
        (goto-char (point-max))
        (princ "\n" buf)
        (princ (concat (format-time-string  "%a %b %d %H:%M:%S %Z %Y" (current-time))
               "    " func) buf)
        (princ " ======>\n" buf)
        (setq pos-start (point))
        (princ (pop msgs) buf)

        (while msgs
          (princ " " buf)

          (let ((count 0))
            (princ (pop msgs) (lambda (x)
                                (when (or (< yc/debug-log-limit 0)
                                          (< count yc/debug-log-limit))
                                  (insert x)
                                  (setq count (1+ count)))))))
        (princ "\n" buf)))
    last))

(defmacro PDEBUG (&rest args)
  "Write ARGS to log, if YC-DEBUG is non-nil."
  `(when YC-DEBUG
     (let ((func ,(or (when (fboundp 'which-function) (which-function)) "unkown")) )
       (funcall 'yc/debug-log func ,@args))))



(defcustom available-width '(78 82 86 92 98)
  "Available column width for Emacs to choose.."
  :group 'user)

(defun yc/calc-column-width (x-width)
  "Calculate column based on X-WIDTH."
  (let* ((r-match-font
          (rx "-" (+? (or alpha "-")) "*-"
              (group (+? digit)) "-*" (+ (or alpha "-" "*"))))
         (font-string (face-font 'default))
         (pos 0)
         (font-width
          (if (string-match r-match-font font-string)
              (string-to-number (match-string 1 font-string))
            12))
         (width-list (sort available-width '>))
         (target (/ x-width (+ 3 font-width)))
         width)
    (PDEBUG "target" target)
    (or (catch 'width
          (while (setq width (pop width-list))
            (when (<= width target)
              (PDEBUG "Got proper width" width)
              (throw 'width width))))
        78)))


(defun yc/setup-column()
  "setup column width, fill-column can be overwriten by 100-private.el"
  (let ((x-width (x-display-pixel-width)))
    (when x-width
      (setq-default fill-column (yc/calc-column-width x-width)))))


(defun pprintfonts (fonts)
  (dolist (f fonts)
    (message "%s" f))
  fonts)

(defun yc/search-fonts (&optional font)
  (interactive)
  (let* ((target (or font (completing-read "Fonty Name: " nil)))
         (answer (pprintfonts
                  (cl-remove-if
                   (lambda (x)
                     (not (string-match-p (concat ".*" target ".*") x)))
                   (font-family-list)))))

    (if (called-interactively-p 'interactive)
        (message "Target: %s, fond: %s" target answer))
    answer))

;; what-cursor-position to describe font under cursor,  with a prefix argument
;; shows the face under point, among other information.
;; keyboard shortcut is C-u C-x =

(defun yc/set-font (font points)
  (awhen (car (yc/search-fonts font))
    (let ((font it)
          (height (* points 10)))
      (PDEBUG "Set FONT:" it
              "HEIGHT:" points)
      (set-face-attribute 'default nil :family it)
      (set-face-attribute 'default nil :height height)
      (cons font points))))

(defun yc/set-cjk-font (font points)
  (awhen (car (yc/search-fonts font))
    (let ((font it)
          (height (* points 10)))

      (PDEBUG "Set CJK FONT:" it
              "HEIGHT:" points)

      (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font nil;; (frame-parameter frame 'font)
                        charset
                        (font-spec :family font
                                   :size points)))
      (cons font points))))


(defun yc/get-currnet-monitor-width ()
  "Return width of current monitor."
  (catch 'd-width
    (dolist (monitor (display-monitor-attributes-list))
      (let ((frames (alist-get 'frames monitor)) )
        (PDEBUG "M" monitor
                (selected-frame)
                "FS: " frames
                (member (selected-frame) frames))

        (when (member (selected-frame) frames)
          (throw 'd-width (nth 2 (alist-get 'geometry monitor))))))))

(defun yc/setup-font ()
  "Setup font."
  (when window-system
    (let ((font (catch 'p-found
                  (dolist (p `(("Monego" .
                                ,(cl-case system-type
                                   ('darwin 13)
                                   ('gnu/linux (if (> (yc/get-currnet-monitor-width) 1920)
                                                   11 ;; hidpi...
                                                 10))
                                   (t 11)))
                               ("Monaco" .
                                ,(cl-case system-type
                                   ('darwin 13)
                                   ('gnu/linux (if (> (yc/get-currnet-monitor-width) 1920)
                                                   11 ;; hidpi...
                                                 10))
                                   (t 11)))
                               ("Cascadia Mono" . 11)
                               ("Inconsolata" . 14 )))

                    (aif (yc/set-font (car p) (cdr p))
                        (throw 'p-found it)))))
          (cjk-font (catch 'p-found
                      (dolist (p '(( "Hiragino Sans GB"    . 15 )
                                   ( "WenQuanYi Micro Hei" . 15 )
                                   ( "Microsoft YaHei"     . 15 )))

                        (aif (yc/set-cjk-font (car p) (cdr p))
                            (throw 'p-found it))))))
      (message "Font set to: %s - %d, CJK-Font: %s - %d"
               (car font) (cdr font) (car cjk-font) (cdr cjk-font)))))

(yc/set-font "Monaco" 15)

(defun yc/setup-display (&optional frame)
  "Setup display, including: font, colortheme, and fill-column."
  (interactive)
  (if window-system
    (condition-case error
        (progn
          (yc/setup-font)
          (yc/setup-column)
          (setq-default header-line-format nil)
          )
      (error (message "set display failed.")))

    ;; set header-line, if connect from home.
    (setq-default header-line-format
      (if (zerop (call-process "bash" nil nil nil "-c" "who | grep 'yyc' | grep '192.168.3'"))
          '("%f"))))
  (load-theme 'tb-dark))

(defun yc/server-create-window-system-frame (func &rest args)
  "Advice for `server-create-window-system-frame'.
Call FUNC with ARGS."
  (yc/setup-display))

(yc/eval-after-load
  "server"
  (advice-add 'server-create-window-system-frame :after #'yc/server-create-window-system-frame))


(defun s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))

(yc/set-keys
 (list
  (cons (kbd "C-,") 'backward-page)
  (cons (kbd "C-.") 'forward-page)
  (cons (kbd "C->") 'end-of-buffer)
  (cons (kbd "C-<") 'beginning-of-buffer)
  (cons (kbd "C-w") 'kill-region)
  (cons [f4] 'goto-line)
  (cons (kbd "C-x C") 'kill-emacs)

  ;; special mappings for iTerm2...
  (cons (kbd "M-[ 1 ; 5 l") 'backward-page)
  (cons (kbd "M-[ 1 ; 5 n") 'forward-page)
  (cons (kbd "M-[ 1 ; 6 l") 'beginning-of-buffer)
  (cons (kbd "M-[ 1 ; 6 n") 'end-of-buffer)
  (cons (kbd "M-[ 1 ; 6 n") 'end-of-buffer)

  ;; narrow-to-xxx
  (cons (kbd "C-x n r") 'narrow-to-region)
  (cons (kbd "C-x n d") 'narrow-to-defun)
  (cons (kbd "C-x n w") 'widen)))



 ;; Advice
(defun yc/try-install-package (pkg)
  "Try to install PKG.
eg: pkg: A-B-C, will try to install following packages:
A-B-C
A-B-C-mode
A-B-Cmode

A-B
A-B-mode
A-Bmode

A
A-mode
Amode.
"
  (let ((cmps (reverse (split-string (if (stringp pkg) pkg (symbol-name pkg)) "-")))
        r-pkgs)
    (while cmps
      (let ((tmp (mapconcat 'identity (reverse cmps) "-")))
        (push tmp r-pkgs)
        (push (concat tmp "-mode")  r-pkgs)
        (push (concat tmp "mode")  r-pkgs)
        (pop cmps)))

    (let ((pkgs (reverse r-pkgs)))
      (catch 'p-installed
        (while pkgs
          (condition-case var
              (aif (car pkgs)
                  (progn
                    (message "Trying to install %s." it)
                    (package-install (intern it))
                    (throw 'p-installed t)))
            (error (progn
                     (message "%s -- %s" (car var) (cdr var))
                     (pop pkgs)))))))))

(autoload 'backtrace-to-string "backtrace")
(defun yc/install-package-on-error (func &rest args)
  "Apply FUNC with ARGS.
And install necessary packages if there are errors while executing FUNC."
  (interactive)
  (condition-case err (apply func args)
    (file-error
     (let ((msg (prin1-to-string err))
           (yc/debug-log-limit -1))
       (PDEBUG "ERR: " err
               "MSG: " msg
               "Backtrace: "
               (backtrace-to-string))
       (if (string-match ".*Cannot open load file.*" msg)
           (if (listp err)
               (let* ((package-name (nth (1- (length err)) err))
                      (fmt "Package %s can not be loaded, try install it? "))
                 (if package-name
                     (when (yes-or-no-p (format fmt package-name))
                       (yc/try-install-package package-name)
                       (set-auto-mode)))))
         (error "Operation failed: %s" msg))))))

(advice-add  'command-execute :around #'yc/install-package-on-error)
(advice-add 'run-hooks :around #'yc/install-package-on-error)

(eval-after-load
  "timer"
  (advice-add 'timer-event-handler :around #'yc/install-package-on-error))

(defun yc/in-comments-p ()
  "Check if current point is in comment."
  (interactive)
  (let ((res   (nth 4 (syntax-ppss))))
    (when (called-interactively-p 'interactive)
      (message "%sInside comments."
               (if res "" "Not ")))
    res))

(defun yc/in-string-p ()
  "Check if current point is in comment."
  (interactive)
  (let ((res   (nth 3 (syntax-ppss))))
    (when (called-interactively-p 'interactive)
      (message "%sInside string ."
               (if res "" "Not ")))
    res))

(defun yc/in-comment-or-string-p ()
  "Check if current point is in comment or in string."
  (interactive)
  (let* ((in-comment (nth 4 (syntax-ppss)))
         (in-string (nth 3 (syntax-ppss)))
         (res   (or in-comment in-string)))
    (when (called-interactively-p 'interactive)
      (message "%sIn comment, %sin string."
               (if in-comment "" "Not ")
               (if in-string "" "Not ")))
    res))


(defun yc/disable-trailling-spaces ()
  "Don't show trailing white spaces."
  (interactive)
  (setq show-trailing-whitespace nil))

(defun yc/update-exec-path ()
  "Description."
  (interactive)
  (mapcar (lambda (x)
            (add-to-list 'exec-path x))
          (split-string (getenv "PATH") ":" )))

(defun yc/load-envs (envs)
  "Load ENVS."
  (dolist (env envs)
    (let ((kv (split-string env "=")))
      (if (not (= (length kv) 2))
          (PDEBUG "ENV entry skipped: " env)
        (let ((k (nth 0 kv))
              (v (nth 1 kv)))
          (PDEBUG "Setting env: " k "=" v)
          (setenv k v))))))

(defun yc/load-shell-env-from-cmd (cmd)
  "Parse and load shell environment variables from result of CMD."
  (yc/load-envs
   (split-string (shell-command-to-string cmd) "\n" t)))

(defun yc/load-shell-env-from-file (&optional file)
  "Load environment variables from FILE.."
  (interactive)
  (unless file
    (if (called-interactively-p 'interactive)
        (setq file (ivy-read "Load from file: " 'read-file-name-internal
                             :matcher #'counsel--find-file-matcher
                             :require-match 'confirm-after-completion
                             :history 'file-name-history
                             :caller 'counsel-find-file))))
  (unless file
    (error "Input file is nil"))

  (unless (file-exists-p file)
    (error "File %s not accessible" file))

  ;; let's clear some environment variables marked as unset...
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx bol (* space) bow "unset" eow (+ space)
                (group (+? (not space))) (* space) eol) nil t)
      (let ((var (match-string 1)))
        (PDEBUG "unset: " var)
        (setenv var  nil))))

  (yc/load-shell-env-from-cmd
   (format "zsh --login -i -c 'source %s && env'" file)))

(defun yc/load-shell-env ()
  "Load environment variables."
  (interactive)
  (yc/load-shell-env-from-cmd "zsh --login -i -c env")
  (yc/update-exec-path))

(add-hook 'emacs-startup-hook (lambda ()
                                (yc/setup-display)
                                (yc/load-shell-env)))

(defun yc/file-directory-p (x)
  "Similar to `file-directory-p', but return X unchanged if x is a directory."
  (let ((path (expand-file-name x)))
    (PDEBUG "CHECKING: " x)
    (if (file-directory-p x)
        path
      (PDEBUG "File does not exist:" x)
      nil)))

(defun yc/file-exists-p (x)
  "Similar to `file-exist-p', but return X unchanged if x is a directory."
  (let ((path (expand-file-name x)))
    (PDEBUG "CHECKING: " x)
    (if (file-exists-p path)
        path
      (PDEBUG "File does not exist:" x)
      nil)))

(defun yc/run-with-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.
Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))

(defvar yc/auto-delete-trailing-spaces-modes nil
  "List of modes in which trailing spaces should be deleted automatically.")

(defun yc/add-auto-delete-spaces (&rest modes)
  "Add modes to `yc/auto-delete-trailing-spaces-modes'."
  (mapc (lambda (x)
          (unless (member x yc/auto-delete-trailing-spaces-modes)
            (push x yc/auto-delete-trailing-spaces-modes)))
        modes))

(add-hook 'before-save-hook
  (lambda ()
    (when (and (buffer-modified-p)
               (member major-mode yc/auto-delete-trailing-spaces-modes))
      (delete-trailing-whitespace))) t t )


(defun term-set-window-title (title)
  "Set the title for term window to TITLE.
This does not change the title in the corresponding icon."
  (interactive "sWindow title: ")
  (term-send-escape-sequence (format "\e]2;%s\a" title)))

(defvar term-screen-dcs-encapsulation
  (not (null (or (getenv "STY")
                 (save-match-data
                   (string-match "^screen\\(\\|-.*\\)$" (getenv "TERM")))))))
(defun term-send-escape-sequence (string)
  "Send STRING to term."
  (cond ((and term-screen-dcs-encapsulation
              (save-match-data (string-match "\e[P\\\\]" string)))
         ;; ^[P and ^[\ are screen's DCS text string encapsulators, so if
         ;; they appear as a literal in the string argument, they would
         ;; truncate/corrupt the encapsulation.  Therefore, any literal
         ;; occurences will be broken up into multiple encapsulations so as
         ;; not to confuse screen.
         (save-match-data
           (let ((pos 0)
                 (substrings nil))
             (while (string-match "\e\\(P\\|\\\\\\)" string pos)
               (setq substrings
                     (cons "\e\\"
                           (cons (substring string pos (match-beginning 1))
                                 (cons "\eP" substrings))))
               (setq pos (match-beginning 1)))
             (setq substrings (cons (substring string pos) substrings))
             (setq string (mapconcat 'identity (nreverse substrings) "")))))
        (term-screen-dcs-encapsulation
         (setq string (format "\eP%s\e\\" string))))
  (send-string-to-terminal string))

(defun yc/update-term-title (&optional arg)
  "Update terminal title."
  (unless window-system
    (term-set-window-title (format "%s@%s  : %s" user-login-name (system-name)
                                 (or (buffer-file-name) dired-directory (buffer-name))))))

(add-to-list 'window-buffer-change-functions 'yc/update-term-title)
;; (add-hook 'buffer-list-update-hook 'yc/update-term-title)
(add-hook 'after-change-major-mode-hook 'yc/update-term-title)
(add-function :after after-focus-change-function #'yc/update-term-title)

 
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

(provide '02-functions)
;;; 02-functions.el ends here
