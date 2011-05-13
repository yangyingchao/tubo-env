;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: 03-rc-misc.el, 08-27-2010

(require 'icomplete)
(icomplete-mode t)

;;;; ido
(when (require 'ido "ido" t)
  (ido-mode 'buffer)
  ;;(ido-mode)
  (setq ido-enable-flex-matching t) ;; enable fuzzy matching
  (define-key ctl-x-map "\C-v" 'ido-find-file))

(require 'make-mode)
(autoload 'makefile-mode "makefile-mode" nil t)
(add-to-list 'auto-mode-alist
             '("Makefile.*" . makefile-mode))
(add-to-list 'auto-mode-alist
             '("makefile.*" . makefile-mode))

(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 99)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

 ;;; ABBREV-MODE;;;

(setq abbrev-file-name  "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(setq-default abbrev-mode t)


;;;; Speedbar

(setq speedbar-mode-hook '(lambda ()
                            (interactive)
                            (other-frame 0)))
(require 'sb-texinfo)
(eval-after-load "speedbar" '(load-library "sb-texinfo"))
(add-hook 'texinfo-mode-hook (lambda () (require 'sb-texinfo)))

(require 'sr-speedbar)
(global-set-key [f7] 'sr-speedbar-toggle);f7打开/关闭speedbar

 ;;; Woman
(require 'woman)
(set-face-foreground 'woman-italic "#73d2e6")
(set-face-foreground 'woman-bold "#a40000")
(setq woman-use-own-frame nil);; 不打开新的 frame

(setq woman-manpath (quote ("/usr/share/man" "/usr/local/share/man/")))

(defun peer-woman() ;; 这个名字有点罪过……
  "Wrap of linux man command"
  (interactive)
  (let ((woman-topic-at-point t))
    (woman)))
(global-set-key [(f1)] 'peer-woman)   ;;设定F1为woman快捷键
(global-set-key (kbd "<S-f1>") 'peer-woman)   ;;设定Shift+F1为woman快捷键


 ;;; ispell
(require 'ispell)

(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))
(set-default 'ispell-skip-html t)
(setq ispell-dictionary "english")
(setq ispell-local-dictionary "english")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))
(defun turn-off-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode nil))



(add-hook 'log-edit-mode-hook 'turn-on-flyspell)

(global-set-key (kbd "<M-S-f11>") 'ispell-buffer)
(global-set-key (kbd "<C-f11>") 'flyspell-mode)
(global-set-key (kbd "<C-S-f11>") 'flyspell-prog-mode)
(global-set-key (kbd "<S-f11>") 'ispell-word)

 ;;Dired
(require 'dired)
(setq dired-dwim-target t)
(load "dired-x")

;; Don't copy files to open dired buffers via drag&drop
(setq dired-dnd-protocol-alist nil)

(setq dired-guess-shell-alist-user
      (list
       ;;(list "\\.fig" "c:\\utils\\4\\4nt /c fig2eps" "xfig")
       (list "\\.ps" "gv")
       (list "\\.doc" "oowriter")
       (list "\\.ppt" "ooimpress")
       (list "\\.xls" "oocalc")
       (list "\\.sxc" "oocalc")
       (list "\\.png" "gimp")
       ))
;;        (list "\\.pdf" "acroread")))

(load "sorter" t t) ; sort dired listing (key s)
(define-key dired-mode-map [(control shift r)] 'dired-rar-add-files)

(when (require 'wdired nil t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))
                                        ;use the lisp ls implementation (using this sort by extension works)

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(load "ls-lisp" t t)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil) ; needed on unix
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

;; ************************** highlight utils ****************************
(require 'highlight-utility)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-remove-all)
(global-set-key [(meta f3)] 'highlight-symbol-prev)
(global-set-key [(control meta f3)] 'highlight-symbol-query-replace)

;; ************************** SVN Settings *****************************

(require 'psvn)
;; C-xvv Bind to vc-next-action.
(global-set-key "\C-xvV" 'svn-status-commit)
(global-set-key "\C-xvl" 'svn-status-show-svn-log)
(global-set-key "\C-xvu" 'svn-status-update-cmd)
(global-set-key "\C-xvs" 'svn-status-curdir)
(global-set-key "\C-xvS" 'svn-status)
(define-key svn-status-mode-map (kbd "d") 'svn-status-rm)

(defun svn-status-curdir()
  (interactive)
  (svn-status (file-name-directory (buffer-file-name))))


 ;; ********************** Browse Killing Ring *************************

(require 'browse-kill-ring)

(browse-kill-ring-default-keybindings)

(global-set-key (kbd "C-c k") 'browse-kill-ring)

(setq kill-ring-max 100)

(when (locate-library "browse-kill-ring")
  (autoload 'browse-kill-ring "browse-kill-ring" "interactively insert items from kill-ring" t)
  (define-key ctl-x-map "\C-y" 'browse-kill-ring)
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    (interactive "p")
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop)
  (setq browse-kill-ring-quit-action 'kill-and-delete-window)
  (setq browse-kill-ring-separator "\n--separator--------------------------"
        browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item t
        browse-kill-ring-resize-window
        ))

 ;; **************************** RFCs ******************************

;;;; get-rfc
(autoload 'rfcview-mode "rfcview" nil t)
(autoload 'get-rfc-view-rfc "get-rfc" "Get and view an RFC" t nil)
(autoload 'get-rfc-view-rfc-at-point "get-rfc" "View the RFC at point" t nil)
(autoload 'get-rfc-grep-rfc-index "get-rfc" "Grep rfc-index.txt" t nil)

(setq get-rfc-wget-program "wget")
(setq get-rfc-remote-rfc-directory "http://www.rfc-editor.org/rfc/")
(setq get-rfc-open-in-new-frame nil)
(setq get-rfc-local-rfc-directory  "~/Documents/TechBooks/RFCs/")
(setq get-rfc-tmp-dir "/tmp/")
(setq get-rfc-wget-output-flag "-O ")

(setq auto-mode-alist
      (cons '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode)
            auto-mode-alist))


(eval-after-load "speedbar" '(load-library "sb-rfcview"))
(custom-set-variables
 '(speedbar-supported-extension-expressions
   (append
    speedbar-supported-extension-expressions
    '("rfc[0-9]+\\.txt"))))

;; Customized face of rfc.
(custom-set-faces
 '(rfcview-title-face ((t (:foreground "darkgreen" :weight bold)))))



 ;; ********************* tramp *******************************

(require 'tramp)

(setq tramp-default-method "scp" tramp-default-user "yyc")

(setq tramp-completion-without-shell-p t)

;; (setq tramp-shell-prompt-pattern "^[ $]+")
(setq tramp-auto-save-directory "~/.emacs.d/auto-save-list")

(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

 ;; ********************* W3m ***********************************
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
(require 'w3m-load)
(provide 'w3m-e23)
(require 'w3m)

(defvar w3m-arrived-file-coding-system nil "nil")


(setq  w3m-arrived-file-coding-system 'utf-8
       w3m-async-exec t
       w3m-coding-system 'utf-8
       w3m-default-save-directory "~/tmp/"
       w3m-delete-duplicated-empty-lines t
       w3m-file-coding-system 'utf-8
       w3m-file-name-coding-system 'utf-8
       w3m-fill-column 99
       w3m-input-coding-system 'utf-8
       w3m-key-binding 'info
       w3m-mailto-url-function 'compose-mail
       w3m-output-coding-system 'utf-8
       w3m-search-default-coding-system 'utf-8
       w3m-terminal-coding-system 'utf-8
       w3m-use-cookies t
       w3m-use-filter t
       w3m-use-form t
       w3m-use-mule-ucs t
       w3m-use-favicon nil
       w3m-default-display-inline-images nil
       w3m-use-filter t
       w3m-command-arguments '("-cookie" "-F" ))

(if (string-match "ITC-208024"  system-name)
    (setq w3m-command-arguments-alist
          '(;; Don't use the proxy server to visit local web pages.
            ("^http://\\([^/]*\\.\\)*your-company\\.com\\(/\\|$\\)"
             "-no-proxy")
            ;; Use the proxy server to visit any foreign urls.
            (""
             "-o" "http_proxy=http://127.0.0.1:5866/")))
  )


(standard-display-ascii ?\212 "-")
(standard-display-ascii ?\226 "-")
(standard-display-ascii ?\227 [?-])
(standard-display-ascii ?\222 [?'])
(standard-display-ascii ?\225 [?+])

;; send all pages through one filter
(setq w3m-filter-rules `(("\\`.+" w3m-filter-all)))

(defun w3m-filter-all (url)
  (let ((list '(
                ;; add more as you see fit!
                ("&#187;" "&gt;&gt;")
                ("&laquo;" "&lt;")
                ("&raquo;" "&gt;")
                ("&ouml;" "o")
                ("&#8230;" "...")
                ("&#8216;" "'")
                ("&#8217;" "'")
                ("&rsquo;" "'")
                ("&lsquo;" "'")
                ("\u2019" "\'")
                ("\u2018" "\'")
                ("\u201c" "\"")
                ("\u201d" "\"")
                ("&rdquo;" "\"")
                ("&ldquo;" "\"")
                ("&#8220;" "\"")
                ("&#8221;" "\"")
                ("\u2013" "-")
                ("\u2014" "-")
                ("&#8211;" "-")
                ("&#8212;" "-")
                ("&ndash;" "-")
                ("&mdash;" "-")
                )))
    (while list
      (let ((pat (car (car list)))
            (rep (car (cdr (car list)))))
        (goto-char (point-min))
        (while (search-forward pat nil t)
          (replace-match rep))
        (setq list (cdr list))))))

(defun cc-w3m-beautify-tables()
  (goto-char (point-min))
  (while (re-search-forward
          "\200\\|\203\\|\206\\|\211\\|\214\\|\202\\|\204\\|\210\\|\201" nil t)
    (replace-match "+"))
  (goto-char (point-min))
  (while (re-search-forward "\205" nil t)
    (replace-match "|"))
  (goto-char (point-min))
  (while (re-search-forward "\212" nil t)
    (replace-match "-")))

(add-hook 'w3m-fontify-after-hook 'cc-w3m-beautify-tables)

;;Removing trailing whitespaces (better display):
(defun cc-w3m-del-trailing-ws()
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "")))

(add-hook 'w3m-fontify-after-hook 'cc-w3m-del-trailing-ws)

(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*" (or w3m-current-title
                                     w3m-current-url)) t)))

(setq browse-url-generic-program "/usr/bin/firefox" )
(defun w3m-open-current-page-in-gui ()
  "Opens the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-generic w3m-current-url))

(defun w3m-open-link-or-image-in-gui ()
  "Opens the current link or image in Firefox."
  (interactive)
  (browse-url-generic (or (w3m-anchor)
                          (w3m-image))))

(defun yyc/load-w3m ()
  "Load configurations about w3m"
  (interactive)
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
  (setq fname (concat "file://" fname) )
  (message fname)

  (w3m fname t)
  )

(define-key w3m-mode-map "\C-co" 'w3m-open-current-page-in-gui)
(define-key w3m-mode-map  "\C-c\C-o" 'w3m-open-current-page-in-gui)
(define-key w3m-mode-map (kbd "j") 'next-line)
(define-key w3m-mode-map (kbd "k") 'previous-line)
(global-set-key (kbd "<C-f8>") 'yyc/load-w3m)
(global-set-key (kbd "<C-S-f8>") 'yyc/w3m-open-this-page)


 ;; *********************** graphviz dot mode ***********
;; (load-file "~/.emacs.d/lisps/misc/graphviz-dot-mode.el")
(load "graphviz-dot-mode.el" nil t t)

(add-hook 'find-file-hook (lambda()
                            (if (string= "dot" (file-name-extension
                                                buffer-file-name))
                                (progn
                                  (message "Enabling Setings for dot-mode")
                                  (setq fill-column 1000)
                                  (base-auto-pair)
                                  (local-set-key (kbd "<C-f6>") 'compile)
                                  )
                              )))

 ;; ***************************** Some extra modes *************************
(add-to-list 'auto-mode-alist '("rc$" . conf-mode))


 ;; ********************** autocompressings *********************
;; Now add bzip2 support and turn auto compression back on.
(add-to-list 'jka-compr-compression-info-list
             ["\\.dia\\'"
              "compressing" "gzip" ("-c" "-q")
              "uncompressing" "gzip" ("-c" "-q" "-d")
              t t ""]
             )
(jka-compr-update)

(setq mail-default-directory "/tmp/")
(provide '03-rc-misc)
;;;;; emacs-rc-misc.el ends here
