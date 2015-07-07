;;; 052-prog-elisp.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

(use-package  elisp-mode
  :preface
  (defun yc/insert-key-sequence-kbd ()
    "Insert key sequence (with kbd function)."
    (interactive)
    (let ((key (read-key-sequence "Stoke:")) )
      (insert (format "(kbd \"%s\")" (key-description key)))))

  (defun yc/insert-key-sequence ()
    "Insert key sequence."
    (interactive)
    (yc/insert-key-sequence-kbd)
    (yc/eval-and-insert-comment))

  (defun yc/clean-native-compiled-file (file)
    "Clean up previous native-compiled FILE."
    (let* ((tgt-dir (expand-file-name "~/.emacs.d/eln-cache"))
           (bname (file-name-base file))
           (yc/debug-log-limit -1)
           deleted-files)

      ;; clean up previously compiled files.
      (when (file-exists-p tgt-dir)
        (let ((subdirs (directory-files tgt-dir t "x86.*")))
          (if (> (length subdirs) 1)
              (progn
                (delete-directory tgt-dir t)
                (warn "don't know which version to load, simply clear all subdirectories."))

            (setq tgt-dir (car subdirs))
            (PDEBUG "Cleaning in: " tgt-dir " for file: " file ", base name: " bname)
            (dolist (fn (directory-files tgt-dir t (concat bname  "-.*\\.eln")))
              (push fn deleted-files)
              (delete-file fn))
            (PDEBUG "Deleted " (length deleted-files) " files:\n"
                    (mapconcat 'identity deleted-files "\n\t")))))))

  (defun yc/byte-compile-current-elisp ()
    "Byte compile Lisp file."
    (interactive)
    (when (eq major-mode 'emacs-lisp-mode)
      (if (or (string-match-p (rx (or (: ".emacs.d/rc/" (+ nonl))
                                      ".dir-locals")
                                  ".el")
                              buffer-file-name)
              (not (string-match-p (rx (+? nonl) ".el") buffer-file-name)))
          (PDEBUG "Byte compile ignored for file: " buffer-file-name)
        (byte-compile-file buffer-file-name)
        (when (fboundp 'native-compile-async)
          (yc/clean-native-compiled-file buffer-file-name)
          (native-compile-async buffer-file-name)))))

  (defun my-lisp-hook ()
    "Hook to run for Lisp mode."
    (set (make-local-variable 'header-field-list)
         '(lisp_desc blank copyright blank author blank n_emacs gpl
                     blank e_comment blank))

    (set (make-local-variable 'autopair-skip-whitespace) 'chmop)
    (eldoc-mode 1)
    (setq c-basic-offset 8
          tab-width 8))

  :mode (((rx "." (or "el" "sexp") eol) . emacs-lisp-mode))
  :bind (:map emacs-lisp-mode-map
              (;; (kbd "C-c M-k")
               [3 134217835] . yc/insert-key-sequence-kbd)
              (;; (kbd "C-c M-K")
               [3 134217803] . yc/insert-key-sequence))

  :hook ((emacs-lisp-mode . my-lisp-hook)
         (after-save .  yc/byte-compile-current-elisp))

  :config
  (put 'add-hook 'lisp-indent-function 'defun)
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(rx "("
           (group (or (: "yc/" (+ (or alnum "-" "_")))
                      "add-to-list" "try-require" "add-hook" "autoload"
                      "yc/eval-after-load" "try-require-autoloads"
                      "fboundp" "boundp" "featurep" "cdsq"
                      "cds" "dsq" "sif" "aif" "font-lock-add-keywords"
                      "csq" "cdsq"  "defun*" "defmacro*" "define-error"))
           (? (+ (or space "\n"))
              (group (* (or alnum "-" "_" "'" "/" "\"")))))
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face nil t))))
  (yc/add-company-backends 'emacs-lisp-mode 'company-elisp 'company-dabbrev-code)
  (yc/add-auto-delete-spaces 'emacs-lisp-mode))


 ;; native compile..
(when (featurep 'nativecomp)
  (setq comp-deferred-compilation t)

  (defadvice! yc/package-unpack-adv (pkg-desc &rest args)
    "Native compile files in this PKG-DESC."
    :after  #'package-unpack
    (let* ((name (package-desc-name pkg-desc))
           (dirname (package-desc-full-name pkg-desc))
           (src-dir (expand-file-name dirname package-user-dir)))

      ;; clean up previously compiled files.
      (mapc 'yc/clean-native-compiled-file (directory-files src-dir nil ".*\.el"))

      ;; now starts compile.
      (native-compile-async src-dir t)))

  (defun yc/native-compile-file ()
    "Native compile selected file."
    (interactive)
    (native-compile-async
     (counsel-list-directory default-directory)
     nil))

  (defun yc/native-compile-dir (&optional dir)
    "Native compile selected DIR."
    (interactive)
    (unless dir
      (setq dir (let* ((suggestion default-directory)
                       (choices (list
                                 (format "    Choose src-dir %s" suggestion)
                                 "    Choose by selecting src-dir interactively."))
                       (action-index (cl-position
                                      (completing-read "Select directory: "
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
                  project-root)))
    (PDEBUG "Native compile: " dir)
    (native-compile-async (expand-file-name dir) t)))

(provide '052-prog-elisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 052-prog-elisp.el ends here
