;;; yc-dump.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

;; dump image
(require 'yc-utils)

(defun yc/do-dump ()
  "Do dump."
  (dump-emacs-portable "~/.emacs.d/emacs.pdmp"))

 ;; (dump-emacs-portable "~/.emacs.d/emacs.pdmp")

(defun yc/dump-emacs ()
  "Start another process to dump Emacs."
  (interactive)
  (let* ((buf (switch-to-buffer "*dump*"))
         (process (start-process
                   "dump-emacs" "*dump*" "emacs" "-batch"
                   "-Q" ;; for now, do not load anything, otherwise: emacs: Trying to load incoherent dumped .eln
                   "-l" (expand-file-name "~/.emacs.d/site-lisp/utils/yc-dump.el")
                   ;; "--execute \"(yc/do-dump)\""
                   ))
         )

    (set-process-sentinel process (lambda (proc event)
                                    (when (equal 'exit (process-status proc))
                                      (when (= 0 (process-exit-status proc))
                                        (PDEBUG "BUF:" buf
                                                "EVT:" event)
                                        (message "OK")))))))

(defun yc/config-emacs ()
  "Configure emacs with current system-configuration-options."
  (interactive)
  (let* ((source-directory
          (if (and (file-exists-p ".git")
                   (file-exists-p "autogen.sh"))
              default-directory
            (yc/choose-directory)))
         (opt-level (if current-prefix-arg current-prefix-arg 2))
         cflags
         options)

    ;; parse current-configuration options.
    (if (string-match (rx  (group bol (*? nonl))
                           "'CFLAGS=" (group (+? nonl))  "'"
                           (group (*? nonl)) eol)
                      system-configuration-options)
        (let* ((p1 (match-string 1 system-configuration-options))
               (p2 (match-string 2 system-configuration-options))
               (p3 (match-string 3 system-configuration-options)))

          (setq cflags (string-split p2)
                options (string-split p3))

          (PDEBUG "P1:" p1 "P2" p2 "P3" p3
                  "FLAGS:"  cflags
                  "OPTIONS: " options)))

    ;; remove unsupported option.
    (setq options (delete "--with-nativecomp" options))

    ;; check cflags
    (setq cflags (-remove (lambda (x) (s-starts-with-p "-O" x)) cflags))
    (if (= opt-level 0 )
        (push "-Og" cflags)
      (push (format "-O%d" opt-level) cflags))

    (dolist (flag '("-g" "-pipe" "-march=native"))
      (unless (member flag cflags)
        (push flag cflags)))

    ;; check customized options
    (dolist (option '("--with-modules" "--with-json" "--with-native-compilation" "--without-pop"))
      (unless (member option options)
        (push option options)))

    (with-current-buffer (eshell)
      (insert (format "cd %s; " source-directory))
      (insert "./autogen.sh && ./configure  ")
      (insert "'CFLAGS=" (mapconcat 'identity cflags " ") "' ")
      (insert (mapconcat 'identity options " "))
      (eshell-send-input))))

(provide 'yc-dump)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yc-dump.el ends here
