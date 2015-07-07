;;; eshell+.el --- Brief introduction here.  -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:


(defun eshell/ldd (&rest args)
  "Implementation of mkdir in Lisp."
  (cond
   ((executable-find "ldd")
    (shell-command-to-string (concat "ldd " (mapconcat 'identity args " "))))

   ((executable-find "otool")
    (shell-command-to-string (concat "otool -L " (mapconcat 'identity args " "))))
   (t
    (error "Can't find ldd or otool"))))

(defun eshell/restart_pg (&optional datadir)
  "Restart PG."
  (interactive)
  (unless datadir
    (error "Usage: restart_pg datadir"))

  (aif (executable-find "pg_ctl")
      (let* ((stop-command (format "%s stop -D %s" it datadir))
             (start-command (format "%s start -D %s" it datadir))
             (final-command (format "echo Stopping via '%s'.;%s;echo Starting via '%s'.;%s"
                                    stop-command stop-command start-command start-command)))
        (start-process-shell-command "restart_pg" (current-buffer) final-command))

    (error "Could not find pg_ctl, current PATH: %s" (getenv "PATH"))))


(provide 'eshell+)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eshell+.el ends here
