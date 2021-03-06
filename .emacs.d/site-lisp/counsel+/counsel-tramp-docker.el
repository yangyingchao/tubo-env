;;; counsel-tramp-docker.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/yyc>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: January 26, 2021
;; Modified: January 26, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yyc/counsel-tramp-docker
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; counsel-tramp-docker provides interfaces of Tramp
;; You can also use tramp with counsel interface as root
;; If you use it with docker-tramp, you can also use docker with counsel interface
;; If you use it with vagrant-tramp, you can also use vagrant with counsel interface

;;; Code:

(require 'counsel)
(require 'tramp)
(require 'cl-lib)

(defgroup counsel-tramp nil
  "Tramp with ivy interface for ssh, docker, vagrant"
  :group 'counsel)

(defcustom counsel-tramp-default-method "ssh"
  "Default method when use tramp multi hop."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-docker-user nil
  "If you want to use login user name when `docker-tramp' used, set variable."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-localhost-directory "/"
  "Initial directory when connecting with /sudo:root@localhost:."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-control-master nil
  "If you want to put out a candidate for completion from ssh controlmaster, please set to t."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-control-master-path "~/.ssh/"
  "Path where ssh controlmaster exists."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-control-master-prefix "master-"
  "Prefix of ssh controlmaster."
  :group 'counsel-tramp
  :type 'string)

(defcustom counsel-tramp-pre-command-hook nil
  "Hook run before `counsel-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-post-command-hook nil
  "Hook run after `counsel-tramp'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-quit-hook nil
  "Hook run when `counsel-tramp-quit'.
The hook is called with one argument that is non-nil."
  :type 'hook)

(defcustom counsel-tramp-custom-connections '()
  "A list to manually add extra connections.
E.g.: '(\"/ssh:domain|sudo:user@localhost:/\")."
  :type 'string)

(defun counsel-tramp-quit ()
  "Quit counsel-tramp.
Kill all remote buffers."
  (interactive)
  (run-hooks 'counsel-tramp-quit-hook)
  (tramp-cleanup-all-buffers))

(defun counsel-tramp--candidates (&optional file)
  "Collect candidates for counsel-tramp from FILE."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents (or file "~/.ssh/config"))
                   (buffer-string))
                 "\n"))
        (hosts (if file '() counsel-tramp-custom-connections)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
	  (if (string-match "[ ]+" host)
	      (let ((result (split-string host " ")))
		(while result
		  (push
		   (concat "/" tramp-default-method ":" (car result) ":")
		   hosts)
                  (if current-prefix-arg
                      (push
		       (concat "/" counsel-tramp-default-method ":" (car result) "|sudo:root@" (car result) ":/")
		       hosts))

		  (pop result)))
	    (push
	     (concat "/" tramp-default-method ":" host ":")
	     hosts)
            (if current-prefix-arg
                (push
	         (concat "/" counsel-tramp-default-method ":" host "|sudo:root@" host ":/")
	         hosts))
	    )))
      (when (string-match "Include +\\(.+\\)$" host)
        (setq include-file (match-string 1 host))
        (when (not (file-name-absolute-p include-file))
          (setq include-file (concat (file-name-as-directory "~/.ssh") include-file)))
        (when (file-exists-p include-file)
          (setq hosts (append hosts (counsel-tramp--candidates include-file))))))
    (when counsel-tramp-control-master
      (let ((files (counsel-tramp--directory-files
		    (expand-file-name
		     counsel-tramp-control-master-path)
		    counsel-tramp-control-master-prefix))
	    (hostuser nil)
	    (hostname nil)
	    (port nil))
	(dolist (controlmaster files)
	  (let ((file (file-name-nondirectory controlmaster)))
	    (when (string-match
		   (concat counsel-tramp-control-master-prefix "\\(.+?\\)@\\(.+?\\):\\(.+?\\)$")
		   file)
	      (setq hostuser (match-string 1 file))
	      (setq hostname (match-string 2 file))
	      (setq port (match-string 3 file))
	      (push
	       (concat "/" tramp-default-method ":" hostuser "@" hostname "#" port ":")
	       hosts)
              (if current-prefix-arg
                  (push
	           (concat "/" counsel-tramp-default-method ":" hostuser "@" hostname "#" port "|sudo:root@" hostname ":/")
	           hosts))
	      )))))
    (when (require 'docker-tramp nil t)
      (cl-loop for line in (cdr (ignore-errors (apply #'process-lines "docker" (list "ps"))))
	       for info = (reverse (split-string line "[[:space:]]+" t))
	       collect (progn (push
			       (concat "/docker:" (car info) ":/")
			       hosts)
			      (when counsel-tramp-docker-user
				(if (listp counsel-tramp-docker-user)
				    (let ((docker-user counsel-tramp-docker-user))
				      (while docker-user
					(push
					 (concat "/docker:" (car docker-user) "@" (car info) ":/")
					 hosts)
					(pop docker-user)))
				  (push
				   (concat "/docker:" counsel-tramp-docker-user "@" (car info) ":/")
				   hosts))))))
    (when (require 'vagrant-tramp nil t)
      (cl-loop for box-name in (cl-map 'list 'cadr (vagrant-tramp--completions))
	       do (progn
		    (push (concat "/vagrant:" box-name ":/") hosts)
		    (push (concat "/vagrant:" box-name "|sudo:" box-name ":/") hosts))))
    (if current-prefix-arg
        (push (concat "/sudo:root@localhost:" counsel-tramp-localhost-directory) hosts))

    (reverse hosts)))

(defun counsel-tramp--directory-files (dir regexp)
  "Return list of all files under DIR that have file names matching REGEXP."
  (let ((result nil)
	(files nil)
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (not (counsel-tramp--directory-name-p file))
	    (when (string-match regexp file)
	      (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defsubst counsel-tramp--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

;;;###autoload
(defun counsel-tramp ()
  "Open your ~/.ssh/config with counsel interface.
You can connect your server with tramp"
  (interactive)
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
  (when (require 'docker-tramp nil t)
    (unless (executable-find "docker")
      (error "'docker' is not installed")))
  (when (require 'vagrant-tramp nil t)
    (unless (executable-find "vagrant")
      (error "'vagrant' is not installed")))
  (run-hooks 'counsel-tramp-pre-command-hook)
  (counsel-find-file (ivy-read "Tramp: " (counsel-tramp--candidates)))
  (run-hooks 'counsel-tramp-post-command-hook))


(defcustom counsel-docker/user-name "yyc"
  "User name of docker."
  :type 'string
  :group 'counsel-tramp
  )

(defcustom counsel-docker/user-passwd nil
  "User name of docker."
  :type 'string
  :group 'counsel-tramp
  )

(defun yc/deploy-my-utilies (remote-address)
  "Deploy my utilities to `SERVER'."
  (let* ((remote-home (concat remote-address "~"))
         (gdb-init-file (concat remote-home "/.gdbinit"))
         (vterm-bash-file (concat remote-home "/emacs-vterm-bash.sh"))
         (bash-rc-file (concat remote-home "/.bashrc"))
         (ssh-config-file (concat remote-home "/.ssh/authorized_keys")))

    ;; gdb & peda
    (unless (file-exists-p gdb-init-file)
      ;; copy .gdbinit & peda...
      (copy-file "~/.gdbinit" gdb-init-file)
      (copy-file "~/.emacs.d/tools/peda" (concat remote-home "/.emacs.d/tools/peda")))

    ;; emacs-vterm-bash
    (unless (file-exists-p vterm-bash-file)
      (with-temp-file vterm-bash-file
        (insert "vterm_set_directory() {\n"
                "\tvterm_cmd update-pwd \"" remote-address "$PWD/\"\n"
                "}\n\n")
        (insert-file-contents "~/.emacs.d/quelpa/build/vterm/etc/emacs-vterm-bash.sh")
        (goto-char (point-max))
        (insert "\n")

        (insert "PROMPT_COMMAND=\"$PROMPT_COMMAND;vterm_set_directory\"\n")))

    ;; update .bashrc
    (with-temp-file bash-rc-file
      (when (file-exists-p bash-rc-file)
        (insert-file-contents bash-rc-file)
        (goto-char (point-max)))

      (goto-char (point-min))
      (unless (search-forward "emacs-vterm-bash.sh" nil t)
        (insert "source ~/emacs-vterm-bash.sh\n")))

    ;; deploy ssh key.
    (with-temp-file ssh-config-file
      (insert-file-contents ssh-config-file)
      (goto-char (point-max))
      (insert-file-contents (expand-file-name "~/.ssh/id_rsa.pub")))))

(defun counsel-docker ()
  "Open your ~/.ssh/config with counsel interface.
You can connect your server with tramp"
  (interactive)
  (let* ((r-match-container (rx (* space) (group (+? alnum)) (+ space) (group (+? nonl)) eol))
         (host (ivy-read "Docker on host: "
                         (-filter
                          (lambda (x)
                            (if (string-match-p "docker-tmp" x)
                                nil
                              x))

                          (remove nil (mapcar (lambda (x)
                                    (when (string-match (rx "/" (or "scp" "sudo") ":" (group (+? nonl)) ":") x)
                                      (match-string 1 x)))
                                  (counsel-tramp--candidates))))))
         (zzz (shell-command-to-string
               (format "scp %s root@%s:/tmp/"
                       (expand-file-name "tools/get-containers.sh" user-emacs-directory)
                       host)))

         (containers
          (-remove (lambda (x)
                     (= (length x) 0) )
                   (mapcar 's-trim (s-split "\n"
                                            (shell-command-to-string (concat "ssh root@" host
                                                                             " /tmp/get-containers.sh"))))))

         (zzz2 (unless containers
                 (error "Failed to  get container list from host: %s" "d6")))
         (container (ivy-read "Choose container:" containers)))

    (if (string-match r-match-container container)
        (let* ((container-address (match-string 2 container))
               (vterm-buffer-name (format "vterm: %s in host %s"
                                          container-address host))
               (tramp-buffer-name (format "/scp:docker-tmp:/home/%s" counsel-docker/user-name)))
          (with-current-buffer (find-file (expand-file-name "~/.ssh/config"))
            (save-excursion
              (goto-char (point-min))
              (when (search-forward-regexp "^Host docker-tmp$" nil t)
                (let ((start (match-beginning 0)))
                  (goto-char (1+ (point)))
                  (while (looking-at-p (rx bol (+ space)))
                    (message "PT: %d -- %d: %s" start (point-at-eol)
                             (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                    (if (< (point-at-eol) (point-max))
                        (forward-line 1)
                      (goto-char (point-at-eol))))
                  (delete-region start (point-at-eol))))
              )

            (goto-char (point-max))

            (while (looking-back (rx (or "\n" space)))
              (delete-char -1))

            (insert (format "\nHost docker-tmp\n  Hostname %s\n  User %s\n  ProxyCommand ssh -l root -W %%h:%%p %s\n"
                            container-address counsel-docker/user-name host))
            (save-buffer))

          ;; open via tramp,
          ;; make sure previous connection is clean up.
          (when (get-buffer tramp-buffer-name)
            (with-current-buffer tramp-buffer-name
              (tramp-cleanup-this-connection)))

          (find-file  "/scp:docker-tmp:")

          (yc/deploy-my-utilies "/scp:docker-tmp:")

          ;; also, open vterm, and ssh to it
          (awhen (get-buffer vterm-buffer-name)
            (unless (y-or-n-p
                     (format "vterm on host %s exists, delete and reconnect? "
                             vterm-buffer-name))
              (error "operation abort..."))
            (kill-buffer it))

          (with-current-buffer (vterm vterm-buffer-name)
            (vterm-send-string "ssh docker-tmp")
            (vterm-send-return)
            ;; (when (length counsel-docker/user-passwd)
            ;;   (vterm-send-string counsel-docker/user-passwd)
            ;;   (vterm-send-return))
            ))

      (error "Failed to parse address from container string: %s" container))))

(provide 'counsel-tramp-docker)

;;; counsel-tramp-docker.el ends here
