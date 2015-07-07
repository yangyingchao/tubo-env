;;; sql+.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:

;;; Code:

(require '02-functions)
(require 'sql)
(require 'company-keywords)
(require 's)
(require 'ivy)
(require 'cl)

 ;; vars
(defgroup sql+ nil
  "Description"
    :group 'tools)

(defcustom sql/env-path nil
  "Path containing environment variables."
  :type 'string
  :group 'sql+)

(defun sql/wrap-command (cmd)
  "Wrap CMD by sourcing env file first."
  (if sql/env-path
      (if (file-exists-p sql/env-path)
          (format "source %s; %s" sql/env-path cmd)
        (error "Env File %s not accessible" sql/env-path))
    cmd))

(defun sql/command-to-string (cmd)
  "Execute CMD and return result as a string."
  (interactive)
  (shell-command-to-string (sql/wrap-command cmd)))

 ;; company for sql.
(eval-after-load
    'company-keywords
  (progn
    (unless (assoc 'sql-mode company-keywords-alist)
      (push
       '(sql-mode nil)       company-keywords-alist)
      nil
      )))

(defvar eval-sql/command nil "Command to evaluate sql file.")
(defvar eval-sql/command-verbose-arg nil "Command to evaluate sql file.")
(defvar db/product nil "Product name.")
(defvar db/target-database nil "Target database.")

(defvar company-sql--candidates-cache nil
  "Cache for command arguments to retrieve descriptions for the candidates.")

(defun company-sql-update-candidates ()
  "Update candidates."
  (interactive)
  (PDEBUG "SQL-PRODUCT: " sql-product)
  (puthash sql-product
           (cond
            ((equal sql-product 'postgres)
             (let* ((cmd-prefix (format "psql %s %s %s --no-psqlrc postgres -c "
                                        (if (> (length sql-user) 0)
                                            (format "-U %s" sql-user)
                                          "")
                                        (if (> (length sql-server) 0)
                                            (format "-h %s" sql-server)
                                          "")
                                        (if (= sql-port 0) "" (format "-p %d" sql-port))))
                    (cmd-list '( "'select name from pg_settings;'"
                                 "'select proname from pg_proc;'")))

               (delete-dups
                (mapcar 's-trim (s-split "\n"
                                         (mapconcat
                                          (lambda (X)
                                            (sql/command-to-string (concat cmd-prefix X)))
                                          cmd-list "\n"))))))

            (t
             (warn
              "Can't get candidates for product %s" (symbol-name sql-product))
             nil))
           company-sql--candidates-cache))

(defun company-sql--candidates (prefix)
  "Return candidates for `PREFIX'."

  (unless company-sql--candidates-cache
    (setq company-sql--candidates-cache (make-hash-table :test 'equal)))

  (PDEBUG "SQL-PRODUCT: " sql-product)

  ;; If hash is empty, fill it.
  (unless (gethash sql-product company-sql--candidates-cache)
    (company-sql-update-candidates))

  (all-completions prefix (gethash sql-product company-sql--candidates-cache)))

(cdsq company-sql-modes '(sql-interactive-mode sql-mode))

(defun company-sql (command &optional arg &rest ignored)
  "`company-mode' completion backend for Sql.
Sql is a cross-platform, open-source make system."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sql))
    (init (unless (memq major-mode company-sql-modes)
            (error "Not sql-mode")))
    (prefix
     (and (not (company-in-string-or-comment))
          (let ((res (company-grab-symbol)))
       (if (company-sql--candidates res)
           res))))
    (candidates (company-sql--candidates arg))))


;;;###autoload
(defun eval-sql/filter (process msg)
  (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
  (if (one-window-p)
      (display-buffer (current-buffer))
    (pop-to-buffer (current-buffer)))

  (goto-char (point-max))
  (insert msg)
  (goto-char (point-max))))

(defun eval-sql/sentinel (process event)
  (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
    (goto-char (point-max))
    (insert "\n-- Execution finished..\n\n")
    (read-only-mode 1)
    (goto-char (point-max))))

(defun sql/eval-sql ()
  "Evaluate this file."
  (interactive)

  (aif (buffer-file-name)
      (progn
        (unless eval-sql/command
          (sql/choose-database)
          (unless eval-sql/command
            (error "Command is not set")))
        (when (buffer-modified-p)
          (if (yes-or-no-p "Buffer modified, save before evaluate? ")
              (save-buffer)))
        (with-current-buffer (get-buffer-create "*SQL-Interpreter*")
          (sql-mode)
          (read-only-mode -1)
          (erase-buffer)
          (goto-char (point-max))
          (display-buffer (current-buffer))

          (let* ((command (concat (eval-sql/command) " " it))
                 (process (start-process-shell-command "sql-command" nil command)))
            (insert (format "### EXECUTING COMMAND:\n\n#  %s\n\n" command))
            (set-process-sentinel process 'eval-sql/sentinel)
            (set-process-filter process 'eval-sql/filter))))
    (save-excursion
      (sql-product-interactive))
    (sql-send-buffer)))


(defun sql/choose-dbms ()
  "Choose dbms..."
  (interactive)

  (let ((target
         (let (mappings names)
           (dolist (product sql-product-alist)
             (let ((iden (car product))
                   (name (plist-get (cdr product) :name)))
               (push (cons name iden) mappings)
               (push name names)
               )
             )
           (PDEBUG "MAP" mappings)
           (alist-get (ivy-read "Choose DBMS: " names) mappings nil nil
                      'equal))))


    (setq db/product target)
    (sql-set-product target)))

(defun eval-sql/command ()
  "Get command to execute a query.."
  (format eval-sql/command
          (if current-prefix-arg
              ""
            eval-sql/command-verbose-arg)))

(defun sql/choose-database ()
  "Choose database.."
  (interactive)

  (unless (featurep 'sql)
    (require 'sql))

  (unless db/product
    (sql/choose-dbms))

  (let* ((cmd
          (cond
           ((eq db/product 'postgres)
            (format
             "psql %s --no-psqlrc -lx | grep Name | awk -F '|' '{print $2}'"
             (if (= sql-port 0)
                 ""
               (format "-p %d" sql-port))))
           (t (error "not implemented %s" (symbol-name db/product))))))
    (setq db/target-database
          (ivy-read "Choose database: "
                    (cl-remove-if (lambda (x)
                                 (= (len x) 0) )
                               (mapcar 's-trim (s-split "\n" (sql/command-to-string cmd)))))))

  (cond
   ((eq db/product 'postgres)
    (setq eval-sql/command
          (mapconcat 'identity
                     (list "TERM=xterm"
                           (sql/wrap-command "")
                           "psql"
                           (if (= sql-port 0) "" (format "-p %d" sql-port)) ;; port
                           "%s" ;; template, left for verbose arg.
                           "-d" db/target-database        ;; database
                           "-f"                           ;; file
                           ) " ")
          eval-sql/command-verbose-arg "-a"))
   (t (error "Not implemented for type: %s" (symbol-name db/product)))))


(defun sql/remove-costs ()
  "Remove cost info."
  (interactive)
  (save-excursion
    (goto-char (point-min))

    ;; remove costs: (cost=1268995.52..1268995.52 rows=1958 width=40)..
    (while (search-forward-regexp
            (rx "cost=" (+ digit) "." (+ digit)".."(+ digit) "." (+ digit) (* space))
            nil t)
      (replace-match "")))

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "Foreign Scan") nil t)
      (replace-match "")))

  ;; calculate time (actual time=14071.851..14373.667 rows=2044 blocks=256 loops=1)

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "(actual time="
                (group (+ digit) "." (+ digit))
                ".."
                (group (+ digit) "." (+ digit))
                (+ space) "rows=" (+? nonl) ")")
            nil t)
      (let ((startup (match-string 1))
            (total (match-string 2)))
        (insert (format " -- %.03f ms" (- (string-to-number total) (string-to-number startup))))))))

(provide 'sql+)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; sql+.el ends here
