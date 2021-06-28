;;; debug-utils.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yingchao.yang@icloud.com>

;;; Commentary:

;;; Code:

(require '02-functions)
(require 'cl)
(require 's)
(require 'ivy)
(require 'swiper)

(autoload 'realgud:gdb "realgud"  "" t)



(defun yc/call-gdb (&rest cmd)
  "Call gdb with CMD."
  (let ((cmd-str (s-join " " cmd )))
  (PDEBUG
   "PFX:" current-prefix-arg
   "CMD:" cmd
   "CMD-STR:" cmd-str)
  (if current-prefix-arg
      (gdb (concat "gdb -i=mi " cmd-str))
    (realgud:gdb (concat "gdb "cmd-str)))))

(defun yc/directory-files (dir)
  "List files in DIR except . & .. ."
  (mapcar (lambda (x)
            (concat dir x) )
          (cl-remove-if
           (lambda (x)
             (or (equal "." x)
                 (equal ".." x)))
           (directory-files dir
                            nil))))

(defvar debug-proc-cands nil "Nil.")

(defun build-exec-list (string)
  "Build a header of list containing STRING from CANDS."
  (PDEBUG "ARGS" string debug-proc-cands)
  (setq debug-proc-cands (cond
               ((s-blank? string) debug-proc-cands)
               ((s-ends-with? "/" string)
                (PDEBUG "enter")
                (yc/directory-files (if (s-starts-with? "/" string) string
                                      (concat default-directory string))))
               (t

                (cl-remove-if-not (lambda (x) (s-contains? string
                                                        x)) debug-proc-cands)))))
(defun counsel-find-exec (string)
  "Filter header CANDS fiels for STRING."
  (if (s-contains? " " string)
      (let* ((array (split-string string " "))
             (r-match-cand (s-join ".*?" array))
             (new-cands (build-exec-list (car array))))
        (PDEBUG "Format: " r-match-cand)
        (cl-remove-if-not
         (lambda (x)
           ;; filter func...
           (string-match-p r-match-cand x))
         new-cands))
    (build-exec-list string)))

(defun yc/debug-proc (&optional proc)
  "Debug PROC.."
  (interactive)

  (unless (executable-find "gdb")
    (error "Can't find executable gdb"))

  (unless proc
    (setq proc
          (let* ((debug-proc-cands (yc/directory-files default-directory)))
            (ivy-read "Name of process: "
                      'counsel-find-exec
                        :initial-input ""
                        :dynamic-collection t
                        :unwind (lambda ()
                                  (swiper--cleanup))
                        :caller 'counsel-skeleton))))

  (yc/call-gdb
   (aif (if (file-executable-p proc)
            proc
          (executable-find proc))
       it
     (error "Can't find %s" proc))))

(defun yc/choose-pids (&optional name no-filter reverse)
  "Choose and return PIDs, as list.
If NAME is true, consider applications only when its name matches `NAME'.
If NO-FILTER is true, all pids are returned without filtering.
If REVERSE if t, pid list should be sorted in reversed order."
  (let ((r-match-entry (rx
                        bol (* space)
                        (group (+ digit)) (+ space)
                        (+? nonl) eol))
        (init (s-join "\\|"
                     (remove nil
                             (mapcar
                              (lambda (x)
                                (let ((v (string-to-number x)))
                                  (when (> v 100)
                                    ;; User's pid is larger than 100 in most cases..
                                    (number-to-string v))))
                              (string-split (buffer-substring-no-properties (point-at-bol) (point-at-eol)) nil t)))))

        (ps-cmds (list (concat "ps -u " user-login-name " -o pid -o user -o start_time -o command"))
                 )
        pid-list choosen final)

    (when name
      (push (format "grep \"%s\" " name) ps-cmds))
    (push "grep -v 'ps\\|grep'"  ps-cmds )

    (when reverse
      (push "sort -r" ps-cmds))

    (with-temp-buffer
      (insert (shell-command-to-string (s-join " | " (nreverse ps-cmds))))
      (goto-char (point-min))
      (PDEBUG "PIDS:" (buffer-substring-no-properties (point-min) (point-max)))
      (while (search-forward-regexp ".+?$" nil t)
        (push (match-string 0)  pid-list)))

    (if no-filter
        (setq choosen pid-list)

      (ivy-read "Choose process: " (nreverse pid-list)
                :initial-input init
                :action (lambda (x)
                          (interactive)
                          (push x choosen)))
      )

    (PDEBUG "CHOOSEN:" choosen)

    (dolist (cand choosen)
      (if (string-match r-match-entry cand)
          (push (string-to-number (match-string 1 cand)) final)
        (error "Failed to parse PID")))

    final))

(defun yc/choose-pid (&optional name reverse)
  "Choose and return single pid.
If NAME is true, consider applications only when its name matches `NAME'.
If REVERSE if t, pid list should be sorted in reversed order."
  (let ((pids (yc/choose-pids name nil reverse)))
    (PDEBUG "PIDS:" pids
            "LENGTH: " (length pids))
    (unless (= (length pids) 1)
      (error "More than on process are chosen"))

    (car pids)))

(defun yc/attach-proc (&optional proc)
  "Attach to PROC.  If PRIVILEGED is specified, run as root."
  (interactive)

  (unless (executable-find "gdb")
    (error "Can't find GNU debuger (gdb)"))

  (yc/call-gdb
   "-p"
   (cond
    ((and proc (numberp proc)) (number-to-string proc))
    ((and proc (> (string-to-number proc) 0)) proc)
    (t (number-to-string (yc/choose-pid proc))))))

(defun yc/kill-gdb-buffers ()
  "Kill all buffers used by dead gdb."
  (interactive)
  (let ((killed-buffers 0))
    (mapc
     (lambda (buffer)
       (when (string-match-p
              (rx "*gdb" (+ space) (+ nonl) (+ space) "shell*")
              (buffer-name buffer)
              )
         (unless (get-buffer-process buffer)
           (kill-buffer buffer)
           (setq killed-buffers (1+ killed-buffers)))))

     (buffer-list))
    (message "Total killed buffers: %d" killed-buffers)))


(defvar gdb-kwlist nil "List of gdb keywords.")

(defun company-gdb--candidates (prefix)
  "Return candidates for `PREFIX'."
  (interactive)
  (all-completions prefix gdb-kwlist))

(defun company-gdb (command &optional arg &rest ignored)
  "Comapny backend for gdb."
  (interactive (list 'interactive))

  (unless gdb-kwlist
    (setq gdb-kwlist '("bt" "up" "down"))
    (let ((output (shell-command-to-string "gdb --batch -ex \"help all\"")))
      (dolist (item (s-split "\n" output))
        (when (s-contains? " -- " item)
          (let ((iitem (car (s-split "--" item))))
            (dolist (subitem (s-split (rx (+ (or " " ","))) iitem))
              (unless (s-contains-p "-silently" subitem)
                (add-to-list 'gdb-kwlist  subitem )
                )))))))

  (cl-case command
    (interactive (company-begin-backend 'company-gdb))
    (init t)
    (prefix
     (let ((res (company-grab-symbol)))
       (if (all-completions res gdb-kwlist)
           res)))
    (candidates (company-gdb--candidates arg))))

(defun yc/kill-proc (SIG &optional app no-confrim)
  "Kill process (default APP) with `SIG', or 9 if SIG not provide."
  (interactive "P")
  (PDEBUG "SIG" SIG)

  (unless SIG
    (setq SIG 9))

  (let ((pids (yc/choose-pids))
        cmd)

    (aif (executable-find "kill")
        (push it cmd)
      (error "Can't find killer"))

    (push (format "-%d" SIG) cmd)
    (dolist (pid pids)
      (push (number-to-string pid) cmd))

    (PDEBUG "CMD: " cmd)

    (let ((shell-command (mapconcat 'identity (nreverse cmd) " ")) )
      (PDEBUG "shell-command: " shell-command)
      (message "KILLING %s " shell-command  (shell-command-to-string shell-command)))))


(plist-put! ivy-rich-display-transformers-list
  'yc/debug-core
  '(:columns
    ((ivy-rich-candidate (:width 40))
     (sb/modified-time (:face font-lock-doc-face)))))

(defun yc/debug-core (&rest dirs)
  "Debug app with core dump.
Core is chosen from DIRS."
  (interactive)
  (let* ((dirs nil)
         (dirs (or dirs (list default-directory)))
         (init (cond
                ((region-active-p)
                 (buffer-substring-no-properties (region-beginning) (region-end)))
                ((and (eq major-mode 'dired-mode)
                      (dired-get-filename t t)
                      (string-match-p "core.[0-9]+" (dired-get-filename t t)))
                 (dired-get-filename))
                (t nil)))
         (core
          (let ((cores (flatten-list (mapcar (lambda (dir) (directory-files dir t "core.[0-9]+"))
                                             dirs))))
            (unless cores
              (if (= 1 (length dirs))
                  (error "No cores found in %s" (car dirs))
                (error "No cores found in following dires:\n%s"
                       (mapconcat (lambda (x) (concat "    " x)) dirs "\n"))))

            (PDEBUG "CORES:" cores)
            (if (= (length cores) 1)
                (car cores)
              (ivy-read "Choose core:" cores
                        :initial-input init
                        :caller 'yc/debug-core))))
         (exec (s-trim (shell-command-to-string
                        (format "file %s | awk -F 'execfn: ' '{print $2}' | awk -F ',' '{print $1}' | sed \"s/'//g\" "
                                core))))
         )

    (PDEBUG "CORE:" core
            "EXEC:" exec)

    (if (file-exists-p exec)
        (yc/call-gdb exec core))))


;; utilities to help analyze logs & traces.

(autoload 'yc/cpp-demangle-buffer "c-tuils" nil t)

(defun yc/reprint-hex-string (input)
  "Reprint INPUT, like: 0x000aa --> 0xaa."
  (let (tempStr)
    (let ((case-fold-search nil))
      (setq tempStr (replace-regexp-in-string "\\`0x" "" input )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr )) ; CSS …
      )
    (format "0x%x"(string-to-number tempStr 16))))

(defun yc/addr-2-line (app)
  "Call add2line with executable file set to `APP'.
If APP is nil, try parse app from buffer."
  (if (and app
           (not (file-exists-p app)))
      (error "Can't find file %s" app))

  (unless (executable-find "addr2line")
    (error "Executable addr2line is not available"))

  (let ((ht-addr (make-hash-table :test #'equal))
        (ht-addr2line (make-hash-table :test #'equal))
        (addr-options '("addr2line" ;; "-p" "-i" "-f" "-a" "-C"
                        "-e")))

    (save-excursion
      (goto-char (point-min))

      (while (search-forward-regexp
              (rx
               (group "/" (*? nonl))	;; file name
               "("
               (? (: (*? nonl) "+" (? (group "0x" (+ hex))))) ;; offset
               ")" (* space) "["
               (group "0x" (+ hex))     ;; absolute address
               "]"
               ) nil t)


        (let* ((parsed-app (match-string 1))
               (offset (match-string 2))
               (addr (match-string 3))
               (final-exe  (if (or (not app) (not (string= (file-name-base app) (file-name-base parsed-app))))
                               parsed-app  app))
               (final-addr (if (s-contains-p ".so" final-exe)  offset addr))
               (addrs (gethash final-exe ht-addr nil))
               (k (concat final-exe ":" final-addr))
               )

          (unless (member final-addr addrs)
            (push final-addr addrs))

          (puthash final-exe addrs ht-addr)

          ;; file:addr --> lines
          (puthash
           k
           (append (gethash k ht-addr2line nil) (list (line-number-at-pos)))
           ht-addr2line)




          ;; (PDEBUG "EXE:" final-exe
          ;;         "ADDR:" final-addr)
          ;; (insert " -- " (if (file-exists-p final-exe)
          ;;                    (yc/command-output-to-string "addr2line" final-addr "-e" final-exe)
          ;;                  (format "executable %s does not exist"
          ;;         final-exe)))
          ))

      (let ((yc/debug-log-limit -1))
        (PDEBUG "ht-addr2line" (hash-table-count ht-addr2line)
                ht-addr2line))



      (maphash
       (lambda (key value)
         (let ((result (s-split "\n" (apply #'yc/command-output-to-string
                                            (append addr-options (list  key) value)))))
           (while value
             (let ((addr (pop value))
                   (text (pop result)))
               (dolist (line (gethash (concat key ":" addr) ht-addr2line))
                 (save-excursion
                   (goto-char (point-min))
                   (forward-line line)
                   (goto-char (point-at-eol))
                   (insert " --- " text)))))



           (PDEBUG "INPUT:" key value
                   "RESULT:" result)

           )
         )
       ht-addr)
      )


    )

  ;; ;; second form??
  ;; (save-excursion
  ;;   (goto-char (point-min))
  ;;   (while (search-forward-regexp
  ;;           (rx (group "0x" (+ hex))       ;; addr
  ;;               (+ space)
  ;;               (group (*? nonl))          ;; function name
  ;;               "("
  ;;               (group (+? nonl))          ;; filename
  ;;               "+"
  ;;               (group "0x" (+ hex))       ;; offset
  ;;               ")") nil t)
  ;;     (let* ((addr (match-string 1)))
  ;;       (if (string= (file-name-base app) (file-name-base (match-string 3)))
  ;;           (let ((result (yc/command-output-to-string "addr2line" addr "-e" app)))
  ;;             (replace-match (concat "\\1 \\2 " result)))))))
  )

;;;###autoload
(defun yc/parse-backtrace (&optional app)
  "Analyze current buffer.
It will do several things:
1. `c++filt' is called to demangled function names.
2. `addr2line' is called if APP is provided."
  (interactive)
  (unless app
    (setq app (let* ((choices (list
                               "  Parse automatically from file."
                               "  Choose file interactively."))
                     (action-index (cl-position
                                    (completing-read "Choose executable: "
                                                     choices
                                                     nil
                                                     t)
                                    choices
                                    :test 'equal))
                     (tmp (cl-case action-index
                            (0 nil)
                            (1 (read-file-name "Choose executable: "
                                               nil
                                               nil
                                               t))
                            (t nil))))
                tmp)))

  (setq buffer-read-only nil)
  (yc/addr-2-line app)
  (yc/cpp-demangle-buffer)
  )

(defun uniq-single-host (obuf start end &optional host)
  "Parse and make stack unique for single host. Return t if stack of this host is suspicious."
  (let ((r-match-thread (rx bol "Thread" (* space) (group (+ digit)) (* space)
                            "(Thread" (+? ascii) ":" eol))
        (r-match-suspicious (rx (+? ascii)
                                (or "segfault" "segment fault" "signal handler called"
                                    "abort" "raise" "__assert_fail")
                                (? space) (? "()")
                                (+? ascii)))
        (htable-stack (make-hash-table :test 'equal :size 2048))
        (htable-threads (make-hash-table :test 'equal :size 2048))
        (nr-uniq 0)
        ordered-numbers suspects summary-pos)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (search-forward-regexp r-match-thread end t))
        (forward-char)
        (let* ((pos (point))
               (endp (cond
                      ((search-forward-regexp r-match-thread end t) (point-at-bol))
                      (t end)))
               (stack (buffer-substring-no-properties pos endp))
               (num (gethash stack htable-stack 0)))
          (puthash stack (1+ num) htable-stack)
          (goto-char endp))))

    ;; Sort tacks based on number of threads.
    (maphash (lambda (stack repeated)
               (let ((lst (gethash repeated htable-threads nil)))
                 (puthash repeated (cons stack lst) htable-threads))
               (add-to-list 'ordered-numbers repeated)
               ) htable-stack)
    (sort ordered-numbers '>)

    ;; Now insert stacks and highlight suspicious ones.
    (with-current-buffer obuf
      (read-only-mode -1)
      (goto-char (point-max))
      (message "Parsing stack of host: %s" host)
      (insert "\n\n========= Host " (or host "Unknown Host") ", ")
      (setq summary-pos (point))

      (dolist (number ordered-numbers)
        (let ((stack-list (gethash number htable-threads)))
          (dolist (stack stack-list)
            (setq nr-uniq (1+ nr-uniq))
            (insert (format "\nNumber of Threads: %d" number))
            (let (added-to-list)
              (dolist (str (string-split stack "\n"))
                (insert (format "\n%s" str))
                (when (string-match r-match-suspicious str)
                  (unless added-to-list
                    (setq added-to-list t
                          suspects (cons (1+ (line-number-at-pos)) suspects)))
                  (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                               'face `(:underline (:style wave :color "Red1")))))))))
      (goto-char summary-pos)
      (insert (format "Unique Stacks: %d, suspicious lines: " nr-uniq))
      (if suspects
          (progn
            (setq suspects (nreverse suspects))
            (insert (format "%d" (pop suspects)))
            (while suspects
              (insert (format ", %d" (pop suspects)))))
        (insert "none"))
      (insert ".=========\n"))
    suspects))

;;;###autoload
(defun yc/uniq-stack ()
  "Make stacks unique."
  (interactive)
  (let ((r-match-host (rx bol (+ "-") (* space)
                          (group  (+? (or alnum "." "-" "_")) )
                          (* space) (+ "-") eol))
        (obuf (get-buffer-create (format "Uniq-Stack of: %s" (buffer-name))))
        (nr-hosts 0) suspect-hosts)

    ;; prepare obuf
    (with-current-buffer obuf
      (read-only-mode -1)
      (erase-buffer))


    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp r-match-host nil t)
          (let ((host (match-string 1))
                (pos (match-end 0))
                host-next pos-end pos-next)
            (while (< pos (point-max))
              (if (search-forward-regexp r-match-host nil t) ;; ok, find next one
                  (setq host-next (match-string 1)
                        pos-next (point)
                        pos-end (1- (point-at-bol)))
                (setq host-next nil
                      pos-end (point-max)
                      pos-next (point-max)))
              (setq nr-hosts (1+ nr-hosts))
              (if (uniq-single-host obuf pos pos-end host)
                  (setq suspect-hosts (cons host suspect-hosts)))
              (setq host host-next
                    pos pos-next)))
        (uniq-single-host obuf (point-min)
                          (if (search-forward-regexp "^cmd:.*$" nil t)
                              (match-beginning 0)
                            (point-max)))))

    ;; show this buffer.
    (with-current-buffer obuf
      (when (> nr-hosts 0)
        (goto-char (point-min))
        (insert (format "Total hosts: %d" nr-hosts))
        (insert (if suspect-hosts
                    (format ", %d suspicious hosts: %s" (length suspect-hosts)
                            (mapconcat 'identity suspect-hosts ","))
                  ".")))

      (read-only-mode 1)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))



(defmacro string-concact (str &rest args)
  "Concat string STR and ARGS."
  `(setq ,str (funcall 'concat ,str ,@args)))

(defun yc/parse_segfault ()
  "Parse segfault entry."
  (interactive)
  (let ((r-match-segfault
         (rx bol (? "[" (+ (or digit "."))"]" (+ space))
             (group (+? nonl))   ;; 1 -- app name
             "[" (+? digit) "]:" (+ space)
             "segfault at"     (+ space) (group (+ hex))   ;; 2 -- fault addr
             (+ space) "ip"    (+ space) (group (+ hex))   ;; 3 -- instruction pointer
             (+ space) "sp"    (+ space) (group (+ hex))   ;; 4 -- stack pointer
             (+ space) "error" (+ space) (group (+ digit)) ;; 5 -- error
             (+ space) "in"    (+ space) (group (+? nonl)) ;; 6 -- name of app or lib
             "[" (group (+ hex)) "+" (group (+ hex))       ;; 7 -- base addr, 8: size??
             "]" eol))
        (obuf (get-buffer-create (format "SegmentFault of: %s" (buffer-name)))))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp r-match-segfault nil t)
        (let* ((app (match-string 1))
               (addr (match-string 2))
               (ip (match-string 3))
               (sp (match-string 4))
               (err (match-string 5))
               (name (match-string 6))
               (base-addr (match-string 7))
               (msg (format "%s: segfault in %s" app name)))
          (setq addr   (string-to-number addr 16)
                ip   (string-to-number ip 16)
                sp   (string-to-number sp 16)
                err  (string-to-number err)
                base-addr (string-to-number base-addr 16))

          (string-concact msg (format " at adrress 0x%x, " addr))
          (string-concact msg (format "instruction pointer: 0x%x, " ip))
          ;; (string-concact msg (format "stack pointer: 0x%x," sp))
          (string-concact msg (format "fault offset 0x%x, Reason: " (- ip base-addr)))

          (cond
           ((= 1 (logand err 8)) (string-concact msg "use of reserved bit detected."))
           ((= 1 (logand err 16)) (string-concact msg "fault was an instruction fetch."))
           (t
            (string-concact msg (if (= 0 (logand err 1)) "no page found" "protection fault"))
            (string-concact msg " while executing "
                            (if (= 0 (logand err 2)) "read" "write") " operation")
            (string-concact msg " in "
                            (if (= 0 (logand err 4)) "kernel" "user") " mode.")))
          (with-current-buffer obuf
            (insert msg)
            (insert "\n")))))
    (display-buffer obuf)))


(provide 'debug-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; debug-utils.el ends here
