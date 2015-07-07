;;; my-net-utils --- Utilities for network.
;;;
;;; Commentary:
;;; Utilities for networks.
;;;
;;; Usage:
;;;
;;;
;;;
;;;
;;; Code:

(require 'mm-decode)
(require 'cl-lib)
(require 'url)
(require 'thingatpt)

(autoload 'browse-url-generic "browse-url" ""  t)
(autoload 'eww "eww" ""  t)

(defconst download-region-version "1.0.0")

;; + customs

(defgroup download-region nil
  "simple in-buffer download manager for Emacs."
  :group 'emacs)

(defcustom download-region-max-downloads 5
  "maximum number of downloads."
  :group 'download-region)

(defface download-region-downloading '((t (:background "#194854")))
  "Face used to show download objects."
  :group 'download-region)

;; + utilities

(defun dlrgn/filter (pred lst)
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun dlrgn/remove-from-list (lstvar elem)
  (cl-labels ((dlrgn/remove (lst elem)
                            (cond ((null lst) nil)
                                  ((equal elem (car lst)) (cdr lst))
                                  (t (cons (car lst) (dlrgn/remove (cdr lst) elem))))))
    (set lstvar (dlrgn/remove (symbol-value lstvar) elem))))

;; + internal fns/vars

;; a "download" is actually an overlay that may have following properties :
;;
;; - dlrgn/buf ... the buffer download process is running in (for active downloads)
;; - dlrgn/url ... the target URL of the download (for pending downloads)
;;
;; - dlrgn/newname ... the filename contents will be saved into
;; - dlrgn/canceled ... when non-nil dlrgn/callback must count it a canceled download

(defvar dlrgn/active-downloads nil)
(defvar dlrgn/pending-downloads nil)

(defvar dlrgn/update-timer nil)

(defun dlrgn/make-download (beg end newname)
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'dlrgn/newname newname)
    (overlay-put ov 'face 'download-region-downloading)
    (overlay-put ov 'intangible t)
    ov))

(defun dlrgn/start-download (url ov)
  (let (buf)
    (cond ((and download-region-max-downloads ; add to the queue
                (>= (length dlrgn/active-downloads)
                    download-region-max-downloads))
           (message "[waiting ...]")
           (overlay-put  'dlrgn/url url)
           (add-to-list 'dlrgn/pending-downloads ov t))
          ((progn                       ; connection succeeded
             (overlay-put ov 'display "[connecting ...]")
             (setq buf (ignore-errors
                         (url-retrieve url 'dlrgn/callback (list ov)))))
           (overlay-put ov 'dlrgn/buf buf)
           ;; if this is the first active download, start the timer
           (when (null dlrgn/active-downloads)
             (setq dlrgn/update-timer (run-with-timer 2 2 'dlrgn/update)))
           (push ov dlrgn/active-downloads))
          ((y-or-n-p "Connection failed. Retry ?") ; retry
           (dlrgn/start-download url ov))
          (t                            ; cancel
           (delete-overlay ov)))))

(defun dlrgn/cancel-download (ov)
  (let ((buf (overlay-get ov 'dlrgn/buf))
        (url (overlay-get ov 'dlrgn/url)))
    (cond (buf                          ; active download
           (overlay-put ov 'dlrgn/canceled t)
           (delete-process (get-buffer-process buf)) ; => cb is called
           )
          (url                          ; pending download
           (overlay-put ov 'dlrgn/canceled t)
           (dlrgn/callback nil ov))
          (t                            ; normal overlays
           nil))))

(defun dlrgn/callback (status ov)
  (let* ((newurl (plist-get status :redirect))
         (err (plist-get status :error))
         (canceled (overlay-get ov 'dlrgn/canceled))
         (buf (overlay-get ov 'dlrgn/buf)))
    (dlrgn/remove-from-list 'dlrgn/active-downloads ov)
    (cond ((or err canceled)            ; error or canceled
           (when buf (kill-buffer buf))
           (delete-overlay ov)
           (message "download aborted."))
          (newurl                       ; redirect
           (kill-buffer buf)
           (when (y-or-n-p (concat "redirect to " newurl))
             (dlrgn/start-download newurl ov)))
          (t                            ; success
           (overlay-put ov 'display "[saving ...]")
           ;; copied from "url-copy-file"
           (with-current-buffer buf
             (let ((handle (mm-dissect-buffer t))
                   (name (overlay-get ov 'dlrgn/newname)))
               (mm-save-part-to-file handle name)
               (kill-buffer buf)
               (set-file-modes name
                               (logior (file-modes name)
                                       #o444))
               ;; chmod: make it readable for others...
               (mm-destroy-parts handle)
               (kill-new (file-name-nondirectory name))))
           (delete-overlay ov)
           (message "download completed.")))
    ;; if any download is pending, start it
    (when dlrgn/pending-downloads
      (let ((ov (car dlrgn/pending-downloads)))
        (dlrgn/start-download (overlay-get ov 'dlrgn/url) ov))
      (setq dlrgn/pending-downloads (cdr dlrgn/pending-downloads)))
    ;; if no download is active, stop the timer
    (when (null dlrgn/active-downloads)
      (cancel-timer dlrgn/update-timer))))

(defun dlrgn/update ()
  (mapc (lambda (ov)
          (overlay-put ov 'display
                       (format "[downloading ... (%.2fMB)]"
                               (/ (buffer-size (overlay-get ov 'dlrgn/buf))
                                  1048576.0))))
        dlrgn/active-downloads))


;; + interactive commands

(defun download-region-cancel (beg end)
  "cancel all downloads in the region."
  (interactive "r")
  (let (dls)
    (while (setq dls
                 (dlrgn/filter (lambda (ov) (overlay-get ov 'dlrgn/newname))
                               (overlays-in beg end)))
      (mapc 'dlrgn/cancel-download dls))))

;;;###autoload
(defun yc/download-url ()
  "Download region as url."
  (interactive)
  (let* ((bounds (cond
                  ((use-region-p)
                   (cons (region-beginning) (region-end)))
                  ((thing-at-point-bounds-of-url-at-point))
                  (t nil)))
         (beg (car bounds))
         (end (cdr bounds))
         (url (if bounds
                  (buffer-substring-no-properties beg end)
                (completing-read "INPUT URL: " nil)))
         (initial (if (file-directory-p "images")
                      "images"))
         (dir (read-directory-name "Save file to: "
                                   default-directory nil t initial))
         (file (convert-standard-filename
                (url-unhex-string (car (last (split-string url "/" t))))))
         (newname (concat dir file))
         (newname (or (and (file-exists-p newname)
                           (not (y-or-n-p "File already exists, overwrite? "))
                           (read-file-name "new filename : " dir file))
                      newname))
         (ov (dlrgn/make-download (or beg (point)) (or end (1+ (point))) newname)))

    (unless (file-exists-p dir)
      (make-directory dir t))

    (dlrgn/start-download url ov)))


;;;###autoload
(defun yc/open-url ()
  "Open url with browser."
  (interactive)
  (let* ((bounds (cond
                  ((use-region-p)
                   (cons (region-beginning) (region-end)))
                  ((thing-at-point-bounds-of-url-at-point))
                  (t (error "There is no region"))))
         (beg (car bounds))
         (end (cdr bounds))
         (url (buffer-substring-no-properties beg end)))
    (if window-system
        (browse-url-generic url)
      (eww url))))

;; + provide

(provide 'my-net-utils)
;;; net-utils.el ends here
