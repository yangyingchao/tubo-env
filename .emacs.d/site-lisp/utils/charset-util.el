;;; charset-util.el --- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:


(defun yc/guess-encoding (fn)
  "Guess encoding of FN."
  (if (and fn (file-exists-p fn))
      (if (executable-find "enca")
          (with-temp-buffer
            (call-process "enca" nil t t "-L" "zh" fn)
            (format "File:   %s\nEncode: %s\n" fn (buffer-substring (point-min) (point-max))))
        (format "Can't guess encoding of %s, enca not found.\n" fn))

    (format "Can not file file: %s\n" (or fn (symbol-name fn)))))

(defun yc/detect-charset ()
  "description"
  (interactive)
  (print (unicad-universal-charset-detect (point-max)))
  )

;;;###autoload
(defun yc/list-non-ascii ()
  "List all lines containing non-ascii codes."
  (interactive)
  (let* ((ob (get-buffer-create (generate-new-buffer-name
                                 (concat "Non-Ascii: "
                                         (file-name-sans-extension(buffer-name))))))
         (fn (buffer-file-name))
         res)

    (push (yc/guess-encoding fn) res)
    (save-excursion
      (goto-char (point-min))

      (let ((idx 1)
            rt sp ep ln)
        (while (search-forward-regexp "[^[:ascii:]]" nil t)
          (beginning-of-line)
          (setq sp (point)
                ln (line-number-at-pos))
          (end-of-line)
          (setq ep (point))
          (push (cons ln (format "%02d -- %03d: %s" idx ln (buffer-substring-no-properties sp ep))) res)
          (setq idx (1+ idx)))))

    ;; output res into ob (output-buffer)
    (let* ((r (nreverse res)) item sp ep)
      (set-buffer ob)
      (insert (pop r))
      (if r
          (while (setq item (pop r))
            (insert (cdr item))
            (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
              (overlay-put ov 'LINE (car item))
              (overlay-put ov 'FILE fn))
            (insert "\n"))
        (insert "\n No non-ascii code found in this buffer\n"))
      (setq buffer-read-only t))
    (local-set-key
     (kbd "RET")
     (lambda ()
       (interactive)
       (let ((overlays (overlays-at (point)))
             found ln fn ov)
         (while (and (not found) overlays)
           (setq ov (pop overlays))
           (if (and (setq ln (overlay-get ov 'LINE))
                    (setq fn (overlay-get ov 'FILE)))
               (setq found t)))
         (find-file fn)
         (goto-line ln))))
    (display-buffer ob)))

(provide 'charset-util)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; charset-util.el ends here
