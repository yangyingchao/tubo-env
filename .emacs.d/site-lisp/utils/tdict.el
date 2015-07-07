;;; tdict.el --- Brief introduction here.    -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:
;; Wrapper of osx-dictionary or "sdcv", or youdao-dictionary...
;;; Code:
(require 'popup)

(defmacro aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defcustom tdict-app "sdcv"
  "Application to run to look up for a word."
  :type 'string
  :group 'tdic)

(autoload 'osx-dictionary--search "osx-dictionary")

 ;; for OS X
(defun tdict--search-osx (word)
  "Search result for `WORD' from OS X dictionary."
  (let ((result (osx-dictionary--search word)))
    (if (and result (> (length result) 0 ))
        result nil)))

 ;; for stardict.
(defvar tdict--missing-app-warned nil "Nil.")

(defun tdict--search-sdcv (word)
  "Search result from app for `WORD'."
  (if (executable-find tdict-app)
      (with-temp-buffer
        (call-process tdict-app nil t nil word)
        (goto-char (point-min))
        (search-forward (format "-->%s\n\n" word))
        (buffer-substring-no-properties (point) (point-max)))

    (unless tdict--missing-app-warned
      (setq tdict--missing-app-warned t)
      (warn "Please install sdcv (stardict command-line version) to use tdict"))
    nil))


(defun tdict--search-app (word)
  "Search result from app for `WORD'."
  (if (string= system-type "darwin")
      (tdict--search-osx word)
    (tdict--search-sdcv word)))

 ;; for youdao.

(defconst tdict--youdao-api-url
  "https://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defun tdict--request-youdao (word)
  "Request WORD, return JSON as an alist if successes."
  (let ((url (format tdict--youdao-api-url (url-hexify-string word)))
        json)
    (PDEBUG "REQUEST: " url)
    (with-current-buffer
        (url-retrieve-synchronously url  nil nil 1)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (PDEBUG "RES:" (buffer-string))

      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json
            (if (fboundp 'json-parse-string)
                (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                   :object-type 'alist
                                   :null-object nil
                                   :false-object nil)
              (json-read-from-string
               (buffer-substring-no-properties (point) (point-max)))))
      (kill-buffer (current-buffer)))
    json))

(defun tdict--search-youdao (word)
  "Search from youdao for definition of `WORD'."
  (let* ((json (tdict--request-youdao word))
         (query        (assoc-default 'query       json)) ; string
         (translation  (assoc-default 'translation json)) ; array
         (errorCode    (assoc-default 'errorCode   json)) ; number
         (web          (assoc-default 'web         json)) ; array
         (basic        (assoc-default 'basic       json)) ; alist
         (phonetic     (assoc-default 'phonetic basic))
         )

    (if (and
         (= errorCode 0)
         (or web basic
             (not (string= (elt translation 0) query))))


        (concat (format " %s [%s]\n\n" query phonetic) ;; query & phonetic
                (when translation
                  (format " * Translation\n%s\n\n"
                          (mapconcat
                           (lambda (trans) (concat "   - " trans " "))
                           translation "\n")))

                (when basic
                  (format " * Basic Explains\n%s\n\n"
                          (mapconcat
                           (lambda (explain) (concat "   - " explain  " "))
                           (assoc-default 'explains basic) "\n")))

                (when web
                  (format " Web References\n%s\n"
                          (mapconcat
                           (lambda (k-v)
                             (format "   - %s: %s  "
                                     (assoc-default 'key k-v)
                                     (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                           web "\n")))))))

 ;;

(defgroup tdict nil
  "My dictionary interface for Emacs."
  :group 'tools)


(defun tdict--region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word t)))

(defun tdict--view-result (word)
  "Run dictionary app to look up for WORD."

  (let* (errmsg
         (result (catch 'p-found
                   (dolist (func '(tdict--search-youdao tdict--search-app))
                     (PDEBUG "FUNC: " func)
                     (condition-case var
                         (let ((ret (funcall func word)))
                           (if ret
                               (throw 'p-found ret)))

                       (error
                        (progn
                          (PDEBUG "ERR: " var)
                          (push (concat (symbol-name func)
                                        (s-join " "(cdr var)))
                                errmsg))))))))


    (if result
        (popup-tip result)
      (error (progn
               (PDEBUG "ERRMSG: " errmsg)
               (format
                "Failed to find definition for word: %s, reason: %s." word
                (if errmsg
                    (s-join ", " errmsg) "not found."
                    )))))))

;;;###autoload
(defun tdict-search ()
  "Search word under point."
  (interactive)

  (let* ((default (tdict--region-or-word))
         (prompt  (if default (format "Define (%s): " default)
                    "Define: "))
         (word (if (or current-prefix-arg
                       (not default))
                   (read-string prompt nil nil default) default)))
    (tdict--view-result word)))

(provide 'tdict)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; tdict.el ends here
