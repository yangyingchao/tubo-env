;;; console-mode.el --- Brief introduction here. -*- lexical-binding: t; -*-

;; Author: Yang,Ying-chao <yangyingchao@g-data.com>

;;; Commentary:
;;  Mode to highlight console logs.
;;; Code:
(require '02-functions)
(cdsq console-font-lock-keywords
  `(
    (,(rx bol (group (+? nonl) (or "$" "#")) (+ space))
     (1 font-lock-builtin-face))
    (,(rx bol
          (group (: "#" (+? nonl))) " in" (+ space)
          (group (+? nonl))
          "[" (** 1 2 digit) ":" (= 2 digit) ":" (= 2 digit) "]
"
          (group "$") (+ space)
          )
     (1 font-lock-builtin-face)
     (2 font-lock-constant-face)
     (3 font-lock-keyword-face))
    )
  "")

;;;###autoload
(define-derived-mode console-mode fundamental-mode "Console"
  "Major mode for editing console files
Key definitions:
\\{console-mode-map}"
                                        ; Setup font-lock mode.
  (set (make-local-variable 'font-lock-defaults) '(console-font-lock-keywords)))

(provide 'console-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; console-mode.el ends here
