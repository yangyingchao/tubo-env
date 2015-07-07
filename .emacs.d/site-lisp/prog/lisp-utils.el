;;; lisp-utils.el --- emacs lisp utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/yyc>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: September 23, 2020
;; Modified: September 23, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yyc/lisp-utils
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'dash)

(defun call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (setq index (1+ index)))
    (-filter 'car frames)))

(defmacro compile-time-function-name ()
  "Get the name of calling function at expansion time."
  (symbol-name
   (cadadr
    (third
     (car (-filter (lambda (frame) (ignore-errors (equal (car (third frame)) 'defalias)))
              (reverse (call-stack))))))))

(defun my-test-function ()
  "Test function."
  (interactive)
  (message "This function is named '%s'" (compile-time-function-name)))

;; ;; you can see the function body contains the name, not a lookup
;; (lambda nil (message "This function is named '%s'" "my-test-function"))

;; (my-test-function)
;; ;; results in:
;; "This function is named 'my-test-function'"

(provide 'lisp-utils)
;;; lisp-utils.el ends here
