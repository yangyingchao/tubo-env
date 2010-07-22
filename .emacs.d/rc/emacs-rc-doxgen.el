;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: emacs-rc-doxgen.el, 星期二, 六月 29 2010
(require 'doxymacs)
(setq doxymacs-doxygen-style "JavaDoc")

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'python-mode-hook 'doxymacs-mode)
(add-hook 'java-mode-hook 'doxymacs-mode)

(defun doxymacs-parm-tempo-element (parms)
  "Inserts tempo elements for the given parms in the given style."
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
	(cond
	 ((string= doxymacs-doxygen-style "JavaDoc")
	  (list 'l " * " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 ((string= doxymacs-doxygen-style "Qt")
	  (list 'l " " (doxymacs-doxygen-command-char)
		"param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 ((string= doxymacs-doxygen-style "C++")
	  (list 'l "*" (doxymacs-doxygen-command-char)
		" param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms))))
	 (t
	  (doxymacs-invalid-style))))
    nil))

(defconst doxymacs-JavaDoc-file-comment-template
  '(
"/*********************************************************************" > n
" * INVENTEC corporation (c)2008 all rights reserved." > n
"* " "File name   : "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
      "") > n
" * Description: " > n
" * Update:" > n
" * Date        Name              Reason" > n
" * =========== ================= ====================" > n
" * " (insert-today)    "  itc208024(SS2-2)  Create" > n
" *" > n
" * Known issues:" > n
" *" > n
" ********************************************************************/" > n)

 "Default C++-style template for file documentation.")

(defconst doxymacs-JavaDoc-blank-singleline-comment-template
 '("	/* */ " > p)
 "Default JavaDoc-style template for a blank single line doxygen comment.")

(defconst doxymacs-JavaDoc-blank-multiline-comment-template
 '("/**" > n "* " p > n "* " > n "*/" > n)
 "Default JavaDoc-style template for a blank multiline doxygen comment.")

(provide 'emacs-rc-doxgen)
;;;;; emacs-rc-doxgen.el ends here


