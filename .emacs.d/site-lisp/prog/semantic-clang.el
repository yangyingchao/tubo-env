;;; semantic-clang.el --- Setup semantic for clang. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <http://github/yyc>
;; Maintainer: Yang,Ying-chao <yingchao.yang@icloud.com>
;; Created: August 11, 2020
;; Modified: August 11, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yyc/semantic-clang
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun semantic-clang-get-include-paths (lang)
  "Return include paths as gcc uses them for language LANG."
  (let* ((lang "c++")
         (clang-cmd (cond
                     ((string= lang "c") "clang")
                     ((string= lang "c++") "clang++")
                     (t (if (stringp lang)
                            (error "Unknown lang: %s" lang)
                          (error "LANG=%S, should be a string" lang)))))
         (clang-output (semantic-gcc-query clang-cmd "-v" "-E" "-x" lang null-device))
         (lines (split-string clang-output "\n"))
         (include-marks 0)
         (inc-mark "#include ")
         (inc-mark-len (length "#include "))
         inc-path)

    (dolist (line lines)
      (when (> (length line) 1)
        (if (= 0 include-marks)
            (when (and (> (length line) inc-mark-len)
                       (string= inc-mark (substring line 0 inc-mark-len)))
              (setq include-marks (1+ include-marks)))
          (let ((chars (append line nil)))
            (when (= 32 (nth 0 chars))
              (let ((path (substring line 1)))
                (when (and (file-accessible-directory-p path)
                           (file-name-absolute-p path))
                  (add-to-list 'inc-path
                               (expand-file-name path)
                               t))))))))
    inc-path))

(defun semantic-clang-setup ()
  "Setup semantic parsing based on clang."
  (interactive)
  (message "Setting environment for clang...")
  (unless (featurep 'semantic/bovine/gcc)
    (require 'semantic/bovine/gcc))
  (let* ((fields (semantic-gcc-fields (semantic-gcc-query "clang" "-v")))
         (cpp-options `("-E" "-dM" "-x" "c++" ,null-device))
         (query (let ((q (apply 'semantic-gcc-query "cpp" cpp-options)))
                  (if (stringp q)
                      q
                    ;; `cpp' command in `semantic-gcc-setup' doesn't work on
                    ;; Mac, try `gcc'.
                    (apply 'semantic-gcc-query "gcc" cpp-options))))
         (defines (if (stringp query)
                      (semantic-cpp-defs query)
                    (message (concat "Could not query gcc for defines. "
                                     "Maybe g++ is not installed."))
                    nil))
         (ver (cdr (assoc 'version fields)))
         (host (or (cdr (assoc 'target fields))
                   (cdr (assoc '--target fields))
                   (cdr (assoc '--host fields))))
         ;; (prefix (cdr (assoc '--prefix fields)))
         ;; gcc output supplied paths
         ;; FIXME: Where are `c-include-path' and `c++-include-path' used?
         (c-include-path (semantic-gcc-get-include-paths "c"))
         (c++-include-path (semantic-gcc-get-include-paths "c++"))
         (gcc-exe (locate-file "clang" exec-path exec-suffixes 'executable))
         )
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    (when (and (not c-include-path) gcc-exe)
      ;; Fallback to guesses
      (let* ( ;; gcc include dirs
             (gcc-root (expand-file-name ".." (file-name-directory gcc-exe)))
             (gcc-include (expand-file-name "include" gcc-root))
             (gcc-include-c++ (expand-file-name "c++" gcc-include))
             (gcc-include-c++-ver (expand-file-name ver gcc-include-c++))
             (gcc-include-c++-ver-host (expand-file-name host gcc-include-c++-ver)))
        (setq c-include-path
              ;; Replace cl-function cl-remove-if-not.
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include" gcc-include))))
        (setq c++-include-path
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include"
                                      gcc-include
                                      gcc-include-c++
                                      gcc-include-c++-ver
                                      gcc-include-c++-ver-host))))))

    ;;; Fix-me: I think this part might have been a misunderstanding, but I am not sure.
    ;; If this option is specified, try it both with and without prefix, and with and without host
    ;; (if (assoc '--with-gxx-include-dir fields)
    ;;     (let ((gxx-include-dir (cdr (assoc '--with-gxx-include-dir fields))))
    ;;       (nconc try-paths (list gxx-include-dir
    ;;                              (concat prefix gxx-include-dir)
    ;;                              (concat gxx-include-dir "/" host)
    ;;                              (concat prefix gxx-include-dir "/" host)))))

    ;; Now setup include paths etc
    (dolist (D (semantic-clang-get-include-paths "c"))
      (semantic-add-system-include D 'c-mode))
    (dolist (D (semantic-clang-get-include-paths "c++"))
      (semantic-add-system-include D 'c++-mode)
      (let ((cppconfig (list (concat D "/bits/c++config.h") (concat D "/sys/cdefs.h")
                             (concat D "/features.h"))))
        (dolist (cur cppconfig)
          ;; Presumably there will be only one of these files in the try-paths list...
          (when (file-readable-p cur)
            ;; Add it to the symbol file
            (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
                ;; Add to the core macro header list
                (add-to-list 'semantic-lex-c-preprocessor-symbol-file cur)
              ;; Setup the core macro header
              (setq semantic-lex-c-preprocessor-symbol-file (list cur)))
            ))))
    (if (not (boundp 'semantic-lex-c-preprocessor-symbol-map))
        (setq semantic-lex-c-preprocessor-symbol-map nil))
    (dolist (D defines)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map D))
    ;; Needed for parsing macOS libc
    (when (eq system-type 'darwin)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("__i386__" . "")))
    (when (featurep 'semantic/bovine/c)
      (semantic-c-reset-preprocessor-symbol-map))
    nil))

(provide 'semantic-clang)
;;; semantic-clang.el ends here
