;;; 055-prog-make.el -- Brief introduction here.

;; Author: Yang,Ying-chao <yangyingchao@icloud.com>

;;; Commentary:

;;; Code:

;; utilities..

 ;; *********** CMake Mode ***************

(use-package cmake-font-lock
  :commands (cmake-font-lock-activate)
  :hook ((cmake-mode . cmake-font-lock-activate)))

(use-package cmake-mode
  :init
  (yc/add-compile-unit 'cmake 88
    (when (or (file-exists-p "CMakeLists.txt")
              (equal file "CMakeLists.txt")
              (equal ext ".cmake"))

      (lambda ()
        (let* ((build-type (if current-prefix-arg "RelWithDebInfo" "Debug"))
               (build-dir (format "%s/cmake_build_%s" default-directory
                                  build-type))
               (cmd-template "cd %s && cmake -DCMAKE_VERBOSE_MAKEFILE=ON -DBUILD_SHARED_LIBS=ON  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=%s .. && make -j%d"))
          (if (file-directory-p build-dir)
              (if (yes-or-no-p "build directory exists, recreate it? ")
                  (progn
                    ;; clear existing caches...
                    (dolist (fn '("CMakeCache.txt" "Makefile" "CMakeFiles"))
                      (let ((ffn (format "%s/%s" build-dir fn)))
                        (when (file-exists-p ffn)
                          (if (file-directory-p ffn)
                              (delete-directory ffn t)
                            (delete-file ffn)))))

                    (unless (file-exists-p build-dir)
                      (mkdir build-dir))

                    (format cmd-template
                            build-dir build-type (yc/get-compiling-threads)))
                (format "cd %s && make -j%d" build-dir (yc/get-compiling-threads)))
            (progn
              (mkdir build-dir)
              (format cmd-template
                      build-dir build-type (yc/get-compiling-threads))))))))
  :commands (cmake-mode cmake-help cmake-help-list-commands)
  :mode (rx (or "CMakeList.txt" "CMakeLists.txt" (: ".cmake" buffer-end)))
  :bind (:map cmake-mode-map
              (;; (kbd "C-c h")
               "h"
               . cmake-help)
              ("\C-cl" . cmake-help-list-commands)
              ("\C-cu" .  unscreamify-cmake-buffer))
  :custom
  (cmake-tab-width 2)
  :config
  (yc/add-company-backends 'cmake-mode 'company-yasnippet 'company-dabbrev-code 'company-dabbrev)
  :hook ((cmake-mode . (lambda ()
                         (yc/lsp--setup "cmake-language-server" "pip install cmake-language-server")))))

(use-package compile
  :commands (compile)
  :init
  (progn
    (custom-set-variables
     '(compilation-scroll-output t)))
  :bind (:map compilation-mode-map

              ([S-f9] ;;(kbd "<S-f9>")
               . (lambda ()
                   (interactive)
                   (let ((cur (point)))
                     (goto-char (point-min))
                     (unless (search-forward-regexp (rx (* space) "error:" (* space)) nil t)
                       (goto-char cur)))))
              ("<f9>" . 'next-error))

  :config
  (defadvice! yc/compile-adv (orig-func command &optional comint)
    "Compile with environment variable LANG set to \"C\".
ORIG-FUNC is called with ARGS."
    :around #'compile
    (funcall orig-func (format "LANG=C %s" command) comint))

  (defadvice! yc/recompile-adv (&rest args)
    "Request user action when compile command is not \"make install/default/all\".
ORIG-FUNC is called with ARGS."
    :before #'recompile
    (unless
        (string-match-p (rx "make" (+ space) (or "install" "default" "all" "-k" "-j"))
                        compile-command)
      (if (yes-or-no-p (format "recompile with command %s" compile-command))
          compile-command
        (error "Action abort"))))

  (add-hook 'compilation-finish-functions
    (lambda (buf str)
      (if (string-match "exited abnormally" str)

          ;;there were errors
          (message "compilation errors, F11 to goto next error.")

        ;;no errors, make the compilation window go away in 0.5 seconds
        ;;        (run-at-time 5.0 nil 'delete-windows-on buf)
        (message "NO COMPILATION ERRORS!")))))


  ;; *********** Makefile ***************

(use-package make-mode
  :mode (((rx bol (or "Makefile" "makefile"
                      (: "." (+ alnum) (: (+ alnum) ".mk")) )
              eol)
          . makefile-mode))
  :hook ((makefile-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))))

(use-package counsel-compile2
  :commands (counsel-compile2 counsel-compile-projectile counsel-make)
  :bind ((;; ,(kbd "<M-f6>")
          [M-f6]. counsel-compile2)
         ;; (kbd "ESC <f6>")
         ([27 f6] . counsel-compile2)
         (;; ,(kbd "<M-S-f6>")
          [M-S-f6]. counsel-compile-projectile)))

(use-package ninja-mode
  :init
  (yc/add-compile-unit 'ninja 77
  (when (file-exists-p "build.ninja")
    (lambda ()"ninja"))))


(use-package compile-utils
  :commands (yc/yank-cmake-command yc/cmake-generate-fake-project)
  :bind (([f6] . do-compile)
         ([f7] . do-run)
         ([C-f6] . compile-or-recompile)))


(provide '058-prog-make)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 055-prog-make.el ends here
