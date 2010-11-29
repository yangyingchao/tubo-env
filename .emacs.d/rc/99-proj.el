;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: my-project.el, 星期三, 五月 19 2010
(ede-cpp-root-project "ssf" :file "/home/yyc/Work/projects/ssf/src/ssf-project-emacs"
                      :include-path '("/service/include" "/public/include"  "/security/include")
                      :system-include-path '( "/usr/lib/gcc/i686-pc-linux-gnu/4.4.4/include" )

                      )

(provide 'my-project)
;;;;; my-project.el ends here
