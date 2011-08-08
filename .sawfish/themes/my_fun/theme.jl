;; my_fun/theme.jl

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defgroup my_fun "Mono Theme"
  :group appearance)

(defcustom my_fun:normal-color nil
  "Color of inactive frames (if unset use GTK+ background color)."
  :type (optional color)
  :group (appearance my_fun)
  :after-set after-setting-frame-option)

(defcustom my_fun:active-color nil
  "Color of active frames (if unset use GTK+ selection color)."
  :type (optional color)
  :group (appearance my_fun)
  :after-set after-setting-frame-option)

(defcustom my_fun:text-justify 'left
  "Text is \\w justified in window titles."
  :type (choice left right center)
  :group (appearance my_fun)
  :after-set after-setting-frame-option)

;; 16x16
(define minimize `((inactive . ,(make-image "min_inactive.png"))
                   (focused . ,(make-image "min.png"))
                   (clicked . ,(make-image "min_clicked.png"))))
(define close `((inactive . ,(make-image "close_inactive.png"))
                (focused . ,(make-image "close.png"))
                (clicked . ,(make-image "close_clicked.png"))))

(define maximize `((inactive . ,(make-image "max_inactive.png"))
                   (focused . ,(make-image "max.png"))
                   (clicked . ,(make-image "max_clicked.png"))))

(define restore `((inactive . ,(make-image "restore_inactive.png"))
                  (focused . ,(make-image "restore.png"))
                  (clicked . ,(make-image "restore_clicked.png"))))

(define (maximize-restore w) (if (window-maximized-p w) restore maximize))

(define menu `((inactive . ,(make-image "menu.png"))
	       (clicked . ,(make-image "menu.png"))))

(define titlebar `((inactive . ,(make-image "titlebar_inactive.png"))
                   (focused . ,(make-image "titlebar.png"))))
(define initialised-gtk nil)

(define (rebuild)
  (when (and (or (not my_fun:normal-color)
		 (not my_fun:active-color))
	     (not initialised-gtk))
    (setq initialised-gtk t)
    (require 'gtkrc)
    (gtkrc-call-after-changed
     (lambda () (rebuild-frames-with-style 'my_fun))))
  (rebuild-frames-with-style 'my_fun))

(define (frame-colors w)
  (list (or (window-get w 'frame-inactive-color)
	    (and (not my_fun:normal-color)
		 (car gtkrc-background))
	    my_fun:normal-color)
	(or (window-get w 'frame-active-color)
	    (and (not my_fun:active-color)
		 (nth 3 gtkrc-background))
	    my_fun:active-color)))

(define (text-justifier w)
  (case my_fun:text-justify
    ((left) 24)
    ((right) -64)
    ((center) 'center)))

(define frame
  `(((background . ,titlebar)
     (foreground . "black")
     (text . ,window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -26)
     (height . 26)
     (class . title))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))

    ((background . "black")
     (left-edge . -1)
     (width . 1)
     (top-edge . -0)
     (bottom-edge . -5))

    ((background . "black")
     (right-edge . -1)
     (width . 1)
     (top-edge . -0)
     (bottom-edge . -5))

    ((background . ,frame-colors)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -4)
     (height . 4)
     (class . bottom-border))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -5)
     (height . 1))

    ((background . ,frame-colors)
     (foreground . ,menu)
     (left-edge . 4)
     (top-edge . -20)
     (width . 11)
     (height . 11)
     (class . menu-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,minimize)
     (right-edge . 52)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . iconify-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,maximize-restore)
     (right-edge . 26)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . maximize-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,close)
     (right-edge . 0)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . close-button)
     (removable . t))))

(define shaped-frame
  `(((background . ,frame-colors)
     (foreground . "black")
     (text . ,window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -26)
     (height . 26)
     (class . title))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))

    ((background . "black")
     (left-edge . -1)
     (width . 1)
     (top-edge . -0)
     (height . 23))

    ((background . "black")
     (right-edge . -1)
     (width . 1)
     (top-edge . -0)
     (height . 23))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))

    ((background . ,frame-colors)
     (foreground . ,menu)
     (left-edge . 4)
     (top-edge . -20)
     (width . 11)
     (height . 11)
     (class . menu-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,minimize)
     (right-edge . 45)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . iconify-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,maximize-restore)
     (right-edge . 26)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . maximize-button)
     (removable . t))

    ((background . ,frame-colors)
     (foreground . ,close)
     (right-edge . 0)
     (top-edge . -20)
     (width . 26)
     (height . 14)
     (class . close-button)
     (removable . t))))


(define transient-frame
  `(((background . ,frame-colors)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -26)
     (height . 26)
     (class . title))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))

    ((background . "black")
     (left-edge . -1)
     (width . 1)
     (top-edge . -0)
     (bottom-edge . -5))

    ((background . "black")
     (right-edge . -1)
     (width . 1)
     (top-edge . -0)
     (bottom-edge . -5))

    ((background . ,frame-colors)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -4)
     (height . 4)
     (class . bottom-border))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -5)
     (height . 1))))

(define shaped-transient-frame
  `(((background . ,frame-colors)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -26)
     (height . 26)
     (class . title))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))

    ((background . "black")
     (left-edge . -1)
     (width . 1)
     (top-edge . -0)
     (height . 6))

    ((background . "black")
     (right-edge . -1)
     (width . 1)
     (top-edge . -0)
     (height . 6))

    ((background . "black")
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -0)
     (height . 1))))

(add-frame-style 'my_fun
		 (lambda (w type)
		   (case type
		     ((default) frame)
		     ((transient) transient-frame)
		     ((shaped) shaped-frame)
		     ((shaped-transient) shaped-transient-frame))))

(rebuild)
(custom-set-property 'my_fun:normal-color ':after-set rebuild)
