;;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; $Id: attention.el, 01-12-2011
;; Author: Yang Ying-chao <yangyingchao@gmail.com>
;; Maintainer: Yang, Ying-chao <yangyingchao@gmail.com>
;; Copyright (C) 2011,Yang Ying-chao, all rights reversed.
;; Created: 2011
;; Version: 0.0.1
;;
;; Features that might be required by this library:
;;
;;  `speedbar' `advice' `cl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  This file was written to emphasize all KEYWORDS that may need additional
;;  ATTENTIONS.
;;  When read or review codes, this may help programmer to find and locate
;;  such keywords quickly.
;;  Keywords are defined in variable attention-keywords.
;;
;; Below are commands you can use:
;;
;; `attentions-open'                   Open `attentions' window.
;; `attentions-close'                  Close `attentions' window.
;; `attentions-toggle'                 Toggle `attentions' window.
;; `attentions-select-window'          Select `attentions' window.
;; `attentions-refresh-turn-on'        Turn on refresh speedbar content.
;; `attentions-refresh-turn-off'       Turn off refresh speedbar content.
;; `attentions-refresh-toggle'         Toggle refresh speedbar content.
;;
;; Enjoy! ;)
;;

;;; Installation:
;;
;; Copy attentions.el to your load-path and add to your ~/.emacs
;;
;;  (require 'attentions)
;;  (global-set-key (kbd "s-s") 'attentions-toggle)
;;
;; ... or any key binding you like.
;;

;;; Customize:
;;
;; `attentions-width-x'
;;      The `attentions' window width under WINDOW system.
;; `attentions-width-console'
;;      The `attentions' window width under CONSOLE.
;; `attentions-max-width'
;;      The max window width allowed remember.
;; `attentions-delete-windows'
;;      Whether delete other window before showing up.
;; `attentions-skip-other-window-p'
;;      Whether skip `attentions' window when use
;;      command `other-window' select window in cyclic ordering of windows.
;; `attentions-auto-refresh'
;;      Control status of refresh speedbar content.
;; `attentions-right-side'
;;      Puts the speedbar on the right side if non-nil (else left).
;;
;; All above setup can customize by:
;;      M-x customize-group RET attentions RET
;;

;;; Change log:
;; 2011-01-12  attentions.el: New created.


;;; Acknowledgements:
;;
;;      All emacsers ... :)
;;

;;; Bug
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'speedbar)
(require 'advice)
(require 'cl)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Customization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup attentions nil
  "Same frame speedbar."
  :group 'speedbar)

(defcustom attentions-width-x 36
  "Initial width of `attentions-window' under window system."
  :type 'integer
  :group 'attentions)

(defcustom attentions-width-console 24
  "Initial width of `attentions-window' on console."
  :type 'integer
  :group 'attentions)

(defcustom attentions-max-width 50
  "The max width limit that window allowed.
Default, if hide `attentions' window will remember
window width, except the window width larger than
this value."
  :type 'integer
  :group 'attentions)

(defcustom attentions-auto-refresh t
  "Automatically refresh speedbar content when changed directory.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value))
  :group 'attentions)

(defcustom attentions-right-side t
  "Show the speedbar to the right side of the current window.
If nil, the speedbar will appear on the left.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value))
  :group 'attentions)

(defcustom attentions-delete-windows nil
  "Allow the speedbar to delete other windows before showing up.
If nil, speedbar will not touch your window configuration.
Otherwise `delete-other-windows' will be called before showing
the speedbar.

Default is nil."
  :type 'boolean
  :group 'attentions)

(defcustom attentions-skip-other-window-p nil
  "Whether skip `attentions' window with `other-window'.
Default, can use `other-window' select window in cyclic
ordering of windows.  But sometimes we don't want select
`attentions' window use `other-window'.
Just want make `attentions' window as a view sidebar.

So please turn on this option if you want skip
`attentions' window with `other-window'.

Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (when (ad-advised-definition-p 'other-window)
           (attentions-handle-other-window-advice value)))
  :group 'attentions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst attentions-version "0.1.8"
  "Current version.")

(defconst attentions-buffer-name "*SPEEDBAR*"
  "The buffer name of attentions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar attentions-width nil
  "Initial width of speedbar-window.")

(defvar attentions-window nil
  "Speedbar window.")

(defvar attentions-last-refresh-dictionary nil
  "The last refresh dictionary record of 'attentions-refresh'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun attentions-toggle ()
  "Toggle attentions window.
Toggle visibility of attentions by resizing
the `attentions-window' to a minimal width
or the last width when visible.
Use this function to create or toggle visibility
of a speedbar-window.  It will be created if necessary."
  (interactive)
  (if (attentions-exist-p)
      (attentions-close)
    (attentions-open)))

(defun attentions-open ()
  "Create `attentions' window."
  (interactive)
  (if (not (attentions-exist-p))
      (let ((current-window (selected-window)))
        ;; Ensure only one window is there
        ;; when `attentions-delete-windows' is non-nil
        (if attentions-delete-windows
            (delete-other-windows))
        ;; Whether activate `other-window' advice
        ;; to skip `attentions' window when use `other-window'.
        (attentions-handle-other-window-advice attentions-skip-other-window-p)
        ;; Switch buffer
        (if (attentions-buffer-exist-p speedbar-buffer)
            (unless (attentions-window-exist-p attentions-window) ;if `attentions' window is not exist
              (attentions-get-window))
          (if (<= (attentions-current-window-take-width) attentions-width) ;if current window width is narrower than `attentions-width'
              (attentions-recalculate-width)) ;recalculate width of `attentions'
          (attentions-get-window)             ;get `attentions' window that split current window
          (setq speedbar-buffer (get-buffer-create attentions-buffer-name)
                speedbar-frame (selected-frame)
                dframe-attached-frame (selected-frame)
                speedbar-select-frame-method 'attached
                speedbar-verbosity-level 0 ;don't say anything, i don't like ... :)
                speedbar-last-selected-file nil)
          (set-buffer speedbar-buffer)
          (buffer-disable-undo speedbar-buffer) ;make disable in speedbar buffer, otherwise will occur `undo-outer-limit' error
          (speedbar-mode)
          (speedbar-reconfigure-keymaps)
          (speedbar-update-contents)
          (speedbar-set-timer 1)
          ;; Emacs 21 compatibility.
          (when (<= emacs-major-version 21)
            (eval-when-compile
              (with-no-warnings
                (make-local-hook 'kill-buffer-hook))))
          ;; Add speedbar hook.
          (add-hook 'speedbar-before-visiting-file-hook 'attentions-before-visiting-file-hook t)
          (add-hook 'speedbar-before-visiting-tag-hook 'attentions-before-visiting-tag-hook t)
          (add-hook 'speedbar-visiting-file-hook 'attentions-visiting-file-hook t)
          (add-hook 'speedbar-visiting-tag-hook 'attentions-visiting-tag-hook t)
          ;; Add `kill-buffer-hook'.
          (add-hook 'kill-buffer-hook 'attentions-kill-buffer-hook) ;add `kill-buffer-hook'
          ;; Auto refresh speedbar content
          ;; if option `attentions-auto-refresh' is non-nil
          (attentions-handle-auto-refresh attentions-auto-refresh))
        (set-window-buffer attentions-window (get-buffer attentions-buffer-name))
        (set-window-dedicated-p attentions-window t) ;make `attentions-window' dedicated to speedbar-buffer.
        (select-window current-window))
    (message "`attentions' window has exist.")))

(defun attentions-close ()
  "Close `attentions' window and save window width."
  (interactive)
  (if (attentions-exist-p)
      (let ((current-window (selected-window)))
        ;; Remember window width.
        (attentions-select-window)
        (attentions-remember-window-width)
        ;; Close window.
        (if (and (require 'ecb nil t)
                 ecb-activated-window-configuration)
            ;; Toggle ECB window when ECB window activated.
            (progn
              (ecb-deactivate)
              (ecb-activate))
          ;; Otherwise delete dedicated window.
          (delete-window attentions-window)
          (if (attentions-window-exist-p current-window)
              (select-window current-window))))
    (message "`attentions' window is not exist.")))

(defun attentions-select-window ()
  "Force the windows that contain `attentions'."
  (interactive)
  (if (attentions-exist-p)
      (select-window attentions-window)
    (message "`attentions' window is not exist.")))

(defun attentions-refresh-turn-on ()
  "Turn on refresh content automatically."
  (interactive)
  (setq attentions-auto-refresh t)
  (attentions-handle-auto-refresh attentions-auto-refresh t))

(defun attentions-refresh-turn-off ()
  "Turn off refresh content automatically."
  (interactive)
  (setq attentions-auto-refresh nil)
  (attentions-handle-auto-refresh attentions-auto-refresh t))

(defun attentions-refresh-toggle ()
  "Toggle refresh content status."
  (interactive)
  (setq attentions-auto-refresh (not attentions-auto-refresh))
  (attentions-handle-auto-refresh attentions-auto-refresh t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utilise functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun attentions-exist-p ()
  "Return `non-nil' if `attentions' is exist.
Otherwise return nil."
  (and (attentions-buffer-exist-p speedbar-buffer)
       (attentions-window-exist-p attentions-window)))

(defun attentions-window-p ()
  "Return `non-nil' if current window is `attentions' window.
Otherwise return nil."
  (equal attentions-buffer-name (buffer-name (window-buffer))))

(defun attentions-remember-window-width ()
  "Remember window width."
  (let ((win-width (attentions-current-window-take-width)))
    (if (and (attentions-window-p)
             (> win-width 1)
             (<= win-width attentions-max-width))
        (setq attentions-width win-width))))

(defun attentions-recalculate-width ()
  "Calculate the speedbar width with respect of window system."
  (if (and window-system
           (not (string= "pc" window-system)))
      (setq attentions-width attentions-width-x)
    (setq attentions-width attentions-width-console)))

(or attentions-width (attentions-recalculate-width)) ;initialization `attentions-width'

(defun attentions-get-window ()
  "Get `attentions' window."
  (let ((current-window (selected-window))
        ;; Get split new window.
        (new-window (split-window
                     (selected-window)
                     (if attentions-right-side
                         (- (attentions-current-window-take-width) attentions-width)
                       attentions-width)
                     t)))
    ;; Select split window.
    (setq attentions-window
          (if attentions-right-side
              ;; Select right window when `attentions-right-side' is enable.
              new-window
            ;; Otherwise select left widnow.
            current-window))))

(defun attentions-before-visiting-file-hook ()
  "Function that hook `speedbar-before-visiting-file-hook'."
  (select-window (previous-window)))

(defun attentions-before-visiting-tag-hook ()
  "Function that hook `speedbar-before-visiting-tag-hook'."
  (select-window (previous-window)))

(defun attentions-visiting-file-hook ()
  "Function that hook `speedbar-visiting-file-hook'."
  (select-window (previous-window)))

(defun attentions-visiting-tag-hook ()
  "Function that hook `speedbar-visiting-tag-hook'."
  (select-window (previous-window)))

(defun attentions-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq (current-buffer) speedbar-buffer)
    (setq speedbar-frame nil
          dframe-attached-frame nil
          speedbar-buffer nil)
    (speedbar-set-timer nil)
    (remove-hook 'speedbar-before-visiting-file-hook 'attentions-before-visiting-file-hook)
    (remove-hook 'speedbar-before-visiting-tag-hook 'attentions-before-visiting-tag-hook)
    (remove-hook 'speedbar-visiting-file-hook 'attentions-visiting-file-hook)
    (remove-hook 'speedbar-visiting-tag-hook 'attentions-visiting-tag-hook)))

(defun attentions-refresh ()
  "Refresh the context of speedbar."
  (when (and (not (equal default-directory attentions-last-refresh-dictionary)) ;if directory is change
             (not (attentions-window-p))) ;and is not in speedbar buffer
    (setq attentions-last-refresh-dictionary default-directory)
    (speedbar-refresh)))

(defun attentions-handle-auto-refresh (activate &optional echo-show)
  "Automatically refresh speedbar content when changed directory.
Do nothing if option ACTIVATE is nil.
Will display message if ECHO-SHOW is non-nil."
  (if activate
      (progn
        (add-hook 'speedbar-timer-hook 'attentions-refresh)
        (if echo-show (message "Turn on speedbar content refresh automatically.")))
    (remove-hook 'speedbar-timer-hook 'attentions-refresh)
    (if echo-show (message "Turn off speedbar content refresh automatically."))))

(defun attentions-current-window-take-width (&optional window)
  "Return the width that WINDOW take up.
If WINDOW is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 2 edges) (nth 0 edges))))

(defun attentions-window-dedicated-only-one-p ()
  "Only have one non-dedicated window."
  (interactive)
  (let ((window-number 0)
        (dedicated-window-number 0))
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (incf window-number)
         (if (window-dedicated-p w)
             (incf dedicated-window-number)))))
    (if (and (> dedicated-window-number 0)
             (= (- window-number dedicated-window-number) 1))
        t nil)))

(defun attentions-window-exist-p (window)
  "Return `non-nil' if WINDOW is exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun attentions-buffer-exist-p (buffer)
  "Return `non-nil' if BUFFER is exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun attentions-handle-other-window-advice (activate)
  "Handle advice for function `other-window'.
If ACTIVATE is `non-nil' enable advice `attentions-other-window-advice'.
Otherwise disable it."
  (if activate
      (ad-enable-advice 'other-window 'after 'attentions-other-window-advice)
    (ad-disable-advice 'other-window 'after 'attentions-other-window-advice))
  (ad-activate 'other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around attentions-delete-other-window-advice activate)
  "This advice to make `attentions' window can't deleted by command `delete-other-windows'."
  (let ((attentions-active-p (attentions-window-exist-p attentions-window)))
    (if attentions-active-p
        (let ((current-window (selected-window)))
          (dolist (win (window-list))
            (when (and (window-live-p win)
                       (not (eq current-window win))
                       (not (window-dedicated-p win)))
              (delete-window win))))
      ad-do-it)))

(defadvice delete-window (before attentions-delete-window-advice activate)
  "This advice to remember `attentions' window width before deleted.
Use `delete-window' delete `attentions' window have same effect as `attentions-close'."
  ;; Remember window width before deleted.
  (attentions-remember-window-width))

(defadvice pop-to-buffer (before attentions-pop-to-buffer-advice activate)
  "This advice is to fix `pop-to-buffer' problem with dedicated window.
Default, function `display-buffer' can't display buffer in select window
if current window is `dedicated'.

So function `display-buffer' conflict with `attentions' window, because
`attentions' window is `dedicated' window.

That is to say, when current frame just have one `non-dedicated' window,
any functions that use `display-buffer' can't split windows
to display buffer, even option `pop-up-windows' is enable.

And the example function that can occur above problem is `pop-to-buffer'."
  (when (and pop-up-windows                            ;`pop-up-windows' is enable
             (attentions-window-dedicated-only-one-p) ;just have one `non-dedicated' window
             (attentions-window-exist-p attentions-window)
             (not (attentions-window-p))) ;not in `attentions' window
    (split-window-vertically)
    (windmove-down)))

(defadvice other-window (after attentions-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want select `attentions' window use `other-window'.
Just want make `attentions' window as a view sidebar.

This advice can make `other-window' skip `attentions' window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (attentions-window-exist-p attentions-window)
               (eq attentions-window (selected-window)))
      (other-window count))))

(provide 'attention)
;;;;; attention.el ends here
