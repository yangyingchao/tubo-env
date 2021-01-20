;;; tb-dark-theme.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 yyc yang
;;
;; Created: November 11, 2020
;; Modified: November 11, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(deftheme tb-dark     "My dark side...")

(let* ((class '((class color) (min-colors 89)))
       (background "#2d3436";; (if window-system  "#2d3435" "#303030")
                   )
       (current-line "darkolivegreen")
       (comment "#b0bec5")
       (red "#f36c60")
       (yellow "#fff59d")
       (green "#8bc34a")
       (aqua "#81d4fa")
       (blue "#65a7e2")
       (doc-blue (if window-system "#87ceeb" "#5fafaf"))
       (keyword (if window-system "#ff4040" "#ff0000"))
       (purple "#b39ddb"))

  (custom-theme-set-faces
   'tb-dark

   ;; default color
   `(default ((t (:background ,background :foreground "#fefffe"))))

   ;; basic font faces.
   '(font-lock-builtin-face       ((t (:foreground "#32cd32"))))
   '(font-lock-comment-face ((t (:italic t :slant oblique  :foreground "#cd5c5c"))))
   '(font-lock-constant-face      ((t (:foreground "#7fffd4" :bold t))))
   `(font-lock-doc-face ((t ( :foreground ,doc-blue))))
   '(font-lock-function-name-face ((t ( :foreground "#1e90ff" :bold t))))
   `(font-lock-keyword-face       ((t (:foreground ,keyword  :bold nil))))
   ;; '(font-lock-preprocessor-face  ((t (:foreground  "#75507b" :italic nil))))
   '(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
   '(font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
   ;; '(font-lock-string-face        ((t (:foreground "RosyBrown1"))))
   '(font-lock-string-face ((t ( :foreground "#d78700"))))
   '(font-lock-type-face ((t ( :foreground "#32cd32" :bold nil))))
   '(font-lock-variable-name-face  ((t ( :foreground "#daa520" :bold nil))))
   '(font-lock-warning-face  ((t ( :foreground "white" :background "#af0000" :bold t
                                   :underline t))))

   '(align-highlight-change-face  ((t (:background "darkseagreen2" :foreground "blue"))))
   '(align-highlight-nochange-face ((t (:background "SkyBlue4"))))
   '(antlr-font-lock-keyword-face ((t (:foreground "SteelBlue")))) ;%
   '(antlr-font-lock-literal-face ((t (:foreground "PaleVioletRed"))))
   '(antlr-font-lock-ruledef-face ((t (:foreground "DarkGreen"))))
   '(antlr-font-lock-ruleref-face ((t (:foreground "SteelBlue"))))
   '(antlr-font-lock-tokendef-face ((t (:foreground "khaki"))))
   '(antlr-font-lock-tokenref-face ((t (:foreground "LightSteelBlue4"))))
   '(bbdb-company ((t (:italic t :slant italic :foreground "indian red"))))
   '(bbdb-field-name ((t (:bold t :weight bold :foreground "steel blue"))))
   '(bbdb-field-value ((t (:foreground "AntiqueWhite2"))))
   '(bbdb-name ((t (:underline t :foreground "cadet blue"))))

   '(button ((t (:underline t :foreground "#00ffff" :weight bold))))
   '(widget-button ((t (:underline t :foreground "#00ffff" :weight bold))))
   '(calendar-today-face ((t (:underline t :bold t :foreground "cornsilk"))))
   '(change-log-acknowledgement-face ((t (:italic t :slant oblique :foreground "AntiqueWhite3"))))
   '(change-log-conditionals-face ((t (:foreground "Aquamarine"))))
   '(change-log-date-face ((t (:italic t :slant oblique :foreground "BurlyWood"))))
   '(change-log-email-face ((t (:foreground "Aquamarine"))))
   '(change-log-file-face ((t (:bold t :weight bold :foreground "LightSkyBlue" :height 0.9))))
   '(change-log-function-face ((t (:foreground "Aquamarine"))))
   '(change-log-list-face ((t (:foreground "LightSkyBlue"))))
   '(change-log-name-face ((t (:bold t :weight bold :foreground "Gold"))))
   '(comint-highlight-input ((t (:bold t :weight bold))))
   '(comint-highlight-prompt ((t (:foreground "cyan1"))))
   '(compilation-column-number ((t (:foreground "PaleGreen"))))
   '(compilation-error ((t (:bold t :weight bold :foreground "Brown1"))))
   '(compilation-info ((t (:bold t :foreground "LightPink1" :weight bold))))
   '(compilation-line-number ((t (:foreground "LightGoldenrod"))))
   '(compilation-message-face  ((t (:underline t))))
   '(compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
   '(compilation-warning-face ((t (:bold t :foreground "Orange" :weight bold))))
   '(completions-common-part ((t (:width normal :weight normal :slant normal))))
   '(completions-first-difference ((t (:bold t :weight bold))))
   '(cperl-array-face ((t (:foreground "LawnGreen" :background "B;ack" :bold t))))
   '(cperl-hash-face ((t (:foreground "SpringGreen" :background "B;ack" :bold t :italic t))))
   '(cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
   '(css-property ((t (:foreground "light sea green"))))
   '(css-selector ((t (:foreground "LightSteelBlue"))))
   '(cursor ((t ( :foreground "#c6e2ff" :background "white"))))
   '(custom-changed-face ((t (:foreground "wheat" :background "blue"))))
   '(custom-comment-face ((t (:background "dim gray"))))
   '(custom-comment-tag-face ((t (:foreground "gray80"))))
   '(custom-face-tag-face ((t (:bold t :weight bold :height 1.1))))
   '(custom-group-tag-face ((t (:bold t :foreground "light blue" :weight bold :height 1.1))))
   '(custom-group-tag-face-1 ((t (:bold t :foreground "pink" :weight bold :height 1.1))))
   '(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
   '(custom-modified-face ((t (:background "blue" :foreground "white"))))
   '(custom-rogue-face ((t (:background "black" :foreground "pink"))))
   '(custom-saved-face ((t (:underline t))))
   '(custom-set-face ((t (:background "white" :foreground "blue"))))
   '(custom-state-face ((t (:foreground "lime green"))))
   '(diary ((t (:foreground "IndianRed"))))
   '(diary-anniversary ((t (:foreground "Cyan1"))))
   '(diary-button ((t (:background "lightgrey" :foreground "black"  :box (:line-width 2 :style released-button)))))
   '(diary-face ((t (:foreground "IndianRed"))))
   '(diary-time ((t (:foreground "LightGoldenrod"))))


   '(diff-added ((t (:foreground "Green"))))
   '(diff-added-face ((t (:foreground "Green"))))
   '(diff-changed-face ((t (:foreground "Khaki"))))
   '(diff-context-face ((t (:foreground "grey70"))))
   '(diff-file-header ((t (:bold t :background "grey20"  :foreground "ivory1" :weight bold))))
   '(diff-file-header-face ((t (:bold t :background "grey20" :foreground "ivory1" :weight bold))))
   '(diff-function-face ((t (:foreground "SpringGreen1"))))
   '(diff-hunk-header ((t (:slant italic :background "DodgerBlue4"))))
   '(diff-hunk-header-face ((t (:slant italic :background "DodgerBlue4"))))
   '(diff-index-face ((t (:bold t :weight bold :background "SteelBlue4" :foreground "linen" ))))
   '(diff-nonexistent ((t (:bold t :weight bold :background "Black" :foreground "Wheat1"))))
   '(diff-nonexistent-face ((t (:bold t :weight bold :background "Black" :foreground "Wheat1"))))
   `(diff-refine-added ((t (:inherit diff-refine-changed :background ,(if window-system
                                                                          "#22aa22" "#005fff"
                                                                          )))))
   '(diff-refine-change      ((t (:background "MidnightBlue"))))
   '(diff-refine-change-face ((t (:background "MidnightBlue"))))
   '(diff-removed ((t (:foreground "salmon1"))))
   '(diff-removed-face ((t (:foreground "salmon1"))))
   '(dired-directory ((t ( :foreground "#1e90ff" :bold nil))))
   `(diff-header ((t (:background ,(if window-system "SlateBlue4" "#5f5fff")))))

   `(ediff-current-diff-A ((t (:background ,(if window-system "pale green" "#87005f")))))
   `(ediff-current-diff-B ((t (:background  "#556b2f"))))
   '(ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))
   '(ediff-even-diff-A ((t (:foreground "Black" :background "light grey"))))
   '(ediff-even-diff-Ancestor ((t (:foreground "White" :background "Grey"))))
   '(ediff-even-diff-B ((t (:foreground "White" :background "Grey"))))
   '(ediff-even-diff-C ((t (:foreground "Black" :background "light grey"))))

   '(ediff-fine-diff-Ancestor ((t (:foreground "Black" :background "Green"))))
   `(ediff-fine-diff-A ((t (:foreground ,(if window-system "Navy" "White") :background ,(if window-system "sky blue" "color-88")))))
   `(ediff-fine-diff-B ((t (:foreground ,(if window-system "Navy" "White") :background ,(if window-system "cyan" "color-17")))))
   '(ediff-fine-diff-C ((t (:foreground "Black" :background "Turquoise"))))

   '(ediff-odd-diff-A ((t (:foreground "White" :background "Grey"))))
   '(ediff-odd-diff-Ancestor ((t (:foreground "Black" :background "light grey"))))
   '(ediff-odd-diff-B ((t (:foreground "Black" :background "light grey"))))
   '(ediff-odd-diff-C ((t (:foreground "White" :background "Grey"))))


   '(ediff-current-diff-face-A ((t (:foreground "firebrick" :background "pale green"))))
   '(ediff-current-diff-face-Ancestor ((t (:foreground "Black" :background "VioletRed"))))
   '(ediff-current-diff-face-B ((t (:foreground "DarkOrchid" :background "Yellow"))))
   '(ediff-current-diff-face-C ((t (:foreground "Navy" :background "Pink"))))
   '(ediff-even-diff-face-A ((t (:foreground "Black" :background "light grey"))))
   '(ediff-even-diff-face-Ancestor ((t (:foreground "White" :background "Grey"))))
   '(ediff-even-diff-face-B ((t (:foreground "White" :background "Grey"))))
   '(ediff-even-diff-face-C ((t (:foreground "Black" :background "light grey"))))
   '(ediff-odd-diff-face-A ((t (:foreground "White" :background "Grey"))))
   '(ediff-odd-diff-face-Ancestor ((t (:foreground "Black" :background "light grey"))))
   '(ediff-odd-diff-face-B ((t (:foreground "Black" :background "light grey"))))
   '(ediff-odd-diff-face-C ((t (:foreground "White" :background "Grey"))))

   '(eieio-custom-slot-tag-face ((t (:foreground "light blue"))))
   '(eldoc-highlight-function-argument ((t (:bold t :weight bold))))
   '(epa-field-body ((t (:italic t :foreground "turquoise" :slant italic))))
   '(epa-field-name ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
   '(epa-mark ((t (:bold t :foreground "orange" :weight bold))))
   '(epa-string ((t (:foreground "lightyellow"))))
   '(epa-validity-disabled ((t (:italic t :slant italic))))
   '(epa-validity-high ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
   '(epa-validity-low ((t (:italic t :slant italic))))
   '(epa-validity-medium ((t (:italic t :foreground "PaleTurquoise" :slant italic))))

   '(escape-glyph ((t (:foreground "cyan"))))

   ;; Eshell
   '(eshell-ls-archive nil :bold((t ( :foreground "Orchid" ))))
   '(eshell-ls-backup ((t ( :foreground "LightSalmon"))))
   '(eshell-ls-clutter ((t ( :bold nil :foreground "OrangeRed" ))))
   '(eshell-ls-directory ((t ( :foreground "#1e90ff" :bold nil))))
   '(eshell-ls-executable ((t ( :bold nil :foreground "Green" ))))
   '(eshell-ls-missing ((t ( :bold nil :foreground "Red" ))))
   '(eshell-ls-picture ((t (:foreground "Violet"))))
   '(eshell-ls-product ((t ( :foreground "LightSalmon"))))
   '(eshell-ls-readonly ((t ( :foreground "Pink"))))
   '(eshell-ls-special ((t ( :bold nil :foreground "Magenta" ))))
   '(eshell-ls-symlink ((t ( :bold nil :foreground "Cyan"))))
   '(eshell-ls-unreadable ((t ( :foreground "DarkGrey"))))
   '(eshell-prompt ((t (:foreground "#4e9a06" :height 1.05 :bold nil))))
   '(eshell-test-failed-face ((t (:foreground "OrangeRed" :bold t))))
   '(eshell-test-ok-face ((t (:foreground "Green" :bold t))))

   '(excerpt ((t (:italic t))))

   '(file-name-shadow ((t (:foreground "grey70"))))
   '(fixed ((t (:bold t))))
   '(flyspell-duplicate-face ((t (:foreground "IndianRed" :bold t :underline t))))
   '(flyspell-incorrect-face ((t (:foreground "Pink" :bold t :underline t))))

   '(fringe ((t ( :background "#2e3436"))))
   '(goto-address-url-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
   '(goto-address-url-mouse-face ((t (:background "darkseagreen2" :foreground "blue"))))
   '(header-line ((t ( :background "#696969" :bold nil))))
   '(help-argument-name ((t (:italic t :slant italic))))
   '(highlight ((t ( :background "#556b2f"))))
   '(highlight-beyond-fill-column-face ((t (:underline t))))
   '(highlight-changes ((t (:foreground nil :background "#382f2f"))))
   '(highlight-changes-delete ((t (:foreground nil :background "#916868"))))
   '(holiday ((t (:background "chocolate4"))))
   '(holiday-face ((t (:background "chocolate4"))))
   '(ibuffer-dired-buffer-face ((t (:foreground "mediumspringgreen" :weight bold :height 1.1))))
   '(ibuffer-help-buffer-face  ((t (:italic t :slant oblique :foreground "chocolate1"))))
   '(ibuffer-hidden-buffer-face  ((t (:bold t :foreground "Pink" :weight bold))))
   '(ibuffer-occur-match-face ((t (:bold t :foreground "Pink" :weight bold))))
   '(ibuffer-read-only-buffer-face ((t (:foreground "SteelBlue1"))))
   '(ibuffer-special-buffer-face ((t (:foreground "SteelBlue1"))))
   '(ido-first-match ((t (:bold t :weight bold))))
   '(ido-incomplete-regexp ((t (:bold t :weight bold :foreground "Pink"))))
   '(ido-indicator ((t (:background "red1" :foreground "yellow1" :width condensed))))
   '(ido-only-match ((t (:foreground "ForestGreen"))))
   '(ido-subdir ((t (:foreground "red1"))))
   '(info-menu-5 ((t (:underline t))))
   '(info-menu-header ((t (:bold t :weight bold))))
   '(info-node ((t (:bold t :italic t :foreground "yellow"))))
   '(info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
   '(info-xref ((t (:bold t :foreground "DodgerBlue1"))))
   '(isearch ((t ( :foreground "#f5deb3" :background "#cd853f"))))
   '(isearch-fail ((t (:background "red4"))))
   '(isearch-lazy-highlight-face ((t ( :background "#668b8b"))))
   '(isearch-secondary ((t (:foreground "red3"))))
   '(ispell-highlight-face ((t (:background "darkseagreen2" :foreground "blue"))))
   '(ispell-highlight-face ((t (:background "darkseagreen2" :foreground "blue"))))
   '(italic ((t (:italic t))))
   '(js2-builtin-face ((t (:foreground "sandy brown"))))
   '(js2-comment-face ((t (:foreground "dark orchid"))))
   '(js2-constant-face ((t (:foreground "pale violet red"))))
   '(js2-error-face ((t (:background "indian red" :foreground "green" :bold t))))
   '(js2-function-name-face ((t (:foreground "cadet blue"))))
   '(js2-function-param-face ((t (:foreground "IndianRed1"))))
   '(js2-instance-member-face ((t (:foreground "IndianRed1"))))
   '(js2-jsdoc-tag-face ((t (:foreground "medium orchid"))))
   '(js2-jsdoc-type-face ((t (:foreground "medium orchid"))))
   '(js2-jsdoc-value-face ((t (:foreground "medium orchid"))))
   '(js2-keyword-face ((t (:foreground "steel blue"))))
   '(js2-private-function-call-face ((t (:foreground "cadet blue"))))
   '(js2-private-member-face ((t (:foreground "IndianRed1"))))
   '(js2-regexp-face ((t (:foreground "khaki"))))
   '(js2-string-face ((t (:foreground "lemon chiffon"))))
   '(js2-type-face ((t (:foreground "medium sea green"))))
   '(js2-variable-name-face ((t (:foreground "IndianRed1"))))
   '(js2-warning-face ((t (:background "indian red" :foreground "green"))))
   '(lazy-highlight ((t (:background "paleturquoise4"))))
   '(link ((t (:foreground "cyan1" :underline t))))
   '(link-visited ((t (:underline t :foreground "violet"))))
   '(makefile-space ((t (:background "hotpink"))))

   ;; Man
   '(Man-overstrike ((t (:bold t :foreground "DodgerBlue1"))))
   '(Man-underline ((t (:bold nil :foreground "cyan1" :underline t))))

   '(match ((t (:background "RoyalBlue3"))))
   '(message-cited-text ((t (:foreground "red3"))))
   '(message-header-cc ((t (:bold t :foreground "chartreuse1" :weight bold))))
   '(message-header-cc-face ((t (:bold t :foreground "chartreuse1" :weight bold))))
   '(message-header-name ((t (:foreground "green"))))
   '(message-header-name-face ((t (:foreground "green"))))
   '(message-header-newsgroups ((t (:italic t :bold t :foreground "papaya whip" :slant italic :weight bold))))
   '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "papaya whip" :slant italic :weight bold))))
   '(message-header-other ((t (:foreground "ivory"))))
   '(message-header-other-face ((t (:foreground "ivory"))))
   '(message-header-subject ((t (:foreground "OliveDrab1"))))
   '(message-header-subject-face ((t (:foreground "OliveDrab1"))))
   '(message-header-to ((t (:bold t :foreground "floral white" :weight bold))))
   '(message-header-to-face ((t (:bold t :foreground "floral white" :weight bold))))
   '(message-header-xheader ((t (:foreground "DeepSkyBlue1"))))
   '(message-header-xheader-face ((t (:foreground "DeepSkyBlue1"))))
   '(message-mml ((t (:foreground "MediumSpringGreen"))))
   '(message-mml-face ((t (:foreground "MediumSpringGreen"))))
   '(message-separator ((t (:foreground "LightSkyBlue1"))))
   '(message-separator-face ((t (:foreground "LightSkyBlue1"))))
   '(message-url ((t (:bold t :foreground "blue" :weight bold))))
   '(minibuffer-prompt ((t ( :foreground "#00ffff" :bold t))))

   '(mouse ((t (:background "OrangeRed"))))
   '(next-error ((t (:background "blue3"))))
   '(nobreak-space ((t (:foreground "cyan" :underline t))))
   '(org-agenda-date ((t (:foreground "LightSkyBlue"))))
   '(org-agenda-date-weekend ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
   '(org-agenda-restriction-lock ((t (:background "skyblue4"))))
   '(org-agenda-structure ((t (:foreground "LightSkyBlue"))))
   '(org-archived ((t (:foreground "grey70"))))
   '(org-code ((t (:foreground "grey70"))))
   '(org-column ((t (:background "grey30" :slant normal :weight normal :height 81))))
   '(org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
   '(org-date ((t (:foreground "Cyan" :underline t))))
   '(org-done ((t (:bold t :foreground "PaleGreen" :weight bold))))
   '(org-drawer ((t (:foreground "LightSkyBlue"))))
   '(org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
   '(org-formula ((t (:foreground "chocolate1"))))
   '(org-headline-done ((t (:foreground "LightSalmon"))))
   '(org-hide ((t (:foreground "black"))))
   '(org-latex-and-export-specials ((t (:foreground "burlywood"))))
   '(org-level-1 ((t (:foreground "LightSkyBlue"))))
   '(org-level-2 ((t (:foreground "LightGoldenrod"))))
   '(org-level-3 ((t (:foreground "Cyan1"))))
   '(org-level-4 ((t (:foreground "chocolate1"))))
   '(org-level-5 ((t (:foreground "PaleGreen"))))
   '(org-level-6 ((t (:foreground "Aquamarine"))))
   '(org-level-7 ((t (:foreground "LightSteelBlue"))))
   '(org-level-8 ((t (:foreground "LightSalmon"))))
   '(org-link ((t (:foreground "Cyan" :underline t))))
   '(org-mode-line-clock  ((t (:foreground "DarkGreen" :underline t))))
   '(org-scheduled-previously ((t (:foreground "chocolate1"))))
   '(org-scheduled-today ((t (:foreground "PaleGreen"))))
   '(org-sexp-date ((t (:foreground "Cyan"))))
   '(org-special-keyword ((t (:foreground "LightSalmon"))))
   '(org-table ((t (:foreground "RoyalBlue3"))))
   '(org-tag ((t (:bold t :weight bold))))
   '(org-target ((t (:underline t))))
   '(org-time-grid ((t (:foreground "LightGoldenrod"))))
   '(org-todo ((t (:foreground "OrangeRed" :weight bold :bold t :underline t))) )
   '(org-upcoming-deadline ((t (:foreground "chocolate1"))))
   '(org-verbatim ((t (:foreground "grey70" :underline t))))
   '(org-warning ((t (:bold t :weight bold :foreground "Pink"))))
   '(outline-1 ((t (:foreground "LightSkyBlue"))))
   '(outline-2 ((t (:foreground "LightGoldenrod"))))
   '(outline-3 ((t (:foreground "Cyan1"))))
   '(outline-4 ((t (:foreground "chocolate1"))))
   '(outline-5 ((t (:foreground "PaleGreen"))))
   '(outline-6 ((t (:foreground "Aquamarine"))))
   '(outline-7 ((t (:foreground "LightSteelBlue"))))
   '(outline-8 ((t (:foreground "LightSalmon"))))
   '(paren-blink-off ((t (:foreground "black"))))
   '(paren-mismatch-face ((t (:bold t :background "white" :foreground "red"))))
   '(paren-no-match-face ((t (:bold t :background "white" :foreground "red"))))
   '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
   '(region ((t ( :background "#6959cd"))))
   '(scroll-bar ((t (:background "grey75" :foreground "WhiteSmoke"))))
   '(secondary-selection ((t (:background "SkyBlue4"))))
   '(semantic-dirty-token-face ((t (:background "lightyellow"))))
   '(semantic-highlight-edits-face ((t (:background "gray20"))))
   '(semantic-unmatched-syntax-face ((t (:underline "red"))))
   '(senator-intangible-face ((t (:foreground "gray75"))))
   '(senator-momentary-highlight-face ((t (:background "gray30"))))
   '(senator-read-only-face ((t (:background "#664444"))))
   '(sgml-doctype-face ((t (:foreground "orange"))))
   '(sgml-end-tag-face ((t (:foreground "greenyellow"))))
   '(sgml-entity-face ((t (:foreground "gold"))))
   '(sgml-ignored-face ((t (:foreground "gray20" :background "gray60"))))
   '(sgml-sgml-face ((t (:foreground "yellow"))))
   '(sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))
   '(shadow ((t (:foreground "grey70"))))
   '(show-paren-match ((t ( :background "#4f94cd"))))
   '(show-paren-match-face ((t (:background "steelblue3"))))
   '(show-paren-mismatch ((t ( :background "#a020f0"))))
   '(smerge-base ((t (:foreground "orange"))))
   '(smerge-markers ((t (:background "grey30"))))
   '(smerge-mine ((t (:foreground "cyan"))))
   '(smerge-other ((t (:foreground "lightgreen"))))
   '(smerge-refined-change ((t (:background "blue4"))))
   '(speedbar-button-face ((t (:foreground "green3"))))
   '(speedbar-directory-face ((t (:foreground "light blue"))))
   '(speedbar-file-face ((t (:foreground "cyan"))))
   '(speedbar-highlight-face ((t (:background "sea green"))))
   '(speedbar-selected-face ((t (:foreground "red" :underline t))))
   '(speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
   '(speedbar-tag-face ((t (:foreground "yellow"))))
   '(table-cell ((t (:background "blue1" :foreground "gray90"))))
   '(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
   '(tooltip ((t (:background "lightyellow" :foreground "black"))))
   '(trailing-whitespace ((t ( :background "#8b0000"))))
   '(underline ((t (:underline t))))
   '(vcursor ((t (:foreground "blue" :background "cyan" :underline t))))
   '(vertical-border ((t (:background "dim gray"))))

   '(viper-minibuffer-emacs-face ((t (:foreground "Black" :background "darkseagreen2"))))
   '(viper-minibuffer-insert-face ((t (:foreground "Black" :background "pink"))))
   '(viper-minibuffer-vi-face ((t (:foreground "DarkGreen" :background "grey"))))
   '(viper-replace-overlay-face ((t (:foreground "Black" :background "darkseagreen2"))))
   '(viper-search-face ((t (:foreground "Black" :background "khaki"))))

   '(vm-highlight-url-face  ((t (:bold t :italic t :slant italic :weight bold))))
   '(vm-highlighted-header-face  ((t (:bold t :weight bold))))
   '(vm-mime-button-face  ((t (:background "grey75" :foreground "black" :box (:line-width 2 :style released-button)))))
   '(vm-summary-highlight-face  ((t (:bold t :weight bold))))
   '(vm-xface ((t (:background "white" :foreground "black"))))
   '(which-func ((t (:foreground "Blue1"))))
   '(widget ((t (:height 1.2 :background "Gray80" :foreground "black"))))
   '(widget-button ((t (:bold t :weight bold :box (:line-width 2 :style released-button)))))
   '(widget-button-face ((t (:bold t :weight bold :box (:line-width 2 :style released-button)))))
   '(widget-button-pressed ((t (:foreground "red1" :background "lightgrey" :box (:line-width 2 :style pressed-button)))))
   '(widget-button-pressed-face ((t (:foreground "red1" :background "lightgrey" :box (:line-width 2 :style pressed-button)))))
   '(widget-documentation ((t (:foreground "lime green"))))
   '(widget-documentation-face ((t (:foreground "lime green"))))
   '(widget-field ((t (:background "dim gray"))))
   '(widget-field-face ((t (:background "dim gray"))))
   '(widget-inactive ((t (:foreground "grey70"))))
   '(widget-inactive-face ((t (:foreground "grey70"))))
   '(widget-mouse-face  ((t (:background "darkseagreen2" :foreground "blue"))))
   '(widget-single-line-field ((t (:background "dim gray"))))
   '(widget-single-line-field-face ((t (:background "dim gray"))))

   '(mode-line ((t ( :foreground "black" :background "#bfbfbf"))))
   '(mode-line-buffer-id ((t ( :foreground "#000080" :bold t))))
   '(mode-line-emphasis ((t (:bold t :weight bold))))
   '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button :height 0.9)))))
   '(mode-line-inactive ((t ( :foreground "#4d4d4d" :background "#cccccc"))))
   '(mode-line-read-only-face ((t (:inherit 'mode-line-buffer-id :foreground "#4271ae"))))
   '(mode-line-modified-face ((t (:inherit 'mode-line-buffer-id :foreground "#8b0000"))))
   '(mode-line-80col-face ((t (:inherit 'mode-line-face :foreground "black" :background "#eab700"))))

   `(hl-line  ((t (:background ,current-line :underline nil))))

   ;; Magit
   `(magit-branch ((t (:inherit magit-header :foreground "dodger blue" :underline t :weight bold))))
   `(magit-log-head-label-local ((t (:background "Grey13" :foreground "#48cf42" :box 1 :bold t))))
   `(magit-log-head-label-remote ((t (:background "gray85" :foreground "#e60000" :box 1 :bold t))))
   `(magit-diff-add ((,class (:inherit diff-added))))
   `(magit-diff-del ((,class (:inherit diff-removed))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-section-heading ((t ( :foreground "#Ff4500" :bold t))))

   `(magit-item-highlight ((,class (:inherit highlight :background ,current-line))))
   `(magit-log-author ((,class (:foreground ,aqua))))
   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,green))))
   `(magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
   `(magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))))
   `(magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))))
   `(magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
   `(magit-log-sha1 ((,class (:foreground ,yellow))))
   `(magit-hash ((,class (:foreground ,yellow))))
   `(magit-blame-heading ((,class (:foreground "#a9a9a9"))))
   `(magit-section-title ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight  ((t (:background ,current-line))))

   ;; flycheck
   `(flycheck-error-list-filename  ((t (:foreground "#f8f8f8"))))

   ;; term
   `(term-color-blue  ((t (:foreground "#1e90ff" :background "#1e90ff"))))
   `(term-color-cyan  ((t (:foreground "#00f5e9" :background "##00f5e9"))))
   `(term-color-green  ((t (:foreground "#8ae234" :background "##8ae234"))))
   `(term-color-magenta  ((t (:foreground "#ad7fa8" :background "##ad7fa8"))))
   `(term-color-red  ((t (:foreground "#ef2929" :background "##ef2929"))))
   `(term-color-white  ((t (:foreground "#eeeeec" :background "##eeeeec"))))
   `(term-color-yellow  ((t (:foreground "#fce94f" :background "##fce94f"))))

   ;; company
   `(company-tooltip-common-selection ((t (:foreground "#fce94f"))))))

(provide 'tb-dark-theme)
;;; tb-dark-theme.el ends here
