;;; edea-light-theme.el --- Color theme based on IntelliJ Light.

;; Copyright (C) 2020 Nicholas Russell

;; Author: Nicholas Russell
;; URL: <https://github.com/nicholasrusell/edea-theme>
;;
;; Version: 1.0
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

;; Initially created with the help of emacs-theme-generator, <https://github.com/mswift42/theme-creator> and spacemacs-light, <https://github.com/nashamri/spacemacs-theme>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; This is a color theme for spacemacs <https://github.com/syl20bnr/spacemacs>.

;;; Code:
(require 'seq)

(defgroup edea-theme nil
  "edea-theme options"
  :group 'faces)

(deftheme edea-light "IntelliJ Light theme")

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun transform-color (color)
  (let* ((subs (substring color 1))
         (red-hex (float (string-to-number (substring subs 0 2) 16)))
         (green-hex (float (string-to-number (substring subs 2 4) 16)))
         (blue-hex (float (string-to-number (substring subs 4 6) 16)))
         (red-dec (lsh (round (/ (* red-hex 7.0) 255.0)) 5))
         (green-dec (lsh (round (/ (* green-hex 7.0) 255.0)) 3))
         (blue-dec (round (/ (* blue-hex 3.0) 255.0))))
    (print red-hex)
    (print green-hex)
    (print blue-hex)
    (print red-dec)
    (print green-dec)
    (print blue-dec)
    (concat "#" (format "%x" (+ red-dec green-dec blue-dec)))))

(defun get-color (color)
  (if (true-color-p)
      color
    (transform-color color)))

(defun edea-transform-idea-color (colors color)
  (let ((colors-value (seq-find
                       (lambda (c)
                         (equalp color (car c)))
                       colors)))
    (when colors-value
      (seq-reduce
       (lambda (acc cur)
         (let ((key (car cur)))
           (append
            acc
            (cond
             ((equalp key ':fg) (list :foreground (car (cdr cur))))
             ((equalp key ':bg) (list :background (car (cdr cur))))
             ((equalp key ':esm) nil) ; TODO
             ((equalp key ':styles)
              (append
               (when (seq-contains cur ':italic)
                 (list :slant 'italic))
               (when (seq-contains cur ':bold)
                 (list :weight 'bold))))
             ((equalp key ':effects)
              (let ((effect (car (cdr cur))))
                (cond
                 ((equalp effect ':bordered) (list :box (car (cdr (cdr cur)))))
                 ((equalp effect ':dotted-line) (list :underline (list :color (car (cdr (cdr cur))) :style 'wave)))
                 ((equalp effect ':strikethrough) (list :strike-through (car (cdr (cdr cur)))))
                 ((equalp effect ':underscored) (list :underline (list :color (car (cdr (cdr cur))) :style 'line)))
                 ((equalp effect ':underwaved) (list :underline (list :color (car (cdr (cdr cur))) :style 'wave)))
                 (t nil))))
             ((equalp key ':ref) (edea-transform-idea-color colors (car (cdr cur))))
             (t nil)))))
       (car (cdr colors-value))
       '()))))

;;;; Theme definition:

(let ((idea-colors
       '(;;; General
         ;; Code
         (edea-general-code-identifier-under-caret ((:bg "#EDEBFC")))
         (edea-general-code-identifier-under-caret-write ((:bg "#FCE8F4")))
         (edea-general-code-injected-language-fragment ((:bg "#EDFCED")))
         (edea-general-code-line-number ((:fg "#ADADAD")))
         (edea-general-code-line-number-on-caret-row ((:fg "#ADADAD")
                                                      (:bg "#FCFAED")))
         (edea-general-code-matched-brace ((:bg "#93D9D9")))
         (edea-general-code-method-separator ((:fg "#C0C0C0")))
         (edea-general-code-todo-defaults ((:fg "#008DDE")
                                           (:esm "#54AAE3")
                                           (:styles :italic)))
         (edea-general-code-unmatched-brace ((:bg "#FFDCDC")))

         ;; Editor
         (edea-general-editor-breadcrumbs-current ((:fg "#585858")
                                                   (:bg "#D6D6D6")))
         (edea-general-editor-breadcrumbs-default ((:fg "#585858")))
         (edea-general-editor-breadcrumbs-hovered ((:fg "#585858")
                                                   (:bg  "#E0E0E0")))
         (edea-general-editor-breadcrumbs-inactive ((:fg "#585858")))
         (edea-general-editor-caret ((:fg "#000000")))
         (edea-general-editor-caret-row ((:bg "#FCFAED")))
         (edea-general-editor-guides-hard-wrap-guide ((:fg "#E0E0E0")))
         (edea-general-editor-guides-indent-guide ((:bg "#E6E6E6")))
         (edea-general-editor-guides-indent-guide-selected ((:bg "#C8C8C8")))
         (edea-general-editor-guides-visual-guides ((:fg "#EEEEEE")))
         (edea-general-editor-gutter ((:bg "#F2F2F2")))
         (edea-general-editor-notification ((:bg "#FFF8D1")))
         (edea-general-editor-selection ((:bg "#A6D2FF")))
         (edea-general-editor-selection-line-below ((:fg "#E4E4E4")))
         (edea-general-editor-tear-line ((:fg "#D4D4D4")))
         (edea-general-editor-tear-line-selection ((:fg "#9C9C9C")))

         ;; Errors and Warnings
         (edea-general-errors-warns-deprecated-symbol ((:effects :strikethrough "#404040")))
         (edea-general-errors-warns-deprecated-symbol-marked-for-removal ((:effects :strikethrough "#FF0000")))
         (edea-general-errors-warns-duplicate-from-server ((:bg "#F5F7F0")))
         (edea-general-errors-warns-error ((:esm "#CF5B56")
                                           (:effects :underwaved "#FF0000")))
         (edea-general-errors-warns-problem-from-server ((:esm "#E69317")
                                                         (:effects :underwaved "#F49810")))
         (edea-general-errors-warns-runtime-problem-error-stripe-mark ((:esm "#CF5B56")
                                                                       (:effects :dotted-line "#F49810")))
         (edea-general-errors-warns-typo ((:effects :underwaved "#B0D1AB")))
         (edea-general-errors-warns-unknown-symbol ((:fg "#F50000")))
         (edea-general-errors-warns-unused-symbol ((:fg "#808080")))
         (edea-general-errors-warns-warning ((:bg "#F5EAC1")
                                             (:esm "#EBC700")))
         (edea-general-errors-warns-weak-warning ((:esm "#D9CFAD")
                                                  (:effects :underwaved "#CCCCCC")))

         ;; Hyperlinks
         (edea-general-hyperlinks ((:fg "#006DCC")
                                   (:effects :undercored "#006DCC")))

         ;; Line Coverage
         (edea-general-line-coverage-full ((:fg "#CCFFCC")
                                           (:styles :bold)))
         (edea-general-line-coverage-partial ((:fg "#FFFFCC")
                                              (:styles :bold)))
         (edea-general-line-coverage-uncovered ((:fg "#FFCCCC")
                                                (:styles :bold)))

         ;; Popups and Hints
         (edea-general-popups-hints-code-lens ((:effects :bordered "#595959")))
         (edea-general-popups-hints-documentation ((:bg "#F7F7F7")))
         (edea-general-popups-hints-error ((:bg "#FFDCDC")))
         (edea-general-popups-hints-information ((:bg "#F7F7F7")))
         (edea-general-popups-hints-promotion-pane ((:bg "#E6EDF7")))
         (edea-general-popups-hints-question ((:bg "#B5D0FB")))
         (edea-general-popups-hints-recent-locations-selection ((:bg "#E9EEF5")))

         ;; Search Results
         (edea-general-search-results-search-result ((:bg "#CCCCFF")
                                                     (:effects :bordered "#000000")))
         (edea-general-search-results-search-result-write-access ((:bg "#FFCDFF")))
         (edea-general-search-results-text-search-result ((:fg "#000000")
                                                          (:bg "#FFE959")
                                                          (:esm "#99661F")))

         ;; Templates
         (edea-general-templates-live ((:effects :bordered "#E63900")))
         (edea-general-templates-template-variable ((:fg "#7F0000")
                                                    (:styles :italic)))

         ;; Text
         (edea-general-text-default ((:fg "#080808")
                                     (:bg "#FFFFFF")))
         (edea-general-text-deleted ((:fg "#C3C3C3")
                                     (:bg "#F0F0F0")))
         (edea-general-text-folded ((:fg "#414D41")
                                    (:bg  "#E9F5E6")))
         (edea-general-text-folded-w-highlighting ((:bg "#ADD9C7")))
         (edea-general-text-read-only-fragment ((:bg "#CFE7FF")))
         (edea-general-text-soft-wrap-sign ((:fg "#C0C0C0")))
         (edea-general-text-whitespaces ((:fg "#ADADAD")))

                ;;; Language Defaults
         ;; Bad Character
         (edea-lang-bad-character ((:bg "#FFCCCC")))

         ;; Braces and Operators
         (edea-lang-braces-ops-braces ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-brackets ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-comma ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-dot ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-operator-sign ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-parentheses ((:ref edea-general-text-default)))
         (edea-lang-braces-ops-semicolon ((:ref edea-general-text-default)))

         ;; Classes
         (edea-lang-classes-name ((:ref edea-lang-identifiers-default)))
         (edea-lang-classes-reference ((:ref edea-lang-identifiers-default)))
         (edea-lang-classes-instance-field ((:fg "#871094")))
         (edea-lang-classes-instance-method ((:ref edea-lang-identifiers-function-declaration)))
         (edea-lang-classes-interface-name ((:ref edea-lang-identifiers-default)))
         (edea-lang-classes-static-field ((:fg "#871094")
                                          (:styles :italic)))
         (edea-lang-classes-static-method ((:fg "#00627A")
                                           (:styles :italic)))

         ;; Comments
         (edea-lang-comments-block ((:fg "#8C8C8C")
                                    (:styles :italic)))
         (edea-lang-comments-doc ((:ref edea-lang-comments-block))) ;; TODO
         (edea-lang-comments-line ((:fg "#8C8C8C")
                                   (:styles :italic)))

         ;; Identifiers
         (edea-lang-identifiers-constant ((:fg "#871094")
                                          (:styles :italic)))
         (edea-lang-identifiers-default ((:fg "#000000")))
         (edea-lang-identifiers-function-call ((:ref edea-lang-identifiers-default)))
         (edea-lang-identifiers-function-declaration ((:fg "#00627A")))
         (edea-lang-identifiers-global-variable ((:ref edea-lang-identifiers-default)))
         (edea-lang-identifiers-label ((:effects :underscored "#808080")))
         (edea-lang-identifiers-local-variable ((:ref edea-lang-identifiers-default)))
         (edea-lang-identifiers-parameter ((:ref edea-lang-identifiers-default)))
         (edea-lang-identifiers-predefined-symbol ((:styles :italic)))
         (edea-lang-identifiers-reassigned-local-variable ((:effects :underscored "#909090")))
         (edea-lang-identifiers-reassigned-parameter ((:effects :underscored "#909090")))

         ;; Inline Parameter Hints
         (edea-lang-param-hints-current ((:fg "#5B5B5B")
                                         (:bg "#BCDAF7")))
         (edea-lang-param-hints-default ((:fg "#7A7A7A")
                                         (:bg "#EDEDED")))
         (edea-lang-param-hints-highlighted ((:fg "#5B5B5B")
                                             (:bg "#CCCCCC")))

         ;; Keyword
         (edea-lang-keyword ((:fg "#0033B3")))

         ;; Markup
         (edea-lang-markup-attribute ((:fg "#174AD4")))
         (edea-lang-markup-entity ((:fg "#174AD4")))
         (edea-lang-markup-tag ((:ref edea-lang-identifiers-default)))

         ;; Metadata
         (edea-lang-metdata ((:fg "#9E880D")))

         ;; Number
         (edea-lang-number ((:fg "#1750EB")))

         ;; String
         (edea-lang-string ((:fg "#067D17")))
         (edea-lang-string-escape-sequence-invalid ((:fg "#067D17")
                                                    (:bg "#FFCCCC")))
         (edea-lang-string-escape-sequence-valid ((:fg "#0037A6")))

         ;; Template Language
         (edea-lang-template-language ((:bg "#F7FAFF")))

         ;;; Clojure
         (edea-clj-bad-character ((:ref edea-lang-bad-character)))
         (edea-clj-brace ((:ref edea-lang-braces-ops-braces)))
         (edea-clj-character ((:ref edea-lang-string)))
         (edea-clj-head-symbol ((:ref edea-lang-keyword)))
         (edea-clj-head-symbol-clj-core ((:ref edea-lang-identifiers-default)))
         (edea-clj-keyword ((:ref edea-lang-classes-static-field)))
         (edea-clj-line-comment ((:ref edea-lang-comments-line)))
         (edea-clj-literal ((:ref edea-lang-keyword)))
         (edea-clj-number ((:ref edea-lang-number)))
         (edea-clj-parenthesis ((:ref edea-lang-braces-ops-parentheses)))
         (edea-clj-string ((:ref edea-lang-string)))
         (edea-clj-symbol ((:ref edea-lang-identifiers-default)))))
      ;;(seq-do
      ;; (lambda (color)
      ;;   (print (edea-transform-idea-color idea-colors (car color))))
      ;; idea-colors))

      (class '((class color) (min-colors 89)))

      ;; IDEA
      (generic-bg "#F2F2F2")
      (generic-bg-hover "#DADADA")
      (generic-bg-separator "#D1D1D1")
      (tab-fg "#000000")
      (tab-fg-modified "#0032A0")
      (tab-bg-selected "#FFFFFF")
      (tab-underline-active "#40A3C9")
      (tab-underline-inactive "#9CA7B8")
      (project-git-added-face "#0A7700")
      (project-git-conflict-face "#993300")
      (project-git-ignored-fg "#727238")
      (project-git-modified-fg "#1539A1")
      (project-git-untracked-fg "#6CBECA")
      (project-header-button-fg "#AFAFAF")

      ;; generic
      (act1          (if (true-color-p) "#e7e5eb" "#d7dfff"))
      (act2          (if (true-color-p) "#d3d3e7" "#afafd7"))
      (base          (if (true-color-p) "#655370" "#5f5f87"))
      (base-dim      (if (true-color-p) "#a094a2" "#afafd7"))
      (bg1           (if (true-color-p) "#fbf8ef" "#ffffff"))
      (bg2           (if (true-color-p) "#efeae9" "#e4e4e4"))
      (bg3           (if (true-color-p) "#e3dedd" "#d0d0d0"))
      (bg4           (if (true-color-p) "#d2ceda" "#bcbcbc"))
      (bg-alt        (if (true-color-p) "#efeae9" "#e4e4e4"))
      (border        (if (true-color-p) "#b3b9be" "#b3b9be"))
      (cblk          (if (true-color-p) "#655370" "#5f5f87"))
      (cblk-bg       (if (true-color-p) "#e8e3f0" "#ffffff"))
      (cblk-ln       (if (true-color-p) "#9380b2" "#af5fdf"))
      (cblk-ln-bg    (if (true-color-p) "#ddd8eb" "#dfdfff"))
      (cursor        (if (true-color-p) "#100a14" "#121212"))
      (const         (if (true-color-p) "#4e3163" "#8700af"))
      (comment       (if (true-color-p) "#2aa1ae" "#008787"))
      (comment-light (if (true-color-p) "#a49da5" "#008787"))
      (comment-bg    (if (true-color-p) "#ecf3ec" "#ffffff"))
      (comp          (if (true-color-p) "#6c4173" "#8700af"))
      (err           (if (true-color-p) "#e0211d" "#e0211d"))
      (func          (if (true-color-p) "#6c3163" "#8700af"))
      (head1         (if (true-color-p) "#3a81c3" "#268bd2"))
      (head1-bg      (if (true-color-p) "#edf1ed" "#ffffff"))
      (head2         (if (true-color-p) "#2d9574" "#2aa198"))
      (head2-bg      (if (true-color-p) "#edf2e9" "#ffffff"))
      (head3         (if (true-color-p) "#67b11d" "#5faf00"))
      (head3-bg      (if (true-color-p) "#edf2e9" "#ffffff"))
      (head4         (if (true-color-p) "#b1951d" "#875f00"))
      (head4-bg      (if (true-color-p) "#f6f1e1" "#ffffff"))
      (highlight     (if (true-color-p) "#d3d3e7" "#d7d7ff"))
      (highlight-dim (if (true-color-p) "#e7e7fc" "#d7d7ff"))
      (keyword       (if (true-color-p) "#3a81c3" "#268bd2"))
      (lnum          (if (true-color-p) "#a8a8bf" "#af87af"))
      (mat           (if (true-color-p) "#ba2f59" "#af005f"))
      (meta          (if (true-color-p) "#da8b55" "#df5f5f"))
      (str           (if (true-color-p) "#2d9574" "#2aa198"))
      (suc           (if (true-color-p) "#42ae2c" "#00af00"))
      (ttip          (if (true-color-p) "#8c799f" "#5f5f87"))
      (ttip-sl       (if (true-color-p) "#c8c6dd" "#afafff"))
      (ttip-bg       (if (true-color-p) "#e2e0ea" "#dfdfff"))
      (type          (if (true-color-p) "#ba2f59" "#af005f"))
      (var           (if (true-color-p) "#715ab1" "#af5fd7"))
      (war           (if (true-color-p) "#dc752f" "#dc752f"))
      ;; colors
      (aqua          (if (true-color-p) "#2d9574" "#2aa198"))
      (aqua-bg       (if (true-color-p) "#edf2e9" "#ffffff"))
      (green         (if (true-color-p) "#67b11d" "#5faf00"))
      (green-bg      (if (true-color-p) "#edf2e9" "#ffffff"))
      (green-bg-s    (if (true-color-p) "#dae6d0" "#ffffff"))
      (cyan          (if (true-color-p) "#21b8c7" "#008080"))
      (red           (if (true-color-p) "#f2241f" "#d70008"))
      (red-bg        (if (true-color-p) "#faede4" "#ffffff"))
      (red-bg-s      (if (true-color-p) "#eed9d2" "#ffffff"))
      (blue          (if (true-color-p) "#3a81c3" "#268bd2"))
      (blue-bg       (if (true-color-p) "#edf1ed" "#d7d7ff"))
      (blue-bg-s     (if (true-color-p) "#d1dcdf" "#d7d7ff"))
      (magenta       (if (true-color-p) "#a31db1" "#800080"))
      (yellow        (if (true-color-p) "#b1951d" "#875f00"))
      (yellow-bg     (if (true-color-p) "#f6f1e1" "#ffffff")))

  (custom-theme-set-faces
   'edea-light
 ;;;;; basics
   `(cursor ((,class (:background "#000000"))))
   `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
   `(default ((,class ,(edea-transform-idea-color idea-colors 'edea-general-text-default))))
   `(default-italic ((,class (:italic t))))
   `(error ((,class ,(edea-transform-idea-color idea-colors 'edea-general-errors-warns-error))))
   `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
   `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
   `(font-lock-builtin-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-keyword))))
   `(font-lock-comment-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-comments-block))))
   `(font-lock-constant-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-identifiers-constant))))
   `(font-lock-doc-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-string))))
   `(font-lock-function-name-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-function-declaration))))
   `(font-lock-function-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-keyword))))
   `(font-lock-keyword-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-keyword))))
   `(font-lock-negation-char-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-identifiers-constant))))
   `(font-lock-preprocessor-face ,(edea-transform-idea-color idea-colors 'edea-lang-identifiers-metadata))
   `(font-lock-reference-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-identifiers-constant))))
   `(font-lock-string-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-string))))
   `(font-lock-type-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-keyword))))
   `(font-lock-variable-name-face ((,class,(edea-transform-idea-color idea-colors 'edea-lang-local-variable))))
   `(font-lock-warning-face ((,class ,(edea-transform-idea-color idea-colors 'edea-general-errors-warns-warning))))
   `(fringe ((,class ,(edea-transform-idea-color idea-colors 'edea-general-text-default))))
   `(header-line ((,class ,(edea-transform-idea-color idea-colors 'edea-general-text-default))))
   `(highlight ((,class ,(edea-transform-idea-color idea-colors 'edea-general-editor-selection))))
   `(hl-line ((,class ,(edea-transform-idea-color idea-colors 'edea-general-editor-caret-row))))
   `(isearch ((,class ,(edea-transform-idea-color idea-colors 'edea-general-search-results-search-result))))
   `(lazy-highlight ((,class ,(edea-transform-idea-color idea-colors 'edea-general-search-results-text-search-result))))
   `(link ((,class ,(edea-transform-idea-color idea-colors 'edea-general-hyperlinks))))
   `(link-visited ((,class ,(edea-transform-idea-color idea-colors 'edea-general-hyperlinks))))
   `(match ((,class ,(edea-transform-idea-color idea-colors 'edea-general-search-results-text-search-result))))
   `(minibuffer-prompt ((,class (:inherit bold :foreground "#3A81C3"))))
   `(page-break-lines ((,class ,(edea-transform-idea-color idea-colors 'edea-general-editor-tear-line))))
   `(region ((,class ,(edea-transform-idea-color idea-colors 'edea-general-editor-selection))))
   `(secondary-selection ((,class (:background ,bg3))))
   `(shadow ((,class (:foreground ,base-dim))))
   `(success ((,class (:foreground ,suc))))
   `(tooltip ((,class ,(edea-transform-idea-color idea-colors 'edea-general-text-default))))
   `(vertical-border ((,class ,(edea-transform-idea-color idea-colors 'edea-general-editor-tear-line))))
   `(warning ((,class ,(edea-transform-idea-color idea-colors 'edea-general-errors-warns-warning))))
   `(widget-button-pressed ((,class (:foreground ,green))))
   ;;;; Clojure
   `(clojure-keyword-face ((,class ,(edea-transform-idea-color idea-colors 'edea-clj-keyword))))
   `(clojure-character-face ((,class ,(edea-transform-idea-color idea-colors 'edea-lang-character))))
   ;;;;; centaur-tabs
   `(centaur-tabs-default ((,class (:inherit default))))
   `(centaur-tabs-selected ((,class (:inherit centaur-tabs-default :background ,tab-bg-selected))))
   `(centaur-tabs-unselected ((,class (:inherit centaur-tabs-default))))
   `(centaur-tabs-selected-modified ((,class (:inherit centaur-tabs-selected :foreground ,tab-fg-modified))))
   `(centaur-tabs-unselected-modified ((,class (:inherit centaur-tabs-unselected :foreground ,tab-fg-modified))))
   `(centaur-tabs-active-bar-face ((,class (:background ,keyword))))
                                        ;`(centaur-tabs-modified-marker-selected ((,class (:inherit 'centaur-tabs-selected :foreground,keyword))))
                                        ;`(centaur-tabs-modified-marker-unselected ((,class (:inherit 'centaur-tabs-unselected :foreground,keyword))))

;;;;; cider
   `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
   `(cider-enlightened-local ((,class (:foreground ,yellow))))
   `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
   `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
   `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
   `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
   `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
   `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
   `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
   `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
   `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
   `(company-preview-search ((,class (:inherit match))))
   `(company-scrollbar-bg ((,class (:background ,bg2))))
   `(company-scrollbar-fg ((,class (:background ,act2))))
   `(company-template-field ((,class (:inherit region))))
   `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
   `(company-tooltip-annotation ((,class (:foreground ,type))))
   `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,keyword))))
   `(company-tooltip-common-selection ((,class (:foreground ,keyword))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-search ((,class (:inherit match))))
   `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))
   ;;;;; line-numbers
   `(linum ((,class ,(edea-transform-idea-color idea-colors 'edea-general-code-line-number))))
   `(line-number ((,class ,(edea-transform-idea-color idea-colors 'edea-general-code-line-number))))
   `(line-number-current-line ((,class ,(edea-transform-idea-color idea-colors 'edea-general-code-line-number-on-caret-row))))
   ;;;;; mode-line
   `(mode-line           ((,class (:foreground "#000000" :background "#FFFFFF" :box (:color ,border :line-width 1)))))
   `(mode-line-buffer-id ((,class (:inherit mode-line :weight bold))))
   `(mode-line-emphasis ((,class (:inherit mode-line :weight bold))))
   `(mode-line-highlight ((,class (:inherit mode-line :weight bold))))
   `(mode-line-inactive  ((,class (:inherit mode-line :foreground "#888888"))))
   ;;;;; treemacs
   `(treemacs-root-face ((,class (:inherit default :weight bold))))
   `(treemacs-header-button-face ((,class (:foreground ,project-header-button-fg))))
   `(treemacs-fringe-indicator-face ((,class (:foreground ,project-header-button-fg))))
   `(treemacs-git-added-face ((,class (:foreground ,project-git-added-face))))
   `(treemacs-git-conflict-face ((,class (:foreground ,project-git-conflict-face))))
   `(treemacs-git-ignored-face ((,class (:foreground ,project-git-ignored-fg))))
   `(treemacs-git-modified-face ((,class (:foreground ,project-git-modified-fg))))
   `(treemacs-git-untracked-face ((,class (:foreground ,project-git-untracked-fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'edea-light)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; edea-light-theme.el ends here
