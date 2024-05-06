;;; russell-ui.el -*- lexical-binding: t; -*-

;;; Custom
;; Disable customize by redirecting writing to /dev/null
(setq-default custom-file null-device)

;; Modeline
; First run: M-x nerd-icons-install-fonts
(require 'doom-modeline)
(doom-modeline-mode 1)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M:%S")
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
; (display-time-mode 1)

;; (defun display-time-bottom-right ()
;;   (and (equal (cddr (window-pixel-edges))
;;               (cddr (window-pixel-edges (frame-root-window))))
;;        '(#(" " 0 1 (display (space :align-to (- right 20))))
;;          display-time-string)))
;; (display-time-mode)
;; (setq global-mode-string '(:eval (display-time-bottom-right)))

;(require 'mini-modeline)
;(mini-modeline-mode t)

;; Line Numbers
(column-number-mode)
(setq-default display-line-numbers-width 2)
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Paren highlight
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(setq blink-matching-paren nil)
(add-hook 'prog-mode-hook #'show-paren-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Highlight line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'conf-mode-hook #'hl-line-mode)

;; Pulse line for cursor point focus
(require 'pulsar)
(setq pulsar-delay 0.025)
(setq pulsar-iterations 10)
(pulsar-global-mode 1)

;; all-the-icons
; First run: M-x all-the-icons-install-fonts
(when (display-graphic-p)
  (require 'all-the-icons)
  (require 'all-the-icons-completion)
  (require 'all-the-icons-dired)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;;; Theme
(setq custom-theme-directory (expand-file-name "themes" russell/emacs-home))
(require 'modus-themes)
(load-theme 'modus-operandi 'no-confirm)
(custom-theme-set-faces 'modus-operandi '(hl-line ((t (:box (:line-width (-1 . -1) :color "#e0e0e0" :style nil) :background "#ffffff")))))

;; Fonts
(defun russell/set-fonts (&optional frame)
  (interactive)
  (let* ((attrs (frame-monitor-attributes frame))
         (geo (alist-get 'geometry attrs))
         (mm-size (alist-get 'mm-size attrs))
         (px-x (caddr geo))
         (cm-x (/ (car mm-size) 10.0))
         (ppcm (if (> cm-x 0) (/ px-x cm-x) 0)))
    (set-face-attribute 'default frame :font "Source Code Pro" :height (if (< ppcm 40) 130 180) :weight 'normal :width 'normal)))
(if (daemonp)
  (add-hook 'server-after-make-frame-hook
        (lambda ()
            (setq doom-modeline-icon t)
            (russell/set-fonts (selected-frame))))
  (russell/set-fonts (selected-frame)))

;; Minibuffer
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.1)
(setq use-short-answers t)
;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Dired
; Keep dired to a single buffer. TODO integrate this automatically
(require 'dired-single)
; extra dired font lock rules
(when (display-graphic-p)
  (require 'diredfl)
  (add-hook 'dired-mode-hook #'diredfl-mode))

;; Emoji
; due to a bug in emojify, set this var first
(setq emojify-display-styles 'unicode)
(require 'emojify)
; (add-hook 'after-init-hook #'global-emojify-mode)

;; Opacity
(defun russell/alpha (n)
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha-background n))

;; Better help pages
(require 'helpful)
(keymap-set helpful-mode-map "<remap> <revert-buffer>" #'helpful-update)
(keymap-global-set "<remap> <describe-command>" #'helpful-command)
(keymap-global-set "<remap> <describe-function>" #'helpful-callable)
(keymap-global-set "<remap> <describe-key>" #'helpful-key)
(keymap-global-set "<remap> <describe-symbol>" #'helpful-symbol)
(keymap-global-set "<remap> <describe-variable>" #'helpful-variable)
(keymap-global-set "C-h F" #'helpful-function)
(keymap-global-set "C-h K" #'describe-keymap)

;; File tree
; https://github.com/Alexander-Miller/treemacs/issues/1017#issuecomment-1515602288
(add-to-list 'image-types 'svg)
(require 'treemacs)
(setq treemacs-follow-after-init t
      treemacs-is-never-other-window t
      treemacs-sorting 'alphabetic-case-insensitive-asc)
(require 'treemacs-magit)

;; Winner
;;; Enable window configuration history (C-c <left> undo, C-c <right> redo)
(winner-mode 1)

(provide 'russell-ui)

