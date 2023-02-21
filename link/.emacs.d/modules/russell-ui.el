;;; russell-ui.el -*- lexical-binding: t; -*-

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package mini-modeline
   :after doom-modeline)
;;   :config
;;   (mini-modeline-mode t))

;; Line Numbers
(column-number-mode)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Paren highlight
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(add-hook 'prog-mode-hook #'show-paren-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq blink-matching-paren nil)

;; Highlight line
(use-package hl-line
  :hook
  ((prog-mode
    text-mode
    conf-mode) . hl-line-mode)
  :custom-face
  ; TODO move to theme
  (hl-line ((t (:box (:line-width (-1 . -1) :color "#e0e0e0" :style nil) :background "#ffffff")))))

;; all-the-icons
; First run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-completion
  :if (display-graphic-p))

;;; Theme
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
;; Disable customize by redirecting writing to /dev/null
(setq-default custom-file null-device)

(use-package modus-themes
  :init
  (load-theme 'modus-operandi 'no-confirm))

;; Fonts
(defun russell/set-fonts ()
  (set-face-attribute 'default nil :font "Source Code Pro" :height 130 :weight 'normal :width 'normal))
(if (daemonp)
  (add-hook 'server-after-make-frame-hook
        (lambda ()
            (setq doom-modeline-icon t)
            (russell/set-fonts)))
  (russell/set-fonts))

;; Visual undo
; (use-package vundo
;  :bind
;  (("C-x C-u" . vundo)))

(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  :custom
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000)) ; 128mb (default is 24mb)

;; Minibuffer
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.1)
(setq use-short-answers t)
;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Dired
(use-package dired
  :ensure nil
  :commands dired-jump
  :custom ((dired-listing-switches "-alghov --group-directories-first --time-style=long-iso"))
  :config
  (when russell/mac-os-p
    (setq insert-directory-program "gls" dired-use-ls-dired t)))
(use-package dired-single)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Emoji
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :custom ((emojify-emoji-styles '(unicode))))

(provide 'russell-ui)

