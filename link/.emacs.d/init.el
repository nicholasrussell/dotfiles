;;; init.el -*- lexical-binding: t; -*-

;;; Util
;; Up GC limit
(setq gc-cons-threshold (* 50 1024 1024))

(defun russell/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
		    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'russell/display-startup-time)

; (add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(defconst mac-os-p (eq system-type 'darwin))

(setq inhibit-startup-message t)

(setq ring-bell-function #'ignore
      visible-bell nil)

(setq frame-title-format '("%b")
      icon-title-format frame-title-format)
;; Don't resize the frames in steps
(setq frame-resize-pixelwise t)
;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode 0))

(setq uniquify-buffer-name-style 'forward)

;; Packages
(require 'package)

(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package quelpa
  :config
  (setq quelpa-upgrade-interval 7)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-checkout-melpa-p nil)
  :hook (after-init-hook . quelpa-upgrade-all-maybe))

;;; Completion
;; Leader key
(global-unset-key (kbd "C-SPC"))
(use-package general
  :config
  (general-define-key "<escape>" 'keyboard-escape-quit)
  (general-create-definer russell/global-leader
    :prefix "C-SPC")
  (general-create-definer russell/toggle-leader
    :prefix "C-SPC t"
    :which-key "toggles")
  (russell/toggle-leader
    "p" '(treemacs :which-key "treemacs")
    "t" '(consult-theme :which-key "choose theme")))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :config
  (setq vertico-cycle t)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :init (vertico-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
    ([remap apropos] . consult-apropos)
    ([remap bookmark-jump] . consult-bookmark)
    ([remap evil-show-marks] . consult-mark)
    ([remap evil-show-jumps] . +vertico/jump-list)
    ([remap evil-show-registers] . consult-register)
    ([remap goto-line] . consult-goto-line)
    ([remap imenu] . consult-imenu)
    ([remap locate] . consult-locate)
    ([remap load-theme] . consult-theme)
    ([remap man] . consult-man)
    ([remap recentf-open-files] . consult-recent-file)
    ([remap switch-to-buffer] . consult-buffer)
    ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ([remap yank-pop] . consult-yank-pop)
    ([remap persp-switch-to-buffer] . +vertico/switch-workspace-buffer)
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  ;; Hook into projectile
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :requires (vertico)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describ-key] . helpful-key))

;; Term
;(use-package vterm
;  :config
;  (setq vterm-max-scrollback 10000))

;; Projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (let ((project-dirs '("~/dev" "~/sandbox"))
	search-path '())
    (dolist (project-dir project-dirs search-path)
      (when (file-directory-p project-dir)
	(setq search-path (cons project-dir search-path))))
    (setq projectile-project-search-path search-path))
  (setq projectile-switch-project-action #'projectile-dired))

(quelpa '(consult-projectile :fetcher git :url "https://gitlab.com/OlMon/consult-projectile.git"))
(russell/global-leader
  "p" '(projectile-command-map :which-key "consult-projectile"))

;; VCS
(use-package magit)
;(use-package forge)

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package treemacs
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc))
(use-package treemacs-projectile
  :requires (treemacs projectile))
(use-package treemacs-magit
  :requires (treemacs magit))

;; Line Numbers
(column-number-mode)
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; TODO hl-line

;; all-the-icons
; First run: M-x all-the-icons-install-fonts
(use-package all-the-icons)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(add-hook 'prog-mode-hook #'show-paren-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq blink-matching-paren nil)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

;; Minibuffer
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.1)
(advice-add #'yes-or-no-p :override #'y-or-n-p)
;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Theme
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
;; Disable customize by redirecting writing to /dev/null
(setq-default custom-file null-device)

(use-package modus-themes
  :init (load-theme 'modus-operandi 'no-confirm))

;; Fonts
(defun russell/set-fonts ()
  (set-face-attribute 'default nil :font "Source Code Pro" :height 130 :weight 'normal :width 'normal))
(if (daemonp)
  (add-hook 'server-after-make-frame-hook
	    (lambda ()
	      (setq doom-modeline-icon t)
	      (russell/set-fonts)))
  (russell/set-fonts))

;; Org
(use-package org
  :config
  (setq org-ellipsis " »"
	org-hide-emphasis-markers t))
(use-package org-bullets
  :requires (org)
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Language Server
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package dap-mode)

(use-package company
  :requires (lsp-mode)
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :requires (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :requires (lsp treemacs))

;; Languages

(use-package clojure-mode
  :mode "\\.clj"
  :hook ((clojure-mode . lsp-deferred)
	 (clojure-refactor-mode . clojure-mode)))

(use-package cider)

(use-package clj-refactor)

;(use-package typescript-mode
;  :mode "\\.ts\\'"
;  :hook (typescript-mode . lsp-deferred)
;  :config
;  (setq typescript-indent-level 2)
;  (require 'dap-node)
;  (dap-node-setup))

(use-package parinfer-rust-mode
    :hook ((emacs-lisp-mode
            clojure-mode
            scheme-mode
            lisp-mode
            racket-mode) . parinfer-rust-mode)
    :init
    (setq parinfer-rust-auto-download t))

;; Dired
(use-package dired
  :ensure nil
  :commands dired-jump
  :custom ((dired-listing-switches "-alghov --group-directories-first --time-style=long-iso"))
  :config
  (when mac-os-p
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

