;;; init.el -*- lexical-binding: t; -*-

;; Allow specifying a root Emacs home for different installations
(when (not (boundp 'russell/emacs-home))
  (defvar russell/emacs-home nil))
(when (null russell/emacs-home)
  (setq russell/emacs-home user-emacs-directory))

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer t)

;; UTF-8
; see: https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/lisp/doom-start.el#L134
(set-language-environment "UTF-8")
; set-language-environment sets default-input-method too, so unset it
(setq default-input-method nil)

;; Environment constants
(defconst russell/env-mac-os-p (eq system-type 'darwin))
(defconst russell/env-linux-p (eq system-type 'gnu/linux))
(defconst russell/env-bsd-p (or russell/env-mac-os-p (eq system-type 'berkeley-unix)))
(defconst russell/env-windows-p (memq system-type '(cygwin windows-nt ms-dos)))
(defconst russell/env-terminal-p (not window-system))

;; Display startup time
(defun russell/display-startup-time ()
  (message "Emacs loaded in %s seconds with %d garbage collections."
           (emacs-init-time "%.2f seconds")
           gcs-done))
(add-hook 'emacs-startup-hook #'russell/display-startup-time)

;; Early UI Customizations
;; Do these here to prevent flickering
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq frame-title-format '("%b")
      icon-title-format frame-title-format)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(setq use-dialog-box nil)

;; Directory to place additional Emacs config
(add-to-list 'load-path (expand-file-name "modules" russell/emacs-home))

(require 'russell-package)

;; Install all the configured packages before configuring them
(package-install-selected-packages :noconfirm)

(require 'russell-defaults)
(require 'russell-evil)
(require 'russell-completion)
(require 'russell-term)
(require 'russell-vcs)
(require 'russell-ui)
(require 'russell-lang)

(provide 'init)

