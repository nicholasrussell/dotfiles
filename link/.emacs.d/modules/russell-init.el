;;; russell-init.el -*- lexical-binding: t; -*-

;; UTF-8
(set-language-environment "UTF-8")
; set-language-environment sets default-input-method too, so unset it
(setq default-input-method nil)

;; Up GC limit
(setq gc-cons-threshold (* 50 1024 1024))

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

;; Additional ELisp functions
(eval-when-compile (require 'subr-x))

;; Environment constants
(defconst russell/mac-os-p (eq system-type 'darwin))
(defconst russell/linux-p (eq system-type 'gnu/linux))
(defconst russell/bsd-p (or russell/mac-os-p (eq system-type 'berkeley-unix)))
(defconst russell/windows-p (memq system-type '(cygwin windows-nt ms-dos)))

(provide 'russell-init)


