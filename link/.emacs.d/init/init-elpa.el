;;; Find and load the correct package.el

(require 'package)

;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Fire up package.el
;; To disable automatic package loading, set package-enable-at-startup to nil
; (setq package-enable-at-startup nil)
(package-initialize)


(provide 'init-elpa)

