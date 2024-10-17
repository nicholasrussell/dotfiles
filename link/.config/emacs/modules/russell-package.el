;;; russell-package.el -*- lexical-binding: t; -*-

;;; Packages to try:
;; tree-edit, evil-tree-edit
;; ibuffer-project
;; tabspaces
;; multi-vterm
;; emacs-everywhere
;; revisit solaire-mode
;; revisit vertico-posframe

;; Defaults
(add-to-list 'package-selected-packages 'savehist)
(add-to-list 'package-selected-packages 'undo-tree)
(add-to-list 'package-selected-packages 'emacs-everywhere)

;; EVIL
(add-to-list 'package-selected-packages 'evil)
(add-to-list 'package-selected-packages 'evil-collection)
(add-to-list 'package-selected-packages 'evil-nerd-commenter)

;; Completion
(add-to-list 'package-selected-packages 'cape)
(add-to-list 'package-selected-packages 'consult)
(add-to-list 'package-selected-packages 'corfu)
(add-to-list 'package-selected-packages 'corfu-terminal)
(add-to-list 'package-selected-packages 'embark)
(add-to-list 'package-selected-packages 'embark-consult)
(add-to-list 'package-selected-packages 'marginalia)
(add-to-list 'package-selected-packages 'orderless)
(add-to-list 'package-selected-packages 'vertico)

;; Term
(add-to-list 'package-selected-packages 'exec-path-from-shell)
(add-to-list 'package-selected-packages 'vterm)

;; VCS
(add-to-list 'package-selected-packages 'magit)

;; UI
(add-to-list 'package-selected-packages 'all-the-icons)
(add-to-list 'package-selected-packages 'all-the-icons-completion)
(add-to-list 'package-selected-packages 'all-the-icons-dired)
(add-to-list 'package-selected-packages 'nerd-icons)
(add-to-list 'package-selected-packages 'diredfl)
(add-to-list 'package-selected-packages 'doom-modeline)
(add-to-list 'package-selected-packages 'emojify)
(add-to-list 'package-selected-packages 'helpful)
(add-to-list 'package-selected-packages 'modus-themes)
(add-to-list 'package-selected-packages 'pulsar)
(add-to-list 'package-selected-packages 'rainbow-delimiters)
(add-to-list 'package-selected-packages 'treemacs)
(add-to-list 'package-selected-packages 'treemacs-magit)

;; Lang
(add-to-list 'package-selected-packages 'org-appear)

(add-to-list 'package-selected-packages 'cider)
(add-to-list 'package-selected-packages 'clj-refactor)
(add-to-list 'package-selected-packages 'clojure-mode)
(add-to-list 'package-selected-packages 'consult-eglot)
(add-to-list 'package-selected-packages 'editorconfig)
(add-to-list 'package-selected-packages 'flycheck-clojure)
(add-to-list 'package-selected-packages 'geiser)
(add-to-list 'package-selected-packages 'geiser-guile)
(add-to-list 'package-selected-packages 'hcl-mode)
(add-to-list 'package-selected-packages 'parinfer-rust-mode)
(add-to-list 'package-selected-packages 'rust-mode)
(add-to-list 'package-selected-packages 'treesit-auto)
(add-to-list 'package-selected-packages 'typescript-mode)
(add-to-list 'package-selected-packages 'yaml-pro)

(provide 'russell-package)

