;;; russell-lang.el -*- lexical-binding: t; -*-

;; Org
(use-package org
  :config
  (setq org-ellipsis " »" org-hide-emphasis-markers t))
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
  :bind
  (("C-." . lsp-find-definition))
  :config
  (lsp-enable-which-key-integration t))

(use-package dap-mode)

;; (use-package company
;;   :requires (lsp-mode)
;;   :hook (lsp-mode . company-mode)
;;   :bind
;;   (:map company-active-map
;; 	("<tab>" . company-complete-selection))
;;   (:map lsp-mode-map
;; 	("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :requires (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp)

(use-package lsp-treemacs
  :requires (lsp treemacs))

;; (use-package eglot
;;   :commands (eglot eglot-ensure)
;;   :bind
;;   (:map eglot-mode-map (([remap xref-find-apropos] . #'consult-eglot-symbols)))
;;   :init
;;   (setq eglot-sync-connect 1
;;         eglot-connect-timeout 10
;;         eglot-autoshutdown t
;;         eglot-send-changes-idle-time 0.5
;;         eglot-auto-display-help-buffer nil))

;; (use-package consult-eglot)

(use-package tree-sitter
  :hook
  ((bash-mode
    c-sharp-mode
    clojure-mode
    java-mode
    javascript-mode
    jsdoc-mode
    json-mode
    markdown-mode
    python-mode
    rust-mode
    typescript-mode
    yaml-mode) . tree-sitter-mode))

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

;; Languages

(use-package clojure-mode
  :mode "\\.clj"
  :hook ((clojure-mode . lsp-deferred)
         (clojure-refactor-mode . clojure-mode)))

(use-package cider
  :custom
  ((cider-repl-display-help-banner nil)
   (cider-repl-pop-to-buffer-on-connect 'display-only)))

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

(provide 'russell-lang)

