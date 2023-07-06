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
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (global-unset-key (kbd "C-."))
;;   (setq lsp-keymap-prefix "C-c l")
;;   :bind
;;   (("C-." . lsp-find-definition))
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-java
;;   :hook ((java-mode . lsp-deferred)))

;; (use-package dap-mode
;;   :config
;;   (dap-auto-configure-mode)
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   (dap-gdb-lldb-setup))

;; TODO Update for Emacs 30
;; (use-package dap-java)

;; (use-package lsp-ui
;;   :requires (lsp-mode)
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :init
;;   (setq lsp-headerline-breadcrumb-enable nil))

;; (use-package consult-lsp)

;; (use-package lsp-treemacs
;;   :requires (lsp treemacs))

(use-package eglot
  :commands (eglot lsp-deferred)
  :bind
  (:map eglot-mode-map (([remap xref-find-apropos] . #'consult-eglot-symbols)
                        ("C-." . #'eglot-find-declaration)))
  :init
  (setq eglot-autoshutdown t))

(use-package consult-eglot)

(use-package tree-sitter
  :hook
  ((bash-mode
    c-sharp-mode
    clojure-mode
    java-mode
    js-mode
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
  :hook ((clojure-refactor-mode . clojure-mode)
         (clojure-mode . eglot-ensure)))

(use-package cider
  :custom
  ((cider-repl-display-help-banner nil)
   (cider-repl-pop-to-buffer-on-connect 'display-only)))

(use-package clj-refactor)

(use-package typescript-mode
  :after tree-sitter
  :hook ((js-mode typescript-mode) . eglot-ensure)
  :config
  (define-derived-mode typescript-react-mode typescript-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-react-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-react-mode . tsx))

  (define-derived-mode js-react-mode js-mode "JavaScript JSX")
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-react-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(javascript-react-mode . jsx))

  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(defun russell/rust-cargo-fix ()
  (interactive)
  (rust--compile "%s fix --allow-staged %s" rust-cargo-bin rust-cargo-default-arguments))

(use-package rust-mode
  :mode "\\.rs"
  :hook ((rust-mode . display-line-numbers-mode)
         (rust-mode . eglot-ensure))
  :bind
  (:map rust-mode-map (("C-c C-c C-f" . #'russell/rust-cargo-fix))))
 
(use-package yaml-mode
  :mode "\\.ya?ml")

(use-package yaml-pro
  :hook
  ((yaml-mode) . yaml-pro-ts-mode))

(use-package hcl-mode
  :mode "\\.\\(hcl\\|tf\\)")

(use-package parinfer-rust-mode
    :hook ((emacs-lisp-mode
            clojure-mode
            scheme-mode
            lisp-mode
            racket-mode) . parinfer-rust-mode)
    :init
    (setq parinfer-rust-auto-download t))

(provide 'russell-lang)

