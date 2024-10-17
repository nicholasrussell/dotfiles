;;; russell-lang.el -*- lexical-binding: t; -*-

;;; Org
;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)
;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)
;; Visually indent org-mode files to a given header level
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'org-appear-mode)
;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(require 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

;;; Eglot
(customize-set-variable 'eglot-autoshutdown t)
(require 'consult-eglot)
(add-hook 'eglot-mode (lambda () (keymap-local-set "<remap> <xref-find-apropos>" #'consult-eglot-symbols)))

;;; Tree Sitter
;(require 'treesit-auto)
;(setq treesit-auto-install t)
;(global-treesit-auto-mode)
;(treesit-auto-install-all)
;(treesit-auto-add-to-auto-mode-alist)

;;; Languages
;; Clojure
(require 'clojure-mode)
(with-eval-after-load "clojure-mode"
  (require 'cider)
  (customize-set-variable 'cider-repl-display-help-banner nil)
  (customize-set-variable 'cider-repl-pop-to-buffer-on-connect 'display-only)
  (require 'clj-refactor)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c r")))
  (with-eval-after-load "flycheck"
    (require 'flycheck-clojure)
    (flycheck-clojure-setup)))

;; Scheme
(customize-set-variable 'scheme-program-name "guile")

;; JavaScript / TypeScript
(require 'typescript-mode)
(define-derived-mode typescript-react-mode typescript-mode "TypeScript TSX")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-react-mode))
(define-derived-mode js-react-mode js-mode "JavaScript JSX")
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-react-mode))
(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)
;(require 'dap-node)
;(dap-node-setup)

;; Rust
(defun russell/rust-cargo-fix ()
  (interactive)
  (rust--compile "%s fix --allow-staged %s" rust-cargo-bin rust-cargo-default-arguments))
(require 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda ()
            (keymap-local-set "C-c C-c C-f" #'russell/rust-cargo-fix)))

;; sh
(customize-set-variable 'flymake-shellcheck-allow-external-files t)
(add-hook 'bash-ts-mode-hook 'flymake-shellcheck-load)
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)
(add-hook 'bash-ts-mode-hook #'flymake-mode)
(add-hook 'sh-mode-hook #'flymake-mode)

;; YAML
(require 'yaml-pro)
(add-hook 'yaml-mode #'yaml-pro-ts-mode)

;; HCL
(require 'hcl-mode)

;;; Editor Config
(require 'editorconfig)
(add-hook 'prog-mode-hook #'editorconfig-mode)

;;; Parinfer
; use parinfer for lisps
(require 'parinfer-rust-mode)
(setq parinfer-rust-auto-download t)
(defvar russell/parinfer-modes
  '(clojure-mode
    emacs-lisp-mode
    lisp-mode
    racket-mode
    scheme-mode))
(dolist (mode russell/parinfer-modes)
  (let ((hook-name (format "%s-hook" (symbol-name mode))))
    (add-hook (intern hook-name) #'parinfer-rust-mode)))

;;; Register eglot modes
(defvar russell/eglot-mode-list
  '(;clojure-mode ; prefer CIDER
    js-mode
    js-ts-mode
    rust-mode
    rust-ts-mode
    typescript-mode
    typescript-ts-mode))
(dolist (mode russell/eglot-mode-list)
  (let ((hook-name (format "%s-hook" (symbol-name mode))))
    (add-hook (intern hook-name) #'eglot-ensure)))

(provide 'russell-lang)

