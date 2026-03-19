;;; russell-lang.el -*- lexical-binding: t; -*-

;;; Org
;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)
;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)
;; Visually indent org-mode files to a given header level
(add-hook 'org-mode-hook #'org-indent-mode)
;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(require 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

;;; Eglot
(customize-set-variable 'eglot-autoshutdown t)
(require 'consult-eglot)
(add-hook 'eglot-mode (lambda () (keymap-local-set "<remap> <xref-find-apropos>" #'consult-eglot-symbols)))
(setq eglot-code-action-indications '(eldoc-hint)) ; 'margin is default but causes weird shifting issues when left-margin is 0 due to the lightbulb emoji size
;; (setq eglot-code-action-indicator (nerd-icons-codicon "nf-cod-lightbulb"))
;; (add-hook 'eglot-managed-mode-hook
;;           (lambda ()
;;             (setq-local left-margin-width 2)
;;             (set-window-buffer (selected-window) (current-buffer))))

;;; Tree Sitter
(require 'treesit-auto)
(treesit-auto-add-to-auto-mode-alist)
;(setq treesit-auto-install t)
;(treesit-auto-install-all)

;;; dap-mode
; (setq dap-auto-configure-features '(sessions locals controls tooltip breakpoints expressions))
; (dap-mode 1)

;;; Languages
;; Clojure
(require 'clojure-mode)
(require 'clojure-ts-mode)
(require 'cider)
(customize-set-variable 'cider-repl-display-help-banner nil)
(customize-set-variable 'cider-repl-pop-to-buffer-on-connect 'display-only)
(require 'clj-refactor)
(defun russell/clojure-setup ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r"))
(add-hook 'clojure-mode-hook #'russell/clojure-setup)
(add-hook 'clojure-ts-mode-hook #'cider-mode)
(add-hook 'clojure-ts-mode-hook #'russell/clojure-setup)

;; Scheme
(customize-set-variable 'scheme-program-name "guile")

;; Java
; (require 'lsp-java)
(add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
(add-to-list
 'eglot-server-programs
 `((java-mode java-ts-mode) .
   ("jdtls" "--java-executable" ,(expand-file-name (concat (or (getenv "JAVA_HOME") "~/.jenv/versions/25") "/bin/java"))
            "-data" ,(expand-file-name (concat (or (getenv "XDG_CACHE_HOME") "~/.cache") "/emacs/jdtls-workspace")))))
              ;:initializationOptions
              ;(:bundles
              ; [,(expand-file-name "~/.opt/jdtls/extras/com.microsoft.java.debug.plugin.jar")
; (require 'dap-java)

;; JavaScript / TypeScript
(require 'typescript-mode)
(define-derived-mode typescript-react-mode typescript-mode "TypeScript TSX")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-react-mode))
(define-derived-mode js-react-mode js-mode "JavaScript JSX")
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-react-mode))
(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)
; (require 'dap-firefox)
; (dap-firefox-setup)
; (require 'dap-node)
; (dap-node-setup)

;; Rust
(defun russell/rust-cargo-fix ()
  (interactive)
  (rust--compile "%s fix --allow-staged %s" rust-cargo-bin rust-cargo-default-arguments))
(require 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda ()
            (keymap-local-set "C-c C-c C-f" #'russell/rust-cargo-fix)))

;; Native Debugging
;(require 'dap-gdb)

;; sh
(add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
(customize-set-variable 'flymake-shellcheck-allow-external-files t)
(add-hook 'bash-ts-mode-hook #'flymake-mode)
(add-hook 'sh-mode-hook #'flymake-mode)

;; YAML
(require 'yaml-pro)
(add-hook 'yaml-mode-hook #'yaml-pro-ts-mode)

;; HCL
(require 'hcl-mode)

;;; Editor Config
(require 'editorconfig)
(editorconfig-mode 1)

;;; Parinfer
; use parinfer for lisps
(require 'parinfer-rust-mode)
(setq parinfer-rust-auto-download t)
(defvar russell/parinfer-modes
  '(clojure-mode
    clojure-ts-mode
    emacs-lisp-mode
    lisp-mode
    racket-mode
    scheme-mode))
(dolist (mode russell/parinfer-modes)
  (let ((hook-name (format "%s-hook" (symbol-name mode))))
    (add-hook (intern hook-name) #'parinfer-rust-mode)))

;;; Register eglot modes
(defvar russell/eglot-mode-list
  '(bash-ts-mode
    c-mode
    clojure-mode
    clojure-ts-mode
    java-mode
    java-ts-mode
    js-mode
    js-ts-mode
    rust-mode
    rust-ts-mode
    sh-mode
    typescript-mode
    typescript-ts-mode))
(dolist (mode russell/eglot-mode-list)
  (let ((hook-name (format "%s-hook" (symbol-name mode))))
    (add-hook (intern hook-name) #'eglot-ensure)))

(provide 'russell-lang)

