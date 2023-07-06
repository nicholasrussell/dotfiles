;;; russell-editor.el -*- lexical-binding: t; -*-

;; Fix Path
;; Doesn't really belong in editor, should move to init, but would need to do package first
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(setq uniquify-buffer-name-style 'forward)
(delete-selection-mode +1)
; (setq-default cursor-type 'bar)
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(setq-default indent-tabs-mode nil)

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; (use-package tree-edit)

;; (use-package evil-tree-edit)

(provide 'russell-editor)

