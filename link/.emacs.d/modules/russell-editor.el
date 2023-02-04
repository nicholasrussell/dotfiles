;;; russell-editor.el -*- lexical-binding: t; -*-

(setq uniquify-buffer-name-style 'forward)
(delete-selection-mode +1)
; (setq-default cursor-type 'bar)
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(setq-default indent-tabs-mode nil)

;; Evil
;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

(provide 'russell-editor)

