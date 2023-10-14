;;; russell-evil.el -*- lexical-binding: t; -*-

; must be set before loading evil
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-want-C-h-delete t)
(customize-set-variable 'evil-undo-system 'undo-redo)

(require 'evil)
(evil-mode 1)

; vim-like search
(evil-select-search-module 'evil-search-module 'evil-search)
; turn on evil nerd commenter
(evil-select-search-module 'evil-search-module 'evil-search)
; Make C-g revert to normal state
(keymap-set evil-insert-state-map "C-g" 'evil-normal-state)
; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(keymap-global-set "C-M-u" 'universal-argument)
; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

; Make sure some modes start in Emacs state
(dolist (mode '(custom-mode
                eshell-mode
                term-mode
                vterm-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(require 'evil-collection)
(evil-collection-init)

(provide 'russell-evil)
