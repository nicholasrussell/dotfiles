;;; russell-evil.el -*- lexical-binding: t; -*-

; must be set before loading evil
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-want-C-h-delete t)
(customize-set-variable 'evil-undo-system 'undo-tree)

(require 'evil)
(defvar evil-mode-buffers '()) ; https://github.com/emacs-evil/evil/issues/1983
(evil-mode 1)

(require 'evil-nerd-commenter)

; vim-like search
(evil-select-search-module 'evil-search-module 'evil-search)
; enable evil nerd commenter
(evilnc-default-hotkeys)
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
