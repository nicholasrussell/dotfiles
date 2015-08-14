;; Prefer newer files when loading
(setq load-prefer-newer t)

;; Paren mode
(show-paren-mode 1)
(require 'rainbow-delimiters)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Turn off line wrap (not recommended to do globally)
; (set-default 'truncate-lines t)

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Global newline and indent
;(define-key global-map (kbd "RET") 'newline-and-indent)

;; Insert newline at end of files
;(setq require-final-newline t)

;; Delete trailing whitespace on save
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Other keybinds
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Apropos settings
(setq apropos-do-all t)

;; Cut and paste
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)

;; Hide magit setup instructions
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Configure Gradle files to run in groovy-mode
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;; Use 4 spaces for groovy and java
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))
(add-hook 'groovy-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))
;; And rainbow delims
(add-hook 'java-mode-hook #'raindow-delimiters-mode)
(add-hook 'groovy-mode-hook #'rainbow-delimiters-mode)

;; Nicer scrolling with mousewhell / trackpad
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Hide menu bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Hide tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Turn off scroll bar mode
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Turn on line numbers globally
(global-linum-mode 1)

;; Show column numbers
(setq column-number-mode t)

;; Suppress GUI features
;(setq use-file-dialog nil)
;(setq use-dialog-box nil)

;; Hide splash screen
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Visible bell
(setq visible-bell t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

;; Non-zero line spacing can mess up ansi-term and co
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Set term program to use
(setq explicit-shell-file-name "/bin/bash")

(when (window-system)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Nyan
(require 'nyan-mode)
;(setq-default nyan-wavy-trail t)
(nyan-mode)
; (nyan-start-animation)


(provide 'init-editing)

