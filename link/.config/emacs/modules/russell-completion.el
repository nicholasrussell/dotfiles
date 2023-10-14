;;; russell-completion.el -*- lexical-binding: t; -*-

;;; Vertico
(require 'vertico)
(require 'vertico-directory)
;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)
(keymap-set vertico-map "C-j" #'vertico-next)
(keymap-set vertico-map "C-k" #'vertico-previous)
(keymap-set vertico-map "M-h" #'vertico-directory-up)
(vertico-mode 1)

;;; Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-annotators
                        '(marginalia-annotators-heavy
                          marginalia-annotators-light
                          nil))
(marginalia-mode 1)

;;; Consult
(keymap-global-set "C-s" 'consult-line)
(keymap-global-set "C-S" 'consult-line-multi)
(keymap-set minibuffer-local-map "C-r" 'consult-history)
(keymap-global-set "<remap> <apropos>" #'consult-apropos)
(keymap-global-set "<remap> <bookmark-jump>" #'consult-bookmark)
(keymap-global-set "<remap> <goto-line>" #'consult-goto-line)
(keymap-global-set "<remap> <imenu>" #'consult-imenu)
(keymap-global-set "<remap> <locate>" #'consult-locate)
(keymap-global-set "<remap> <load-theme>" #'consult-theme)
(keymap-global-set "<remap> <man>" #'consult-man)
(keymap-global-set "<remap> <project-switch-to-buffer>" #'consult-project-buffer)
(keymap-global-set "<remap> <recentf-open-files>" #'consult-recent-file)
(keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)
(keymap-global-set "<remap> <switch-to-buffer-other-window>" #'consult-buffer-other-window)
(keymap-global-set "<remap> <switch-to-buffer-other-frame>" #'consult-buffer-other-frame)
(keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
(keymap-global-set "M-s g" #'consult-grep)
(keymap-global-set "M-s G" #'consult-git-grep)
(keymap-global-set "M-s r" #'consult-ripgrep)
(setq completion-in-region-function #'consult-completion-in-region)
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

;;; Orderless
(require 'orderless)
  ;; Set up Orderless for better fuzzy matching
(customize-set-variable 'completion-styles '(orderless basic))
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))

;;; Embark
(require 'embark)
(keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
(keymap-global-set "C-." 'embark-act)
;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)
(require 'embark-consult)
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Corfu
(require 'corfu nil)
(unless (display-graphic-p)
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))
;; Setup corfu for popup like completion
(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto t)
(customize-set-variable 'corfu-auto-prefix 2)
(global-corfu-mode 1)
(require 'corfu-popupinfo)
(corfu-popupinfo-mode 1)
(eldoc-add-command #'corfu-insert)
(keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
(keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
(keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)

;;; Cape
(require 'cape)
;; Setup Cape for better completion-at-point support and more
;; Add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(provide 'russell-completion)

