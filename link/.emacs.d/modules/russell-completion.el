;;; russell-completion.el -*- lexical-binding: t; -*-

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :config
  (setq vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)
    (define-key vertico-map (kbd "M-h") 'vertico-directory-up))
  :init
  (require 'vertico-directory)
  (vertico-mode)
  :custom
  (vertico-cycle t))

;; (use-package vertico-posframe
;;   :init
;;   (vertico-posframe-mode 1))

(use-package marginalia
  :requires (vertico)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
    ([remap apropos] . consult-apropos)
    ([remap bookmark-jump] . consult-bookmark)
    ([remap goto-line] . consult-goto-line)
    ([remap imenu] . consult-imenu)
    ([remap locate] . consult-locate)
    ([remap load-theme] . consult-theme)
    ([remap man] . consult-man)
    ([remap recentf-open-files] . consult-recent-file)
    ([remap switch-to-buffer] . consult-buffer)
    ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ([remap yank-pop] . consult-yank-pop)
  ; :init
  ; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ; (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  ;; Hook into projectile
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describ-key] . helpful-key))

(provide 'russell-completion)

