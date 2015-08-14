;;;; Themes

; (require 'color-theme-sanityinc-tomorrow)
; (require 'github-theme)
; (require 'monokai-theme)
; (require 'solarized-theme)
; (require 'tangotango-theme)
; (require 'zenburn-theme)

;; Theme path
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; Some Themes
; github
; leuven
; monokai
; sanityinc-tomorrow-day
; sanityinc-tomorrow-night
; sanityinc-tomorrow-blue
; sanityinc-tomorrow-bright
; sanityinc-tomorrow-eighties
; solarized-light
; solarized-dark
; tangotango
; wombat

;;;; Buffer themes
;(require 'load-theme-buffer-local)

;; Java
;(add-hook 'java-mode-hook
;  (lambda () (load-theme-buffer-local 'leuven (current-buffer))))

;; JavaScript
;(add-hook 'js-mode-hook
;  (lambda () (load-theme-buffer-local 'monokai (current-buffer))))

;;;; Hack theme work around
;; Java
;(add-hook 'java-mode-hook
;  (lambda () (load-theme 'leuven (current-buffer))))

;; JavaScript
;(add-hook 'js-mode-hook
;  (lambda () (load-theme 'monokai (current-buffer))))

;; Toggle between light and dark
;(defun theme-light ()
;  "Activate a light color theme."
;  (load-theme 'leuven t))

;(defun theme-dark ()
;  "Activate a dark color theme."
;  (load-theme 'solarized-dark t))

;(theme-dark)

; (load-theme 'sanityinc-tomorrow-bright t)


(provide 'init-themes)

