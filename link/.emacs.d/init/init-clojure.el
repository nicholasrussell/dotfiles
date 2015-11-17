;;;; Clojure

(require 'clojure-mode)
(require 'cljsbuild-mode)
; (require 'elein)

;; TODO Set up CIDER
(require 'cider)

;; Log communication with nREPL server (usefule for debugging CIDER)
; (setq nrepl-log-messages t)

; Enable eldoc
(add-hook 'cider-mode-hook #'eldoc-mode)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)


;; Use TAB to auto-complete in CIDER buffers (incompatible with (setq tab-always-indent 'complete))
;(defun set-auto-complete-as-completion-at-point-function ()
;  (setq completion-at-point-functions '(auto-complete)))
;(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cljsbuild-mode-hook #'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'subword-mode)
(require 'clojure-mode-extra-font-locking)


(provide 'init-clojure)

