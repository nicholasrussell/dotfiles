;;;; Clojure

(require 'clojure-mode)
(require 'cljsbuild-mode)
; (require 'elein)

;; TODO Set up CIDER
(require 'cider)

;; Log communication with nREPL server (usefule for debugging CIDER)
; (setq nrepl-log-messages t)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Use TAB to auto-complete in CIDER buffers (incompatible with (setq tab-always-indent 'complete))
;(defun set-auto-complete-as-completion-at-point-function ()
;  (setq completion-at-point-functions '(auto-complete)))

;(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cljsbuild-mode-hook #'rainbow-delimiters-mode)


(provide 'init-clojure)

