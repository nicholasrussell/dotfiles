;;;; SmartParens

(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)

(defun parens-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
(sp-pair "{" nil :post-handlers '((parens-create-newline-and-enter-sexp "RET")))

;;;; ParEdit

;(require 'paredit)

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
; with SLIME
;(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
; (defun override-slime-repl-bindings-with-paredit ()
;   (define-key slime-repl-mode-map
;     (read-kbd-macro paredit-backward-delete-key) nil))
;(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
; Electric RETURN
; (defvar electrify-return-match
;   "[\]}\)\"]"
;   "If this regexp matches the text after the cursor, do an \"electric\" return.")
; (defun electrify-return-if-match (arg)
;   "If the text after the cursor matches `electrify-return-match' then
;   open and indent an empty line between the cursor and the text.  Move the
;   cursor to the new line."
;   (interactive "P")
;   (let ((case-fold-search nil))
;     (if (looking-at electrify-return-match)
;         (save-excursion (newline-and-indent)))
;     (newline arg)
;     (indent-according-to-mode)))
;(global-set-key (kbd "RET") 'electrify-return-if-match)
; (defun paredit-delete-indentation (&optional arg)
;   "Handle joining lines that end in a comment."
;   (interactive "*P")
;   (let (comt)
;     (save-excursion
;       (move-beginning-of-line (if arg 1 0))
;       (when (skip-syntax-forward "^<" (point-at-eol))
;         (setq comt (delete-and-extract-region (point) (point-at-eol)))))
;     (delete-indentation arg)
;     (when comt
;       (save-excursion
;         (move-end-of-line 1)
;         (insert " ")
;         (insert comt)))))
;(define-key paredit-mode-map (kbd "M-^") 'paredit-delete-indentation)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(provide 'init-parens)

