;;;; Org

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;(defun sacha/org-html-checkbox (checkbox)
;  "Format CHECKBOX into HTML."
;  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
;	(off "<span class=\"checkbox\">&#x2610;</span>")
;	(trans "<code>[-]</code>")
;	(t "")))
;(defadvice org-html-checkbox (around sacha activate)
;  (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))

(provide 'init-org)

