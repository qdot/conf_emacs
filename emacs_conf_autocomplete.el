;; Autocomplete setup
(define-key ac-complete-mode-map [tab] 'ac-expand)
(ac-flyspell-workaround)
(add-to-list 'ac-modes 'org-mode)
(ac-config-default)

