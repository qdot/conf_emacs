;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Small require blocks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up tramp and set default protocol to ssh

(require 'tramp)
(setq tramp-default-method "ssh")

;; converts unix/windows line ending
;;(require 'eol-conversion)

;; use M-y and M-n in y-or-n minibuffer prompts
(require 'quick-yes)

;;asciidoc markup mode
(require 'asciidoc-mode)
(autoload 'asciidoc-mode "asciidoc-mode")
(add-hook 'asciidoc-mode-hook
		  '(lambda ()
			 (turn-on-auto-fill)
			 (require 'asciidoc)))


;; mingus mpd controller
(autoload 'mingus "mingus")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; active-menu, menu collapsing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you turn active-menu on and off frequently, you might want to use
;;
(autoload 'active-menu
  "active-menu"
  "Show menu only when mouse is at the top of the frame."
  t)
