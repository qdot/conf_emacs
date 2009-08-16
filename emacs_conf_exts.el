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
			 (require 'asciidoc)))


;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; jekyll
(require 'jekyll)

;; mingus mpd controller
(require 'mingus)

;; mediawiki editor
(require 'mediawiki)


;; icomplete and icomplete+
;; Via http://nflath.com/2009/07/icomplete/

(icomplete-mode 1)
(setq icomplete-compute-delay 0)
(require 'icomplete+)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Additions to dired
;; http://nflath.com/2009/07/dired/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired-x)
(require 'wdired)
(setq wdired-allow-to-change-permissions 'advanced)
(define-key dired-mode-map	      	      (kbd "r")		'wdired-change-to-wdired-mode)

;;Updated file system on all buffer switches if in dired mode
(defadvice switch-to-buffer-other-window (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice switch-to-buffer (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice display-buffer (after auto-refresh-dired (buffer &optional not-this-window frame)  activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))
(defadvice other-window (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))