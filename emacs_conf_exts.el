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

;; one dired buffer, damnit
;; taken from http://bitbucket.org/kcfelix/emacsd/src/tip/init.el
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
        loaded."
  (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames-regexp)
  (define-key dired-mode-map (kbd "C-M-s") 'dired-isearch-filenames)
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map "v" 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

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