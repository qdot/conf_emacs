;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Small require blocks
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up tramp and set default protocol to ssh

(require 'tramp)
(setq tramp-default-method "ssh")

;; use M-y and M-n in y-or-n minibuffer prompts
(require 'quick-yes)

;;asciidoc markup mode
(require 'asciidoc-mode)
(autoload 'asciidoc-mode "asciidoc-mode")
(add-hook 'asciidoc-mode-hook
          '(lambda ()
             (require 'asciidoc)))
(setq auto-mode-alist
      (append
       '(
         ("\\.testdoc$" . asciidoc-mode)
         ("\\.archdoc$" . asciidoc-mode)
         ("\\.doc$" . asciidoc-mode)         
         ) auto-mode-alist))

;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append
       '(
         ("\\.markdown$" . markdown-mode)
         ) auto-mode-alist))

;; jekyll
(require 'jekyll)

;; icomplete and icomplete+
;; Via http://nflath.com/2009/07/icomplete/

(icomplete-mode 1)
(setq icomplete-compute-delay 0)
(require 'icomplete+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elscreen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Set prefix key to `, just like in gnu screen
(setq elscreen-prefix-key "`")

(require 'elscreen)
(require 'elscreen-dired)
(require 'elscreen-color-theme)
(require 'elscreen-server)
(require 'elscreen-buffer-list)

(setq elscreen-buffer-list-enabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dired modifications
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Supercollider Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p "~/.emacs_files/elisp_local/scel")

  ;;Assume we're on a mac with SuperCollider in the normal spot
  ;; (when (file-exists-p "/Applications/SuperCollider/sclang")
  ;;   (add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/scel/el"))
  ;;   (custom-set-variables
  ;;    '(sclang-program "/Applications/SuperCollider/sclang")
  ;;    )
  ;;   (require 'sclang)
  ;;   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; color-theme Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dark-laptop)     
     ))
(custom-set-variables
 '(global-font-lock-mode t nil (font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-execute-command-cache nil)
(setq ido-create-new-buffer 'always)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; uniquify Mode for buffer name seperation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Twitter mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require 'todochiku)
(require 'twit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame maximizer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame change to work with/like windmove
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame maximizer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'frame-cmds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modeline decorator to get rid of the scrollbar
;; Via http://emacs-fu.blogspot.com/2010/03/showing-buffer-position-in-mode-line.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
    (progn 
      (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
      (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right

(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "auto-complete/"))
  (add-to-list 'load-path (expand-file-name (concat emacs-repo-elisp-submodule-dir "auto-complete/")))
  (require 'auto-complete-config)
  (ac-config-default)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Todochiku notifier
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'todochiku)
(setq 
 todochiku-message-too t
 todochiku-icons-directory (expand-file-name (concat emacs-repo-elisp-dir "todochiku-icons/"))
 todochiku-icons (quote ((alarm . "alarm.png") (mail . "mail.png")))
 )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yasnippets
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path
             (expand-file-name (concat emacs-repo-elisp-src-dir "yasnippet-0.6.1c/")))
(require 'yasnippet)

(defvar snippet-directories nil)

(mapc '(lambda (path)
         (and
          (file-accessible-directory-p path)
          (add-to-list 'snippet-directories
                       (expand-file-name path))))
      (list (expand-file-name (concat emacs-repo-conf-dir "snippets/"))
            (expand-file-name (concat emacs-repo-elisp-src-dir "yasnippet-0.6.1c/snippets"))
                               ))

(setq yas/root-directory
      (list (expand-file-name (expand-file-name (concat emacs-repo-conf-dir "snippets/")))))

(defun yas/load-all-directories ()
  (interactive)
  (yas/reload-all)
  (mapc 'yas/load-directory-1 snippet-directories))

(yas/initialize)
(yas/load-all-directories)














