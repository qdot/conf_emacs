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
;; (require 'adoc-mode)
;; (setq auto-mode-alist
;;       (append
;;        '(
;;          ("\\.testdoc$" . adoc-mode)
;;          ("\\.archdoc$" . adoc-mode)
;;          ("\\.asciidoc$" . adoc-mode)
;;          ) auto-mode-alist))

;; jekyll
;; (require 'jekyll)

(icomplete-mode 1)
(setq icomplete-compute-delay 0)

(setq revive:configuration-file (concat user-emacs-directory "revive.el")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffer switching - ibuffer is much better than the regular ol' buffer list
;; http://nflath.com/2009/07/ibuffer-dired-for-buffers/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)
(setq ibuffer-show-empty-filter-groups nil)

;; Set up buffer groups based on file and mode types
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" (mode . org-mode))
               ("ERC" (mode . erc-mode))
               ("Emacs Setup" (or
                               (filename . "/.emacs_files/")
                               (filename . "/.emacs_d/")
                               (filename . "/emacs_d/")))
               ("magit" (name . "magit"))
               ("dired" (mode . dired-mode))
               ("work projects" (filename . "/build/"))
               ("home projects" (filename . "/git-projects/"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

;; Make sure we're always using our buffer groups
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ANSI comint support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Japan! (APEL/FLIM/SEMI for Wanderlust, elscreen, and other things)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you don't require mailcap first, symbols get redefined weird and
;; it'll miss some stuff and it's hard to debug. Comment it out and
;; see. No, really.
(require 'mailcap)

(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "apel/"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "apel/"))
  )

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
(defun qdot/dired-init ()
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
    (qdot/dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'qdot/dired-init))

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
  ;; Assume we're on a mac with SuperCollider in the normal spot
  (when (file-exists-p "/Applications/SuperCollider/sclang")
    (add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/scel/el"))
    (custom-set-variables
     '(sclang-auto-scroll-post-buffer t)
     '(sclang-eval-line-forward nil)
     '(sclang-help-path (quote ("/Applications/SuperCollider/Help")))
     '(sclang-runtime-directory "~/.sclang/")
     '(sclang-program "/Applications/SuperCollider/sclang")
     )
    (require 'sclang)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; color-theme Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(color-theme-dark-laptop)     

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
;; Frame change to work with/like windmove
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Frame maximizer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'window-setup-hook 'maximize-frame t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modeline decorator to get rid of the horizontal scrollbar
;; Via http://emacs-fu.blogspot.com/2010/03/showing-buffer-position-in-mode-line.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
    (progn 
      (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
      (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Todochiku notifier
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq 
 todochiku-icons-directory (expand-file-name (concat emacs-repo-elisp-dir "todochiku-icons/"))
 todochiku-icons (quote ((alarm . "alarm.png") (mail . "mail.png") (irc . "irc.png")))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yasnippets
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar yas-directory (expand-file-name (concat emacs-repo-elisp-submodule-dir "yasnippet/")))

(add-to-list 'load-path
             yas-directory)
(setq yas/root-directory '("~/.emacs_files/snippets"
                           "~/.emacs_files/elisp_auto/yasnippet/snippets"))
(yas/initialize)
(mapc 'yas/load-directory yas/root-directory)
(yas/global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make sure I always come back to the same place in a file
;; http://groups.google.com/group/comp.emacs/browse_thread/thread/c5e4c18b77a18512
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default save-place t) 
(require 'saveplace) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name (concat emacs-repo-elisp-submodule-dir "pymacs")))

;; (when (file-exists-p (concat emacs-repo-elisp-submodule-dir "auto-complete/"))
(add-to-list 'load-path (expand-file-name (concat emacs-repo-elisp-submodule-dir "auto-complete/")))
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-dwim t)
(set-default 'ac-sources '(ac-source-yasnippet ac-source-semantic))
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; workgroups
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (file-exists-p (concat emacs-repo-elisp-submodule-dir "workgroups/"))
;;   (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "workgroups/"))
;;   (require 'workgroups-mode)
;;   (make-directory (concat emacs-repo-elisp-submodule-dir "workgroups") t)
;;   (if macosx-p
;;       (setq workgroups-default-file (concat emacs-repo-conf-dir "workgroups/workgroups-osx.el"))
;;     )
;;   (if linux-p
;;       (setq workgroups-default-file (concat emacs-repo-conf-dir "workgroups/workgroups-linux.el"))
;;     )

;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HURF DURF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jerkcity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Super simple generic mode for nsis editing
;; God forbid I have to do that often
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'nsis-generic-mode
  nil ;;'(";")
  '("Section" "SectionEnd" "Function" "FunctionEnd" "Call" "Goto")
  '(("!\\([A-Za-z]+\\)" (1 'font-lock-builtin-face))
    ("$[({]?\\([A-Za-z0-9_]+\\)[)}]?" (1 'font-lock-variable-name-face))
    )
  (list "\\.\\(nsi\\|nsh\\)$")    
  nil
  "Generic mode for nsis files.")

