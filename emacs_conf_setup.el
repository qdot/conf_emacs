;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs variable setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off start message

(setq inhibit-start-message t)
(setq inhibit-splash-screen t)

;; Bells? Who needs them

(setq visible-bell nil) 
(setq ring-bell-function 'ignore)

(setq message-log-max 5000)

;; Platform fonts and meta keys

;; We know we have consolas on OS X, so use it
;; We also need to do this as near the beginning as possible, since it crashes
;; otherwise?
(defun qdot/set-platform-font ()
  "Set the default font for the system type."
  (interactive)
  (when (and macosx-p
	     (when (member "Consolas" (font-family-list))
	       (set-face-font 'default "consolas-11"))))
  (when mswindows-p
    (set-face-font 'default "consolas-8"))
  (when linux-p
    (when (member "Inconsolata" (font-family-list))
      (set-face-font 'default "inconsolata-11"))))

(qdot/set-platform-font)

(when macosx-p
  ;;Change meta to alt    
  (setq mac-command-modifier 'meta)
  ;;avoid hiding with M-h
  (setq mac-pass-command-to-system nil))

(when linux-p
  (setq x-alt-keysym 'meta))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system

(defvar autosave-dir (concat user-emacs-directory "autosaves/"))

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)

(defvar backup-dir (concat user-emacs-directory "backups/"))
(make-directory backup-dir t)
(setq backup-directory-alist (list (cons "." backup-dir)))

;; show marks for regions

(setq transient-mark-mode t)

;; setup common tab width

(setq tab-width 3)
(setq standard-indent 3)

;; Modeline and display setup

(setq
 display-time-24hr-format t
 display-time-day-and-date t
 indent-tabs-mode nil
 ;; copy emacs clipboard to system
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 ;; http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/
 redisplay-dont-pause t
 ;; period single space ends sentence
 sentence-end-double-space nil
 default-directory "~"
 message-log-max 1024
 )
(set-default 'indicate-empty-lines t)
(display-time)
(line-number-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-font-lock-mode 1)
(global-auto-revert-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; If we're in linux, assume we have chrome
(defun qdot/set-firefox-trunk ()
  (interactive)
  (if linux-p
      (custom-set-variables '(browse-url-firefox-program "firefox-trunk"))))

(defun qdot/set-firefox ()
  (interactive)
  (if linux-p
      (custom-set-variables '(browse-url-firefox-program "firefox"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup for packages included with emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido Mode

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-execute-command-cache nil)
(setq ido-create-new-buffer 'always)

;; uniquify Mode for buffer name seperation

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Window movement

(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Make sure I always come back to the same place in a file
;; http://groups.google.com/group/comp.emacs/browse_thread/thread/c5e4c18b77a18512

(setq-default save-place t)
(require 'saveplace)

;; Set up tramp and set default protocol to ssh

(require 'tramp)
(setq tramp-default-method "ssh")

;; Buffer switching - ibuffer is much better than the regular ol' buffer list

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
               ("work projects" (filename . "/mozbuild/"))
               ("home projects" (filename . "/git-projects/"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

;; Make sure we're always using our buffer groups
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; ANSI comint support

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Needs to be required before apel, since apel also has a mailcap
;; that is incompatible
(require 'mailcap)

;; dired modifications

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

;; Additions to dired
;; http://nflath.com/2009/07/dired/

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

(if macosx-p
    (custom-set-variables
     '(magit-git-executable "/usr/local/git/bin/git")))

;; If something updates under us and we haven't changed the buffer
;; ourselves, reload without asking. Handy for git.
(setq global-auto-revert-mode t)

;; Turn on narrowing
(put 'narrow-to-region 'disabled nil)

;; Turn on easy-pg
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Twittering additions
(add-hook 'twittering-mode-hook (lambda () (visual-line-mode 1)))


;; offline imap setup

(setq sauron-separate-frame nil)

(require 'memory-usage)

;; byte compile the current buffer on save if a byte compiled version already
;; exists.

(defun qdot/byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'qdot/byte-compile-current-buffer)

