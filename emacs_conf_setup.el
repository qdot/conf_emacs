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

;; Platform fonts and meta keys

;; We know we have consolas on OS X, so use it
;; We also need to do this as near the beginning as possible, since it crashes
;; otherwise?
(when (and macosx-p
  (when (member "Consolas" (font-family-list))
    (set-face-font 'default "consolas-11"))))
(when mswindows-p
  (set-face-font 'default "consolas-8"))
(when linux-p
  (when (member "Inconsolata" (font-family-list))
    (set-face-font 'default "inconsolata-11")))

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

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

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
 interprogram-paste-function 'x-cut-buffer-or-selection-value)
(set-default 'indicate-empty-lines t)
(display-time)
(line-number-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(global-font-lock-mode 1)
(show-paren-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; If we're in linux, assume we have chrome
(if linux-p
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "google-chrome"))

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
               ("work projects" (filename . "/build/"))
               ("home projects" (filename . "/git-projects/"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

;; Make sure we're always using our buffer groups
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(add-to-list 'ibuffer-formats '(mark modified read-only git-status-mini " "
				     (name 18 18 :left :elide)
				     " "
				     (size 9 -1 :right)
				     " "
				     (mode 16 16 :left :elide)
				     " "
				     (git-status 8 8 :left)
				     " " filename-and-process))

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
