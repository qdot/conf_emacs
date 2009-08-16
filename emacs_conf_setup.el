;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;turn off start message
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-start-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Emacs22 doesn't actually seem to have splash-screen aliased to start-message
;;like it says it does in the docs?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command Frequency Recording Mode
;; Put before autosave
;; http://xahlee.org/emacs/command-frequency.html
;; http://nflath.com/2009/08/command-frequency-mode/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq-default command-frequency-table-file "~/.emacs.d/frequencies")
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar autosave-dir "~/.emacs_meta/autosaves/")

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
		  (if buffer-file-name
			  (concat "#" (file-name-nondirectory buffer-file-name) "#")
			(expand-file-name
			 (concat "#%" (buffer-name) "#")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar backup-dir "~/.emacs_meta/backups")
(make-directory backup-dir t)
(setq backup-directory-alist (list (cons "." backup-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; When custom decides to add things, have it do so in something other than
;; .emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs_files/emacs_conf_custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; show marks for regions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setup common tab width
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 3)
(custom-set-variables
 '(standard-indent 3)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modeline setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display time in the modeline
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(line-number-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(global-font-lock-mode 1)
(show-paren-mode t)

(custom-set-variables
 '(scroll-bar-mode 'right)
 '(indent-tabs-mode nil))


;; copy emacs clipboard to system
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

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
(global-set-key  (kbd "C-x C-b")        'ibuffer-other-window)


;; Transparently open compressed files
(auto-compression-mode t)
;; Save a list of recent files visited.
(recentf-mode 1)
(set-default 'indicate-empty-lines t)
(setq auto-mode-alist
      (append '(("\\.\\(xml\\|mxml\\|html\\|htm\\)$" . nxml-mode)
                ("\\.css$" . css-mode)) auto-mode-alist))

;; always turn on lienum
(global-linum-mode 1)
(setq linum-format "%d ")