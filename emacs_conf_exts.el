;; use M-y and M-n in y-or-n minibuffer prompts
(require 'quick-yes)

(icomplete-mode 1)
(setq icomplete-compute-delay 0)

;; Supercollider Mode

(when (file-exists-p "~/.emacs_files/elisp_local/scel")
  (when macosx-p
    ;; Assume we're on a mac with SuperCollider in the normal spot
    (when (file-exists-p "/Applications/SuperCollider/sclang")
      (add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/scel/el"))
      (custom-set-variables
       '(sclang-auto-scroll-post-buffer t)
       '(sclang-eval-line-forward nil)
       '(sclang-help-path (quote ("/Applications/SuperCollider/Help")))
       '(sclang-runtime-directory "~/.sclang/")
       '(sclang-program "/Applications/SuperCollider/sclang"))
      (require 'sclang))))

;; color-theme setup

(color-theme-dark-laptop)     

;; Modeline decorator to get rid of the horizontal scrollbar
;; Via http://emacs-fu.blogspot.com/2010/03/showing-buffer-position-in-mode-line.html

(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
    (progn 
      (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
      (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right

;; Todochiku notifier setup

(setq 
 todochiku-icons-directory (expand-file-name (concat qdot/emacs-elisp-dir "todochiku-icons/"))
 todochiku-icons (quote ((alarm . "alarm.png") (mail . "mail.png") (irc . "irc.png"))))

;; Yasnippets

(defvar yas-directory (expand-file-name (concat emacs-repo-elisp-submodule-dir "yasnippet/")))

(add-to-list 'load-path
             yas-directory)
(setq yas/root-directory '("~/.emacs_files/snippets"
                           "~/.emacs_files/elisp_auto/yasnippet/snippets"))
(yas/initialize)
(mapc 'yas/load-directory yas/root-directory)
(yas/global-mode t)

;; auto-complete

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

;; Workgroups

(workgroups-mode 1)
(setq wg-morph-on nil)
(setq wg-prefix-key "`")
(wg-set-prefix-key)

;; Adoc customization
(setq adoc-insert-replacement nil)


;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

					; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; eshell + autocomplete
;; From http://paste.lisp.org/display/120731

(require 'pcomplete)
(add-to-list 'ac-modes 'eshell-mode)
(ac-define-source pcomplete
  '((candidates . pcomplete-completions)))

(defun nm-eshell-pcomplete ()
  (interactive)
  (let ((ac-sources '(ac-source-pcomplete
		      ac-source-filename)))
    (auto-complete)))

(defun nm-eshell-auto-complete ()
  (interactive)
  (let ((ac-sources '(ac-source-functions
		      ac-source-variables
		      ac-source-features
		      ac-source-symbols
		      ac-source-words-in-same-mode-buffers)))
    (auto-complete)))

(defun nm-eshell-mode-hook ()
  (local-unset-key (kbd "M-?"))

  ;; (local-set-key (kbd "TAB") 'nm-eshell-pcomplete)
  ;; (local-set-key [tab] 'nm-eshell-pcomplete)

  (local-set-key (kbd "TAB") 'nm-eshell-auto-complete)
  (local-set-key [tab] 'nm-eshell-auto-complete))

(add-hook 'eshell-mode-hook 'nm-eshell-mode-hook)