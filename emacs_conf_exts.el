;; use M-y and M-n in y-or-n minibuffer prompts
(require 'quick-yes)

(icomplete-mode 1)
(setq icomplete-compute-delay 0)

(setq revive:configuration-file (concat user-emacs-directory "revive.el")) 

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

;; Maximize frames on creation

(add-hook 'window-setup-hook 'maximize-frame t)

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
 todochiku-icons-directory (expand-file-name (concat emacs-repo-elisp-dir "todochiku-icons/"))
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

;; HURF DURF

(require 'jerkcity)

