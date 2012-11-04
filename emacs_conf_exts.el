;; Why is smex-initialize not running with el-get anymore? I dunno.
;; Doesn't hurt to make sure it runs anyways.

(smex-initialize)

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

(color-theme-initialize)
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

;; (setq yas/root-directory '("~/.emacs_files/snippets"
;;                            "~/.emacs_files/elisp_auto/yasnippet/snippets"))
;; (yas/initialize)
;; (mapc 'yas/load-directory yas/root-directory)
;; (yas/global-mode t)



;; auto-complete

(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(ac-linum-workaround)
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

;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; link numbering for twittering mode
(add-hook 'eshell-preoutput-filter-functions
	  'ansi-color-filter-apply)

(require 'twittering-numbering)
(add-hook 'twittering-mode-hook 'twittering-numbering)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 600)
(setq twittering-url-show-status nil)
(add-hook 'twittering-edit-mode-hook (lambda () 
				       (ispell-minor-mode) 
				       (flyspell-mode)))

;; (bbdb-initialize 'gnus 'message)
;; (bbdb-mua-auto-update-init 'gnus 'message)

(require 'org-compat)
(require 'calfw)
;; (require 'calfw-org)

;; (require 'popwin)
;; (setq display-buffer-function 'popwin:display-buffer)

(setq bbdb-phone-style 'nil)

(require 'diminish)

(workgroups-mode t)

(diminish 'workgroups-mode " ω")
(diminish 'auto-complete-mode " α")

