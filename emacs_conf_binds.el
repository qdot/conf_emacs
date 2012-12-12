;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key triggers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c s") 'shell-current-directory)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-s") 'sudo-edit-current-file)
(global-set-key (kbd "C-c C-r") 'reload-file)
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Bind smex over M-x, deals with sorting most used commands to front of IDO
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)

;; Stealin' from esk

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

;; Let's see how long it takes to forget I put this here even though it's just a
;; fix for a HEAD issue.
(defun go-back ()
  (interactive)
  (forward-line -1))

(global-set-key (kbd "C-w") 'backward-kill-word)

(global-set-key (kbd "C-p") 'go-back)
(global-set-key (kbd "<up>") 'go-back)
