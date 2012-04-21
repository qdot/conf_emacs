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
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-c C-s") 'sudo-edit-current-file)
(global-set-key (kbd "C-c C-r") 'reload-file)
(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "C-x C-p") 'previous-multiframe-window)
(global-set-key (kbd "C-x C-n") 'next-multiframe-window)

;; Bind smex over M-x, deals with sorting most used commands to front of IDO
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)
