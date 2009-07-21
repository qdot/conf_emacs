;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; kill all open buffers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;; Takes a multi-line paragraph and makes it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set window to full screen using meta-return
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
					   nil
					 'fullboth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Don't quit without asking first
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ask-before-quit ()
  "Ask me before I quit emacs if I think that's a good thing to do"
  (interactive)
  (yes-or-no-p "Do you really want to quit Emacs?")
  )
(add-hook 'kill-emacs-query-functions 'ask-before-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get IP Address
;; http://emacs-fu.blogspot.com/2009/05/getting-your-ip-address.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reload current file with position saved
;; http://www.thekidder.net/2008/10/21/emacs-reload-file/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

(global-set-key "\C-c\C-r" 'reload-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Swap buffers in two windows
;; http://www.emacswiki.org/emacs/TransposeWindows
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq swapping-buffer nil)
(setq swapping-window nil)

(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))

(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)