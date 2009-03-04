;;;
;; sr-speedbar.el
;;
;; Athors:  Andy Stewart, Sebastian Rose, Unknown.
;;
;; Copyright (c) 2008 Sebastian Rose, Hannover, Germany
;;               sebastian_rose at gmx de
;;
;; Copyright (c) 2008 Andy Stewart
;;
;; Released under the GNU General Public License v. 2
;;
;; The idea and the starting point for this code was found here:
;;             http://www.emacswiki.org/cgi-bin/wiki/SpeedBar
;;
;; The author of the original code snippet is unknown.
;;
;; Usage:
;; 1.) in .emacs (or where ever your startup lives in):
;;
;;      (require 'sr-speedbar)
;;      (global-set-key [(super ?s)] 'sr-speedbar-toggle)
;;
;; ...or any key binding you like.
;; Now your windows key and 's' show the speedbar in an extra window, same
;; frame. Currently speedbar always shows up on the right. You can customize
;; the initial width of the speedbar window further down for console/DOS and
;; X/Win/MacOS seperatly.
;;
;;
;; ChangeLog:
;;
;; * 13 SEP 2008:
;;   Andy Stewart: Added advices to refuse the removal of the speedbars
;;                 window.
;;   Sebastian: sr-speedbar-delete-windows: set to non-nil to avoid
;;              the removal of other windows.
;;
;; * 26 JUN 2008:
;;   Added Andy Stewart's patch to refresh the speedbar's contents.
;;   Thank's for this one!
;;
;; * Added some lines to get it working:
;;     * splitting the window and remember it,
;;     * changing the way speedbar finds a file.
;;     * File view of speedbar is now working all right.
;;     * C-x 1 in other window deletes speedbar-window, just calling
;;       M-x sr-speedbar-no-separate-frame again is fine now.
;;     * Toggle speedbar works, width is save when toggeling.
;;     * Recalc speedbar width if window-width - speedbar-width <= 0
;;     * Speedbar window is now dedicated to speedbar-buffer.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Customization
;;
(defvar sr-speedbar-width-x        24 "Initial width of sr-speedbar-window under window system.")
(defvar sr-speedbar-width-console  24 "Initial width of sr-speedbar-window on console.")
(defvar sr-speedbar-delete-windows nil
  "Allow the speedbar to delte other windows before showing up.
If nil, speedbar will not touch your window configuration.
Otherwise delete-other-windows will be called before showing
the speedbar.")
;;
;;                         END OF CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'speedbar)

(defconst sr-speedbar-version "0.0.3")

(defvar sr-speedbar-width 24 "intial width of speedbar-window")
(defun sr-speedbar-recalculate ()
  "Calculate the speedbar width with respect of window system"
  (if (and window-system (not (string= "pc" window-system)))
      (setq sr-speedbar-width sr-speedbar-width-x)
    (setq sr-speedbar-width sr-speedbar-width-console)))

(sr-speedbar-recalculate)

(defconst sr-speedbar-buffer-name "*SPEEDBAR*")
(defvar sr-speedbar-window nil "speedbar window")
(defvar speedbar-buffer nil "speedbar buffer")
(defvar sr-speedbar-last-refresh-dictionary nil
  "The last refresh dictionary record of 'sr-speedbar-refresh'.")



(defun sr-sp-before-visiting-file-hook () "" (select-window (previous-window)))
(defun sr-sp-before-visiting-tag-hook () "" (select-window (previous-window)))
(defun sr-sp-visiting-file-hook () "" (select-window (previous-window)))
(defun sr-sp-visiting-tag-hook () "" (select-window (previous-window)))


(defun sr-speedbar-no-separate-frame ()
  (interactive)
  (let ( (win (selected-window)) )
    (if (not (buffer-live-p speedbar-buffer))
        (progn
          (if sr-speedbar-delete-windows
              (delete-other-windows)) ;; ensure only one window is there
          (if (<= (window-width) sr-speedbar-width)
              (sr-speedbar-recalculate))
          (setq sr-speedbar-window (split-window (selected-window) (- (window-width) sr-speedbar-width) t))
          (setq speedbar-buffer (get-buffer-create sr-speedbar-buffer-name)
                speedbar-frame (selected-frame)
                dframe-attached-frame (selected-frame)
                speedbar-select-frame-method 'attached
                speedbar-verbosity-level 0
                speedbar-last-selected-file nil)
          (set-buffer speedbar-buffer)
          (buffer-disable-undo speedbar-buffer) ;make disable in speedbar buffer, otherwise will occur 'undo-outer-limit' error
          (speedbar-mode)
          (speedbar-reconfigure-keymaps)
          (speedbar-update-contents)
          (speedbar-set-timer 1)
          (add-hook 'speedbar-before-visiting-file-hook 'sr-sp-before-visiting-file-hook t)
          (add-hook 'speedbar-before-visiting-tag-hook 'sr-sp-before-visiting-tag-hook t)
          (add-hook 'speedbar-visiting-file-hook 'sr-sp-visiting-file-hook t)
          (add-hook 'speedbar-visiting-tag-hook 'sr-sp-visiting-tag-hook t)
          ; (make-local-hook 'kill-buffer-hook) ; depricated. uncomment for emacs 21
          (add-hook 'kill-buffer-hook
                    (lambda () (when (eq (current-buffer) speedbar-buffer)
                                 (progn
                                   (setq speedbar-frame nil
                                         dframe-attached-frame nil
                                         speedbar-buffer nil)
                                   (remove-hook 'speedbar-before-visiting-file-hook 'sr-sp-before-visiting-file-hook)
                                   (remove-hook 'speedbar-before-visiting-tag-hook 'sr-sp-before-visiting-tag-hook)
                                   (remove-hook 'speedbar-visiting-file-hook 'sr-sp-visiting-file-hook)
                                   (remove-hook 'speedbar-visiting-tag-hook 'sr-sp-visiting-tag-hook))
                                 (speedbar-set-timer nil)))))
      (if (not (window-live-p sr-speedbar-window))
          (progn
            (if sr-speedbar-delete-windows
                (delete-other-windows)) ;; ensure only one window is there
            (setq sr-speedbar-window
                  (split-window (selected-window)
                                (- (window-width) sr-speedbar-width) t)))))
    (set-window-buffer sr-speedbar-window (get-buffer sr-speedbar-buffer-name))
    (set-window-dedicated-p sr-speedbar-window t)
    (select-window win)
    (bury-buffer speedbar-buffer)
    ))


(defun sr-speedbar-toggle ()
  "Toggle visibility of sr-speedbar by resizing the sr-speedbar-window to a minimal width
or the last width when visible. Use this function to create or toggle visibility
of a speedbar-window. It will be created if neccessary."
  (interactive)
  (if (or (not (window-live-p sr-speedbar-window)) (not (buffer-live-p speedbar-buffer)))
      (progn
        (sr-speedbar-no-separate-frame)
        (setq sr-speedbar-width (window-width sr-speedbar-window)))
    (let ( (win (selected-window)) )
      (let ( (w (window-width sr-speedbar-window)) )
      (if (<= w 1)
          (progn
            (select-window sr-speedbar-window)
            (enlarge-window-horizontally (- sr-speedbar-width 1)))
        (select-window sr-speedbar-window)
        (setq sr-speedbar-width (window-width))
        (shrink-window-horizontally (- sr-speedbar-width 1))))
      (bury-buffer speedbar-buffer)
      (if (window-live-p win)
          (select-window win)))))


(defun sr-speedbar-select-window ()
  "Force the windows that contain speedbar."
  (interactive)
  (select-window sr-speedbar-window))


(defun sr-speedbar-refresh ()
  "Refresh the context of speedbar."
  (interactive)
  (let ((name (if (eq major-mode 'dired-mode) ;get current buffer name
                  (dired-get-filename)
                (or (buffer-file-name) "")))
        current-refresh-directory)
    (setq current-refresh-directory (file-name-directory name)) ;get current directory
    (if (and (not (equal current-refresh-directory sr-speedbar-last-refresh-dictionary)) ;if directory is not change
             (not (equal sr-speedbar-buffer-name (buffer-name (window-buffer))))) ;and is not in speedbar buffer
        (progn
          (setq sr-speedbar-last-refresh-dictionary current-refresh-directory)
          (speedbar-refresh)
          ))))

(add-hook 'speedbar-timer-hook          ;automatic update context of speedbar
          (lambda ()
            (sr-speedbar-refresh)))

;;
;; Refuse the removal of the speedbars window.
;;

(defvar sr-speedbar-active nil
  "This is active mark of `sr-speedbar-window'.")

(defadvice delete-other-windows (before sr-speedbar-1 activate)
  "This advice to mark whether `sr-speedbar-window' is active."
  (if (window-live-p sr-speedbar-window)
      (setq sr-speedbar-active t)
    (setq sr-speedbar-active nil)))

(defadvice delete-other-windows (after sr-speedbar-1 activate)
  "This advice to restore `sr-speedbar-window'."
  (if (and sr-speedbar-active
           (not (window-live-p sr-speedbar-window)))
      (sr-speedbar-no-separate-frame)))

(defadvice delete-window (before sr-speedbar-2 activate)
  "This advice to mark whether `sr-speedbar-window' is active."
  (if (window-live-p sr-speedbar-window)
      (setq sr-speedbar-active t)
    (setq sr-speedbar-active nil)))

(defadvice delete-window (after sr-speedbar-2 activate)
  "This advice to restore `sr-speedbar-window'."
  (if (and sr-speedbar-active
           (not (window-live-p sr-speedbar-window)))
      (progn
        (sr-speedbar-no-separate-frame)
        (message "Can't delete current window, please use `sr-speedbar-toggle' to close."))))


(provide 'sr-speedbar)

;; This plain hack probably will not work with versions other than speedbar v 1.0
(defun speedbar-timer-fn ()
  "Run whenever Emacs is idle to update the speedbar item."
  (if (or (not (speedbar-current-frame))
	  (not (frame-live-p (speedbar-current-frame))))
      (speedbar-set-timer nil)
    ;; Save all the match data so that we don't mess up executing fns
    (save-match-data
      ;; Only do stuff if the frame is visible, not an icon, and if
      ;; it is currently flagged to do something.
      (if (and speedbar-update-flag
	       (speedbar-current-frame)
	       (frame-visible-p (speedbar-current-frame))
	       (not (eq (frame-visible-p (speedbar-current-frame)) 'icon)))
	  (let ((af (selected-frame)))
	      (dframe-select-attached-frame speedbar-frame)
	      ;; make sure we at least choose a window to
	      ;; get a good directory from
	      (if (window-minibuffer-p (selected-window))
		  nil
		;; Check for special modes
		(speedbar-maybe-add-localized-support (current-buffer))
		;; Update for special mode all the time!
		(if (and speedbar-mode-specific-contents-flag
			 (consp speedbar-special-mode-expansion-list)
			 (local-variable-p
			  'speedbar-special-mode-expansion-list
			  (current-buffer)))
		    ;;(eq (get major-mode 'mode-class 'special)))
		    (progn
		      (if (<= 2 speedbar-verbosity-level)
			  (speedbar-message
			   "Updating speedbar to special mode: %s..."
			   major-mode))
		      (speedbar-update-special-contents)
		      (if (<= 2 speedbar-verbosity-level)
			  (progn
			    (speedbar-message
			     "Updating speedbar to special mode: %s...done"
			     major-mode)
			    (speedbar-message nil))))

 		  ;; Update all the contents if directories change!
 		  (unless (and (or (member major-mode speedbar-ignored-modes)
				   (string-equal "*SPEEDBAR*" (buffer-name))
				   (not (buffer-file-name)))
			       ;; Always update for GUD.
			       (not (string-equal "GUD"
				     speedbar-initial-expansion-list-name)))
		    (speedbar-update-localized-contents)))
		(select-frame af))
	    ;; Now run stealthy updates of time-consuming items
	    (speedbar-stealthy-updates)))))
  (run-hooks 'speedbar-timer-hook))
