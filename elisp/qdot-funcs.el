;; Close all buffers

(defun qdot/close-all-buffers ()
  "Close all currently open buffers"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; Unfill paragraph

(defun qdot/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;; Ask before quitting

(defun qdot/ask-before-quit ()
  "Ask me before I quit emacs if I think that's a good thing to do"
  (interactive)
  (yes-or-no-p "Do you really want to quit Emacs?"))

;; Reload file with current position saved

(defun qdot/reload-file ()
  "Reload a file, resetting the cursor to the current position"
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)))


;; Show just matches instead of everything in occur buffer

(defun qdot/occurrences (regexp &rest ignore)
  "Show all matches for REGEXP in an `occur' buffer."
  ;; keep text covered by occur-prefix and match text-properties
  (interactive (occur-read-primary-args))
  (occur regexp)
  (with-current-buffer (get-buffer "*Occur*")
    (let ((inhibit-read-only t)
          delete-from
          pos)
      (save-excursion
        (while (setq pos (next-property-change (point)))
          (goto-char pos)
          (if (not (or (get-text-property (point) 'occur-prefix)
                       (get-text-property (point) 'occur-match)))
              (if delete-from
                  (delete-region delete-from (point))
                (setq delete-from (point)))
            (when delete-from
              (delete-region delete-from (point))
              (if (get-text-property (point) 'occur-prefix)
                  (insert "\n")
                (insert " ")))
            (setq delete-from nil)))))))


;; Make kill-ring-save/kill-region smarter based on mark position

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; Edit files as root
;; http://nflath.com/2009/08/tramp/


(defun qdot/sudo-edit (&optional arg)
  "Open file as root and edit via tramp"
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(defun qdot/sudo-edit-current-file ()
  "Open file in current buffer as root and edit via tramp"
  (interactive)
  (let ((pos (point)))
    (find-alternate-file
     (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char pos)))


;; Rename a file and the buffer it's in at the same time
;; Via yeggeconf http://sites.google.com/site/steveyegge2/my-dot-emacs-file


(defun qdot/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))


;; Abort minibuffer when mousing
;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html


(defun qdot/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (>= (recursion-depth) 1)
    (abort-recursive-edit)))

;; Simple filter function
;; http://www.emacswiki.org/emacs/ElispCookbook

(defun qdot/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


;; Return major mode of a buffer
;; http://stackoverflow.com/questions/2238418/emacs-lisp-how-to-get-buffer-major-mode

(defun qdot/buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
    (set-buffer buffer-or-string)
    major-mode))


;; Open current buffer file in browser

(defun qdot/open-in-browser()
  "Open current file in browser"
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


;; Evaluate and replace preceding sexp

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;; Delete current buffer and file
;; http://blog.tuxicity.se/elisp/emacs/2010/11/16/delete-file-and-buffer-in-emacs.html

(defun qdot/delete-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


;; Insert a uuid at current position using uuidgen

(defun qdot/insert-uuid ()
  "Use uuidgen to insert a uuid at point"
  (interactive)
  (shell-command-on-region (point) (point) "uuidgen" t)
  (delete-backward-char 1))


;; Clear kill ring

(defun qdot/clear-kill-ring ()
  "Clear the kill ring variable"
  (setq kill-ring nil))


;; Override org-agenda-open-in-other-window

(defun qdot/org-agenda-open-in-other-window()
  "Used for opening org files in the agenda in another window instead of over the agenda"
  (interactive)
  (when (and workgroups-mode
             (eq (wg-get-workgroup "agenda") (wg-current-workgroup)))
    (setq pop-up-windows nil)
    (org-agenda-switch-to)
    (setq pop-up-windows t))
  (org-agenda-switch-to))


;; Byte compile the current buffer on save if a byte compiled version
;; already exists.


(defun qdot/byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

;; Desktop setup function

(defun qdot/start-desktop ()
  (interactive)
  (qdot/set-platform-font)
  (when linux-p
    (wg-find-session-file (concat qdot/emacs-conf-dir "workgroups/linux-wg.el")))
  (org-agenda nil " ")
  (sauron-start)
  (qdot/monkey-patch-sr)
  (qdot/add-notify-hooks)
  (qdot/erc-znc-start)
  (qdot/bitlbee-connect)
  (qdot/personal-wg-setup)
  (wg-switch-to-workgroup "bitlbee")
  (qdot/bitlbee-resume-layout)
  (wg-switch-to-workgroup "twitter"))

(provide 'qdot-funcs)