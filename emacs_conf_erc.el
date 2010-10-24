;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ERC and Extensions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'erc)

(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-match)

;; For bitlbee
(require 'erc-nicklist)

(load-library "erc-highlight-nicknames")
(add-to-list 'erc-modules 'highlight-nicknames)
;; (add-to-list 'erc-modules 'scrolltobottom)
(add-to-list 'erc-modules 'match)
(erc-update-modules)

(erc-match-enable)
(erc-match-mode 1)

(erc-timestamp-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ERC setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "[%H:%M] "
      erc-fill-prefix "      "
      erc-timestamp-mode t
      erc-max-buffer-size 20000
      erc-interpret-mirc-color nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
;; (erc-scrolltobottom-enable)
;;  (erc-scrolltobottom-disable)
;; (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
;; (setq erc-keywords '((".*Online.*" (:foreground "green"))
;;                      (".*Busy" (:foreground "red"))
;;                      (".*Away" (:foreground "red"))
;;                      (".*Idle" (:foreground "orange"))
;;                      ))

(setq erc-keywords nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change fill column on resize
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Query event functionality
;; Todochiku messaging on privmsgs, checks for repeat pages, etc...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Todochiku notifier for priv messages
(defun qdot/text-match-erc-hook (match-type nick msg)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (if (featurep 'todochiku)
      (todochiku-message
       "ERC Mention"
       (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
       (todochiku-icon 'irc)
       )))
(add-hook 'erc-text-matched-hook 'qdot/text-match-erc-hook)

(defvar qdot/erc-nopage-nick-list nil 
  "List of nick regexps that should not be allowed to page.")
(setq qdot/erc-nopage-nick-list '("twitter_qdot"))

(defvar qdot/erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a notification")

(defvar qdot/erc-page-timeout 10
  "Number of seconds that must elapse between notifications from the same 
person.")

(defun qdot/erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`qdot/erc-page-timeout'."

  ;; Check to see if we even want to page about this nick
  ;; We want to make sure the nick isn't in the list, so negate
  (when (not (member nick qdot/erc-nopage-nick-list))
    (unless delay (setq delay qdot/erc-page-timeout))    
    (let ((cur-time (time-to-seconds (current-time)))
          (cur-assoc (assoc nick qdot/erc-page-nick-alist))
          (last-time))
      (if cur-assoc
          (progn
            (setq last-time (cdr cur-assoc))
            (setcdr cur-assoc cur-time)
            (> (abs (- cur-time last-time)) delay))
        (push (cons nick cur-time) qdot/erc-page-nick-alist)
        t))))

(defun qdot/erc-page-popup-notification (nick)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (if (featurep 'todochiku)    
        (todochiku-message
         "ERC Mention"
         (concat "ERC: priv-msg from " nick)
         (todochiku-icon 'irc)))))

;; The hook for figuring out whether or not we should be paged
(defun qdot/erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    ;; If we're the target, and we haven't been paged in a while, page
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (qdot/erc-page-allowed nick))
      (qdot/erc-page-popup-notification nick)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'qdot/erc-page-me-PRIVMSG)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Privmsg window allocation
;;
;; We create a buffer with a ton of windows pointing to the bitlbee-placeholder
;; buffer. These can then be used to throw query windows into as they're
;; created by IMs or IRC privmsgs, so we don't have to worry about ERC screwing
;; with whatever buffer we're in now.
;;
;; Similarly, whenever we kill a query window, we should have it pop back to
;; the placeholder buffer so it can be reused.
;;
;; We also make the assumption that the privmsg allocation frame is the one
;; with the &bitlbee channel buffer in one of its windows. I just usually
;; assume bitlbee is going to be running anyways, so this seemed ok for my
;; setup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Since we have our own allocator, just have ERC bury the buffers and then
;; we'll take care of them ourselves
(setq erc-auto-query 'bury)

(defun qdot/free-query-window-p (window)
  (let ((r nil))
    (if (string= "bitlbee-placeholder" (buffer-name (window-buffer window)))
        (setq r t))
    r))



(defun qdot/erc-move-query-to-placeholder (buffer)
  (if (get-buffer-window "&bitlbee" t)
      (let* ((free-window-list (qdot/filter 'qdot/free-query-window-p (window-list (window-frame (get-buffer-window "&bitlbee" t))))))
        (set-window-buffer (car free-window-list) buffer))))

(defun qdot/erc-privmsg-query-allocate (proc parsed)
  ;; Find the frame holding the bitlbee& buffer. We'll consider that our privmsg window
  ;; Once we find it, walk the windows until we find an open bitlbee-placeholder
  ;; Set the window list to that so we can just pick the first window off the top
  (if (get-buffer-window "&bitlbee" t)
      (let* 
          (
           (nick (car (erc-parse-user (erc-response.sender parsed))))
           (target (car (erc-response.command-args parsed)))
           (msg (erc-response.contents parsed))
           (query  (if (not erc-query-on-unjoined-chan-privmsg)
		       nick
		     (if (erc-current-nick-p target)
			 nick
		       target))))

        ;;If the buffer doesn't even exist yet, go ahead and run auto-query to make it happen
        (if (not (erc-get-buffer query proc))
            (erc-auto-query proc parsed))
        ;;If we find one, allocate into that, otherwise, commense undefined behavior
        (when (and (erc-current-nick-p target)
                   (not (erc-is-message-ctcp-and-not-action-p msg))
                   (not (get-buffer-window (erc-get-buffer query proc) t)))
          (qdot/erc-move-query-to-placeholder (erc-get-buffer query proc)))))
  nil
  )

(add-hook 'erc-server-PRIVMSG-functions 'qdot/erc-privmsg-query-allocate)

;; Once we close a query window, return it to being a query placeholder window

(defun qdot/erc-query-buffer-recycle ()
  (if (and (erc-query-buffer-p (current-buffer)) (get-buffer "bitlbee-placeholder"))
      (set-window-buffer (get-buffer-window (current-buffer)) (get-buffer "bitlbee-placeholder")))
  nil)

(add-hook 'kill-buffer-hook 'qdot/erc-query-buffer-recycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ZNC IRC Bouncer Setup
;;
;; I use the ZNC IRC bouncer to keep IRC connected, kinda like screen, except
;; far more complicated and only useful for one thing. Yay!
;;
;; ZNC divides up networks to be one per account, so we have to start once ERC
;; instance per network we want to connect to.
;;
;; We also need to figure out whether we're at home, or tunneling home, in
;; order to know what server address to use.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq qdot/erc-znc-server-rename-list (list))

(defun qdot/erc-set-tunnel-flag ()
  (setq qdot/erc-znc-tunnel-flag nil)
  (if (not (qdot/filter (lambda (arg) (if (string-match qdot/erc-znc-home-prefix arg) arg nil)) (get-ip-addresses)))      
      (setq qdot/erc-znc-tunnel-flag t)))

(defun qdot/erc-znc-connect (nick)
  (interactive "ZNC Nick:")
  (if qdot/erc-znc-tunnel-flag
      (erc :server qdot/erc-znc-remote-server :port qdot/erc-znc-port :nick nick :full-name nick)
    (erc :server qdot/erc-znc-home-server :port qdot/erc-znc-port :nick nick :full-name nick)))

(defun qdot/erc-znc-rename-server-buffer ()
  (interactive)
  (let ((current-network (caddr (split-string (erc-current-nick) "-"))))
    (save-excursion
      (set-buffer (erc-server-buffer))
      (rename-buffer (concat "znc-" current-network))
      (message (format "Renamed buffer to %s" (concat "znc-" current-network)))
      )))

(defun qdot/erc-znc-initialize (proc parsed)
  ;; Prepend all ZNC buffers with znc-
  (if (and (not (string-match "znc-" (buffer-name (erc-server-buffer)))) (string-match "qdot-znc" (erc-current-nick)))
      (progn
        (qdot/erc-znc-rename-server-buffer)
        (erc-server-send (format "PASS %s:%s" (erc-current-nick) qdot/erc-znc-password))))
  nil)

(add-hook 'erc-server-NOTICE-functions 'qdot/erc-znc-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ZNC variables and utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar qdot/erc-znc-nicks '("qdot-znc-freenode" "qdot-znc-510")) ;;"qdot-znc-electricrain" "qdot-znc-anthrochat"
(defvar qdot/erc-znc-tunnel-flag nil "*Non-nil means use tunnel")
(defvar qdot/erc-znc-password "doesnotmatter")
(defvar qdot/erc-znc-home-server "192.168.123.75")
(defvar qdot/erc-znc-home-prefix "192.168.123.")
(defvar qdot/erc-znc-remote-server "localhost")
(defvar qdot/erc-znc-port 9999)

(defun qdot/erc-znc-start ()
  (interactive)
  (qdot/erc-set-tunnel-flag)
  (mapcar 'qdot/erc-znc-connect qdot/erc-znc-nicks))

(defun qdot/bitlbee-connect ()
  (interactive)
  (qdot/erc-set-tunnel-flag)
  (qdot/erc-znc-connect "qdot-znc-bitlbee"))

(defun qdot/bitlbee-resume-layout ()
  (interactive)
  ;; If we havn't created a placeholder buffer yet, do so now
  (get-buffer-create "bitlbee-placeholder")
  (save-excursion
    ;; Bring up the bitlbee nicklist
    (set-buffer "&bitlbee")
    (erc-nicklist))
  (qdot/resume-layout-file "~/.emacs_files/layouts/bitlbee_layout.el")
  ;; For each already opened query window, reallocate
  (mapc (lambda (buf) (qdot/erc-move-query-to-placeholder buf)) (qdot/filter 'erc-query-buffer-p (buffer-list)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kill all ERC windows and connections for the current frame
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Walk all of the server buffers first
;; Close those first, which autodetaches us from channels
;; Then go back through and close everything
(defun qdot/kill-erc ()
  (interactive)
  (walk-windows 
   (lambda (arg) 
     (if (erc-server-buffer-p (window-buffer arg)) 
         (progn 
           (select-window arg) 
           (erc-quit-server "Wheee.")
           (if (get-buffer-process (window-buffer))
               (delete-process (get-buffer-process (window-buffer))))
           (kill-buffer (window-buffer))
           )))
   nil nil)
  (walk-windows
   (lambda (arg) 
     (if (or (erc-server-buffer-p (window-buffer arg)) (erc-channel-p (window-buffer arg)))
         (kill-buffer (window-buffer arg))))
   nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code left over from when I tried ezbounce 
;; Needs to be integrated back into emacs trunk
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (erc-ezb-initialize)

;; The expected string format for detached sessions has changed
;; Therefore, we need our own session addition function

;; (defun qdot/erc-ezb-add-session (message)
;;   "Add an EZBounce session to the session list."
;;   (when (and erc-ezb-inside-session-listing
;; 	     (string-match "^\\([^ \n]+\\) +\\([^ \n]+\\) +\\([^ \n]+\\) +\\([^ \n]+\\) +\\([^ \n]+\\)$" message))
;;     (let ((id (match-string 1 message))
;; 	  (nick (match-string 2 message))
;; 	  (to   (match-string 3 message)))
;;       (add-to-list 'erc-ezb-session-list (list id nick to)))))

;; (defun qdot/reattach-ezb ()
;;   (when (or (string-match "192.168.123.75" erc-session-server) (string-match "localhost" erc-session-server))
;;     (erc-server-send "REATTACH 1")
;;     )
;; )

;; (defvar qdot/suppress-ezb-reattach-query nil "Don't show the reattach query when connecting to ezb servers")

;; (defun qdot/erc-ezb-select (message)
;;   (if (equal qdot/suppress-ezb-reattach-query nil)      
;;       (erc-ezb-select message)
;;     (qdot/reattach-ezb)
;;     )
;;   )

;; (setq qdot/ezb-action-list
;;       '(("^\\[awaiting login/pass command\\]$"  . erc-ezb-identify)
;;         ("^\\[use /quote CONN <server> to connect\\]$"    . qdot/erc-ezb-select)
;;         ("^# +IRC NICK +TO +TIME +REAL ID$" . erc-ezb-init-session-list)
;;         ("^ $" . erc-ezb-end-of-session-list)
;;         (".*" . qdot/erc-ezb-add-session)
;;         )
;;       )

