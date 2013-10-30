;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notifications relay file
;;
;; This file is responsible for creating and deleting files on the filesystem
;; for events like privmsg's on irc (which also means ims), new mail, etc...)
;;
;; Existence of these files can then be used by shell scripts to post
;; notifications without having to worry about stupid shit like dbus.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when linux-p
  (defvar qdot/status-file-directory "/home/qdot/.xmonad/status/"
    "Directory to store status files in. Existence of status denotes that xmobar
should highlight that status.")

  (when (not (file-exists-p qdot/status-file-directory))
    (make-directory qdot/status-file-directory))

  (setq qdot/status-types '("bitlbee" "freenode" "twitter" "mozilla" "mail"))

  (defun qdot/add-notify-type (origin prio msg &optional props)
    (cond
     ((string-match "privmsg" msg)
      (with-temp-file (concat qdot/status-file-directory "bitlbee")))
     ((string-match "freenode" msg)
      (with-temp-file (concat qdot/status-file-directory "freenode")))
     ((string-match "mozilla" msg)
      (with-temp-file (concat qdot/status-file-directory "mozilla"))))
    ;; ((string-match "twit" (symbol-name origin))
    ;;  (with-temp-file (concat qdot/status-file-directory "twitter"))))
    nil)

  (defun qdot/remove-notify-type (type)
    (let ((status-file (concat qdot/status-file-directory type)))
      (when (and (file-exists-p status-file) (member type qdot/status-types))
        (delete-file status-file))))

  (defun qdot/add-notify-hooks ()
    (add-hook 'wg-switch-to-workgroup-hook
              (lambda ()
                (cond
                 ((eq (wg-get-workgroup "irc") (wg-current-workgroup))
                  (qdot/remove-notify-type "freenode"))
                 ((eq (wg-get-workgroup "workirc") (wg-current-workgroup))
                  (qdot/remove-notify-type "mozilla"))
                 ((eq (wg-get-workgroup "bitlbee") (wg-current-workgroup))
                  (qdot/remove-notify-type "bitlbee"))
                 ((eq (wg-get-workgroup "twitter") (wg-current-workgroup))
                  (qdot/remove-notify-type "twitter")))))

    (add-hook 'sauron-event-added-functions
              'qdot/add-notify-type)))

(provide 'qdot-sauron-notifications)
