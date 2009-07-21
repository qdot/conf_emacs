;; wanderlust

(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/wanderlust/wl"))
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/wanderlust/elmo"))
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp_local/wanderlust/utils"))
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq
  elmo-maildir-folder-path "~/Maildir"          ;; where i store my mail

  wl-stay-folder-window t                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'

  wl-smtp-connection-type 'starttls
  wl-smtp-posting-server "mail.nonpolynomial.com"            ;; put the smtp server here
  wl-smtp-posting-user "kyle@nonpolynomial.com"
  wl-smtp-posting-port 587
  wl-local-domain "nonpolynomial.com"          ;; put something here...
  wl-message-id-domain "nonpolynomial.com"     ;; ...

  wl-from "Kyle Machulis <kyle@nonpolynomial.com>"                  ;; my From:
  wl-default-folder ".INBOX"
  wl-draft-folder ".[GMail].Drafts"
  wl-trash-folder ".[GMail].Trash"

  wl-folder-check-async t

  elmo-imap4-use-modified-utf7 t
)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
