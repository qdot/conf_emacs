(when (file-exists-p (concat emacs-repo-elisp-submodule-dir "wanderlust/wl"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "wanderlust/wl"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "wanderlust/elmo"))
  (add-to-list 'load-path (concat emacs-repo-elisp-submodule-dir "wanderlust/utils"))
  (require 'wl)

  ;; Set verification policy, otherwise session errors show up
  (setq ssl-certificate-verification-policy 1)

  ;; Bug fix for character encoding
  (require 'metamail)
  (setq mel-b-ccl-module nil)
  (setq mel-q-ccl-module nil)
  (setq base64-external-encoder '("mimencode"))
  (setq base64-external-decoder '("mimencode" "-u"))
  (setq base64-external-decoder-option-to-specify-file '("-o"))
  (setq quoted-printable-external-encoder '("mimencode" "-q"))
  (setq quoted-printable-external-decoder '("mimencode" "-q" "-u"))
  (setq quoted-printable-external-decoder-option-to-specify-file '("-o"))
  (setq base64-internal-decoding-limit 0)
  (setq base64-internal-encoding-limit 0)
  (setq quoted-printable-internal-decoding-limit 0)
  (setq quoted-printable-internal-encoding-limit 0)

  (setq-default mime-transfer-level 8)
  (setq mime-header-accept-quoted-encoded-words t)

  (require 'filladapt)


  ;; from a WL mailing list post by Per b. Sederber
  ;; Re-fill messages that arrive poorly formatted
  (defun wl-summary-refill-message (all)
    (interactive "P")
    (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
        (progn

          (wl-summary-toggle-disp-msg 'on)
          (save-excursion
            (set-buffer wl-message-buffer)
            (goto-char (point-min))
            (re-search-forward "^$")
            (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
              (forward-line 1))
            (let* ((buffer-read-only nil)
                   (find (lambda (regexp)
                           (save-excursion

                             (if (re-search-forward regexp nil t)
                                 (match-beginning 0)
                               (point-max)))))
                   (start (point))
                   (end (if all
                            (point-max)
                          (min (funcall find "^[^>\n]* wrote:[ \n]+")
                               (funcall find "^>>>>>")
                               (funcall find "^ *>.*\n *>")
                               (funcall find "^-----Original Message-----")))))
              (save-restriction

                (narrow-to-region start end)
                (filladapt-mode 1)
                (fill-region (point-min) (point-max)))))
          (message "Message re-filled"))
      (message "No message to re-fill")))

  (define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)

  (add-hook 'wl-biff-notify-hook
            (lambda()
              (todochiku-message "Wanderlust" "You have new mail!"
                                 (todochiku-icon 'mail))))

  ;; use "Fwd: " not "Forward: "
  (setq wl-forward-subject-prefix "Fwd: ")

  ;; from a WL-mailinglist post by David Bremner
  
  ;; Invert behaviour of with and without argument replies.
  ;; just the author
  (setq wl-draft-reply-without-argument-list
        '(("Reply-To" ("Reply-To") nil nil)
          ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
          ("From" ("From") nil nil)))
  
  
  ;; bombard the world
  (setq wl-draft-reply-with-argument-list
        '(("Followup-To" nil nil ("Followup-To"))
          ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
          ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
          ("From" ("From") ("To" "Cc") ("Newsgroups"))))
  )

