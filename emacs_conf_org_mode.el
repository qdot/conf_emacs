;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org mode setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load in the orgmode code directly from the submodule directory
(setq load-path (cons "~/.emacs_files/elisp_local/org_mode/lisp" load-path))
(setq load-path (cons "~/.emacs_files/elisp_local/org_mode/contrib/lisp" load-path))
(require 'org-install)

;; Most of this ripped from http://doc.norang.ca/org-mode.html
(setq auto-mode-alist
      (append
       '(("\\.org$"  . org-mode)
	 ) auto-mode-alist))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)

(setq org-indent-mode t)
;; (setq org-hide-leading-stars t)

(defun reload-org-files ()
  (interactive)
  (setq org-agenda-files 
        (append
         (file-expand-wildcards "~/emacs_org/*.org")
         (file-expand-wildcards "~/emacs_org/work/*.org")
         (file-expand-wildcards "~/emacs_org/home/*.org")
         (file-expand-wildcards "~/emacs_org/projects/*.org")
         )
        ) 
  )

(reload-org-files)

;;
;;;  Load Org Remember Stuff
(require 'remember)
(org-remember-insinuate)

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)"))))

;; I use C-M-r to start org-remember
(global-set-key (kbd "C-M-r") 'org-remember)

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates (quote (("todo" ?t "* TODO %?
  %u
  %a" "~/emacs_org/tasks.org" bottom nil)
                                     ("note" ?n "* %?                                        :NOTE:
  %u
  %a" "~/emacs_org/notes.org" bottom nil)
				     )))



;; Use IDO for target completion
(setq org-completion-use-ido t)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;;
;; Resume clocking tasks when emacs is restarted
(setq org-clock-persistence-insinuate)
;;
;; Yes it's long... but more is better ;)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Don't clock out when moving task to a done state
(setq org-clock-out-when-done nil)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Personal agenda modes
(setq org-agenda-custom-commands
      (quote (("h" "Tasks for home" tags-todo "HOME" nil)
              ("w" "Tasks for work" tags-todo "WORK" nil)
              ("p" "Tasks for personal projects" tags-todo "PROJECTS" nil)
              )))

;; org mobile setup, for when it comes out
;; (setq org-mobile-directory "~/emacs_org/")
;; (add-hook 'org-mobile-post-push-hook
;;           (lambda ()
;;             (shell-command "scp ~/emacs_org/* user@nonpolynomial.com:~/mobile/")))
;; (add-hook 'org-mobile-pre-pull-hook
;;           (lambda ()
;;             (shell-command "scp user@webdavhost:mobile/mobileorg.org ~/stage/ ")))
;; (add-hook 'org-mobile-post-pull-hook
;;           (lambda ()
;;             (shell-command "scp ~/stage/mobileorg.org user@webdavhost:mobile/")))
