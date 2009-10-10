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
(setq org-agenda-files 
      (list "~/emacs_org/notes.org"
            "~/emacs_org/tasks.org"
            "~/emacs_org/work/work.org"
            "~/emacs_org/work/geoviewer.org"
            "~/emacs_org/work/geotools.org"
            "~/emacs_org/work/build_system.org"
            "~/emacs_org/work/licensing.org"
            "~/emacs_org/work/libdss.org"
            "~/emacs_org/work/camera.org"
            "~/emacs_org/work/sigma_filter.org"
            "~/emacs_org/work/interviews.org"
            "~/emacs_org/home/home.org"
            "~/emacs_org/home/computer.org"
            "~/emacs_org/home/conferences.org"
            "~/emacs_org/home/liblightstone.org"
            "~/emacs_org/home/libnifalcon.org"
            "~/emacs_org/home/libomron.org"
            "~/emacs_org/home/libtrancevibe.org"
            "~/emacs_org/home/someday.org"
            "~/emacs_org/home/vienna.org"
            "~/emacs_org/home/neurosky.org"
            )
      )

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


					; Use IDO for target completion
(setq org-completion-use-ido t)

					; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

					; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

					; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
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
