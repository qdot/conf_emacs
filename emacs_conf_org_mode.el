;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org mode setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-contacts)
(require 'org-checklist)
(require 'org-screen)
(require 'org-protocol)
(require 'org-mobile)

(setq org-modules (quote (org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-protocol org-w3m)))

					; global STYLE property values for completion
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))

;; Most of this ripped from http://doc.norang.ca/org-mode.html
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq
 ;; Use ~/emacs_org for storing files. Usually symlinked to Dropbox
 org-directory "~/emacs_org"

 ;; By default, at least timestamp done states
 org-log-done t

 ;; Always make sure indentation is on
 org-indent-mode t

 ;; Start indented
 org-startup-indented t

 ;; Hide blank lines inside folded nodes
 org-cycle-separator-lines 0

 ;; Show notes in a task first
 org-reverse-note-order nil

 ;; Just show one day on the agenda
 org-agenda-ndays 1

 ;; Not sure, think I copied it from norang
 org-indent-indentation-per-level 2

 ;; Archive to the file name, assume we're not doubling up names across projects
 org-archive-location "~/emacs_org/archives/%s_archive::"

 ;; Don't really use priorities, turn them off
 org-enable-priority-commands nil

 ;; Do single letter confirm of links
 org-confirm-elisp-link-function 'y-or-n-p

 ;; Use IDO for target completion
 org-completion-use-ido t

 ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
 org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))

 ;; Targets start with the file name - allows creating level 1 tasks
 org-refile-use-outline-path (quote file)

 ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
 org-outline-path-complete-in-steps t

 ;; Yes it's long... but more is better ;
 org-clock-history-length 35

 ;; Resume clocking task on clock-in if the clock is open
 org-clock-in-resume t

 ;; Change task state to STARTED when clocking in
 org-clock-in-switch-to-state "STARTED"

 ;; Save clock data and notes in the LOGBOOK drawer
 org-clock-into-drawer t

 ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
 org-clock-out-remove-zero-time-clocks t

 ;; Don't clock out when moving task to a done state
 org-clock-out-when-done nil

 ;; Save the running clock and all clock history when exiting Emacs, load it on startup
 org-clock-persist t

 ;; Don't use priorities and accidentally set them all the time, so just turn them off.
 org-enable-priority-commands nil

 ;; Don't use super/subscript, makes exports weird.
 org-use-sub-superscripts nil

 ;; The habit graph display column in the agenda
 org-habit-graph-column 50

 ;; warn 15 min in advance
 appt-message-warning-time 15

 ;; warn every 5 minutes
 appt-display-interval 5

 ;; show in the modeline
 appt-display-mode-line t

 ;; use our func
 appt-display-format 'nil

 ;; use speed commands
 org-use-speed-commands t

 ;; I like links being active ret
 org-return-follows-link t

 ;; Make lists cycle whether they're nodes or plain
 org-cycle-include-plain-lists t

 ;; Fontify org-src blocks like their language mode
 org-src-fontify-natively t

 ;; Turn on sticky agendas so we don't have to regenerate them
 org-agenda-sticky t
 )

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable C-c [ and C-c ] in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; Undefine C-c [ and C-c ] since this breaks my
            ;; org-agenda files when directories are include It
            ;; expands the files in the directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined))
          'append)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

;; 3 different stage setups. First two are from norang, final is for
;; events, so I can search for things like "all concerts I attended
;; this year" via state and tags
(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
				(sequence "EVENT(e)" "|" "ATTENDED(a!)" "SKIPPED(k!)"))))

;; I use C-M-r to start org-remember
(global-set-key (kbd "C-M-R") 'org-capture)

;; Once again, stolen from norang, except for the contacts one, which
;; was taken from the org-mode list.
(setq org-capture-templates
      (quote
       (("t" "todo" entry (file "~/emacs_org/tasks.org")
	 "* TODO %?
  %u
  %a")
	("n" "note" entry (file "~/emacs_org/notes.org")
	 "* %?                                        :NOTE:
  %u
  %a")
	("r" "reply" entry (file+headline "~/emacs_org/tasks.org" "Email")
	 "* [[gnus:%:group#%:message-id]]                                        :email:"
	 :immediate-finish t)
	("w" "link" entry (file+headline "~/emacs_org/links.org" "Links")
	 "* %c                                        :link:"
	 :immediate-finish t)
	("s" "snowmew link" entry (file+headline "~/emacs_org/links.org" "Snowmew Links")
	 "* %c                                        :link:"
	 :immediate-finish t))))

;; Personal agenda modes
(setq org-agenda-custom-commands
      (quote (("h" "Tasks for home" tags-todo "+HOME-someday" nil)
              ("5" "Tasks for work" tags-todo "+WORK-someday" nil)
              ("p" "Tasks for personal projects" tags-todo "+PROJECTS-someday" nil)
              ("X" agenda ""
               (;;(org-agenda-prefix-format " [ ] ")
                (org-agenda-with-colors nil)
                (org-agenda-remove-tags t))
               ("~/emacs_org/agenda.txt"))
              ("w" agenda "Week with events and no daily/chores"
               ((org-agenda-ndays-to-span 7)
                (org-agenda-ndays 7)
                (org-agenda-filter-preset '("-daily"))))
	      (" " "Agenda"
               ((agenda "" nil)
                (tags "email"
                      ((org-agenda-overriding-header "Emails")
                       (org-tags-match-list-sublevels nil)
		       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))))
		nil)))))

;; Org mode notifications via aptp
;; the appointment notification facility
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; Show the weather, set in the main task file via:
;;* Weather
;;%%(org-google-weather "XXXXX")
;;
;; (XXXXX being zip code)

(require 'org-google-weather)

;; Embed location maps, with directions from home

(setq calendar-location-name "Home")
(setq calendar-latitude 37.870975)
(setq calendar-longitude -122.288813)

(require 'org-location-google-maps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://kanis.fr/blog-emacs.html#%20Diary%20block%20without%20week%2Dend
;; %%(diary-block-no-week-end 15 9 2010 30 10 2010) block without week-end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qdot/diary-block-no-week-end (m1 d1 y1 m2 d2 y2 &optional mark)
  "Block diary entry.
Entry applies if date is between two dates and not in the
weekend."
  (let ((date1 (calendar-absolute-from-gregorian
                (diary-make-date m1 d1 y1)))
        (date2 (calendar-absolute-from-gregorian
                (diary-make-date m2 d2 y2)))
        (day (calendar-day-of-week date))
        (d (calendar-absolute-from-gregorian date)))
    (and (<= date1 d) (<= d date2) (not (= day 6)) (not (= day 0))
         (cons mark entry))))

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
	"xelatex -interaction nonstopmode %f")) ;; for multiple passes


(defun qdot/reload-org-files ()
  (interactive)
  (setq org-agenda-files
        (append
         (file-expand-wildcards "~/emacs_org/tasks.org")
         (file-expand-wildcards "~/emacs_org/mozilla/*.org")
         (file-expand-wildcards "~/emacs_org/home/*.org")
         (file-expand-wildcards "~/emacs_org/nplabs/*.org")
         (file-expand-wildcards "~/emacs_org/travel/*.org")
         (file-expand-wildcards "~/emacs_org/projects/*.org")
         (file-expand-wildcards "~/emacs_org/personal/*.org"))))

(qdot/reload-org-files)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

;; Turn habits on at 6am every morning
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-ghi-interesting-repos '("qdot/libnifalcon" "qdot/liblightstone" "qdot/libtrancevibe" "qdot/emokit" "qdot/libomron" "qdot/libfitbit"))
(setq org-ghi-org-file "~/emacs_org/github.org")
(setq org-ghi-file-under-repo-headline t)

(setq org-mobile-inbox-for-pull "~/emacs_org/tasks.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/emacs_org/tasks.org" "~/emacs_org/mozilla/B2G.org"))
(setq org-mobile-agendas nil)

(defvar org-agenda-no-resize nil
  "When non-nil, don't let org-mode resize windows for you")

(setq org-agenda-no-resize t)

(defadvice qdot/org-fit-agenda-window (around org-fit-agenda-window-select)
  "Will not let org-fit-agenda-window resize if
org-agenda-no-resize is non-nil"
  (when (not org-agenda-no-resize)
    ad-do-it))
