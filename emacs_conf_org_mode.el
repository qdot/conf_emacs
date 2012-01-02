;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org mode setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-contacts)
(require 'org-checklist)
(require 'org-screen)

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

 ;; I think just showing the last star looks cleaner
 org-hide-leading-stars t

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
 appt-display-format 'nil) 

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
      (quote (("t" "todo" entry (file "~/emacs_org/tasks.org") 
	       "* TODO %?
  %u
  %a") 
	      ("n" "note" entry (file "~/emacs_org/notes.org") 
	       "* %?                                        :NOTE:
  %u
  %a")
	      ("c" "contact" entry (file "~/emacs_org/contacts.org")
	       "* %^{Name}
:PROPERTIES:
:EMAIL: %^{Email}
:BIRTHDAY: %^{Birthday}
:NICKNAME: %^{IRC or AIM nick}
:PHONE_HOME: %^{Phone (home)}
:PHONE_WORK: %^{Phone (work)}
:PHONE_MOBILE: %^{Phone (cell)}
:SKYPE: %^{Skype-Name}
:URL: %^{Web}
:COMPANY: %^{Company Name}
:POSITION: %^{Position}
:COUNTRY: %^{Country}
:STREET: %^{Street Address}
:CITY: %^{City}
:POSTCODE: %^{Postcode}
:AIM: %^{AIM}
:TWITTER: %^{Twitter}
:END:"
	       ))))

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
                (org-agenda-filter-preset '("-daily")))))))

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


(defun reload-org-files ()
  (interactive)
  (setq org-agenda-files 
        (append
         (file-expand-wildcards "~/emacs_org/tasks.org")
         ;; (file-expand-wildcards "~/emacs_org/work/*.org")
         (file-expand-wildcards "~/emacs_org/home/*.org")
         (file-expand-wildcards "~/emacs_org/nplabs/*.org")
         (file-expand-wildcards "~/emacs_org/travel/*.org")
         (file-expand-wildcards "~/emacs_org/projects/*.org")
         (file-expand-wildcards "~/emacs_org/personal/*.org"))))

(reload-org-files)

