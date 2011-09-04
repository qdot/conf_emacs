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
(setq auto-mode-alist
      (append
       '(("\\.org$"  . org-mode)
	 ) auto-mode-alist))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-indent-mode t)
(setq org-hide-leading-stars t)
(setq org-agenda-ndays 1)
(setq org-indent-indentation-per-level 2)
(setq org-archive-location "~/emacs_org/archives/%s_archive::")
(setq org-enable-priority-commands nil)
(setq org-confirm-elisp-link-function nil)

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
         )
        )  
  )




(reload-org-files)

(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
				(sequence "EVENT(e)" "|" "ATTENDED(a!)" "SKIPPED(k!)"))))

;; I use C-M-r to start org-remember
(global-set-key (kbd "C-M-r") 'org-capture)

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
	       )
	      )))

;; Use IDO for target completion
(setq org-completion-use-ido t)
;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)
;; Resume clocking tasks when emacs is restarted
(setq org-clock-persistence-insinuate)
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

;; Don't use priorities and accidentally set them all the time, so just turn them off.
(setq org-enable-priority-commands nil)

;; Don't use super/subscript, makes exports weird.
(setq org-use-sub-superscripts nil)

(setq org-habit-graph-column 50)
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
              )))

(setq org-directory "~/emacs_org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/emacs_org/mobile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Org mode notifications via aptp
;; the appointment notification facility
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-interval 5      ;; warn every 5 minutes
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'nil) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(require 'org-google-weather)

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

(setq calendar-location-name "Home")
(setq calendar-latitude 37.870975)
(setq calendar-longitude -122.288813)


(require 'org-location-google-maps)

(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

