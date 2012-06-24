(load-file "emacs_org_only.el")

(defun qdot/weekly-agenda-kyle ()
	(org-batch-agenda " " org-agenda-ndays 7 org-agenda-include-diary nil))

(qdot/weekly-agenda-kyle)
