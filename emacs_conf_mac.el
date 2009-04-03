;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the meta on mac to actually /be/ meta
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(set-default-font
;    "-apple-Consolas-normal-r-normal-normal-10-97-96-96-c-*-iso8859-1")
(setq mac-command-modifier 'meta)
;avoid hiding with M-h
(setq mac-pass-command-to-system nil)
;;(if (eq 'mac-osxp 0)
;;     (if  (eq 'aquamacs nil)
;; 		  (set-default-font
;; 			"-apple-Consolas-medium-normal-normal-Regular-11-*-*-*-*-*-iso10646-1"
;; 			)
;; 		)
;; 	 )

(if (featurep 'carbon-emacs-package)
   (set-default-font
     "-apple-consolas-medium-r-normal--11-0-72-72-m-0-iso10646-1")
)
