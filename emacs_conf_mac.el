;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the meta on mac to actually /be/ meta
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if macosx-p
    ;;Change meta to alt
    (setq mac-command-modifier 'meta)
  ;;avoid hiding with M-h
  (setq mac-pass-command-to-system nil))

