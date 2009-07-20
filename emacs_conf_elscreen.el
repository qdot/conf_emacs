;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; screen for emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Make sure we include the apel autoloads before this file (emacs_conf_apel.el)

;; Set prefix key to `, just like in gnu screen
(setq elscreen-prefix-key "`")

(require 'elscreen)
(require 'elscreen-dired)
(require 'elscreen-color-theme)
(require 'elscreen-server)
;;(require 'elscreen-wl)