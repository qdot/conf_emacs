;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; screen for emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add our prerequisites
;; Available from http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp/emu"))
(add-to-list 'load-path (expand-file-name "~/.emacs_files/elisp/apel"))

;; Set prefix key to `, just like in gnu screen
(setq elscreen-prefix-key "`")

(require 'elscreen)
(require 'elscreen-dired)
(require 'elscreen-color-theme)
(require 'elscreen-server)
;;(require 'elscreen-wl)