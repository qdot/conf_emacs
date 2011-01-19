(load-library "mudel.el")

(defgroup mudel-faces 
  nil 
  "Faces for mudel MUD mode")

(defface mudel-page-face 
 '((t (:foreground "green" :slant italic :weight bold)))
 "Mudel Paging face"
 :group 'mudel-faces)

(defface mudel-whisper-face 
 '((t (:foreground "yellow" :slant italic :weight bold)))
 "Mudel Whisper face"
 :group 'mudel-faces)

(defface mudel-scream-face 
 '((t (:foreground "blueviolet" :weight bold)))
 "Mudel Scream face"
 :group 'mudel-faces)

(defface mudel-connect-face 
 '((t (:foreground "red" :weight bold)))
 "Mudel Connect/Disconnect face"
 :group 'mudel-faces)

(setq mudel-mode-hook nil)
(add-hook 'mudel-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                      '(("\\(?:##p>\\|You page\\).*$" . 'mudel-page-face)
                                        ("\\(?:##s>\\).*" . 'mudel-scream-face)
                                        ("\\(?:##w>\\|You whisper\\).*$" . 'mudel-whisper-face)))))

(defun qdot/muck-furry ()
  (interactive)
  (mudel "FurryMUCK" "muck.furry.com" 8888))

(defun qdot/muck-taps ()
  (interactive)
  (mudel "Taps" "tapestries.fur.com" 2069))

(defun qdot/moo-darksleep ()
  (interactive)
  (mudel "darksleep" "darksleep.com" 7777))

