
;; Startup Timing
;; Take the time at the beginning of load, then use it at the end to see
;; how long it took to load packages.

;; [[file:~/.emacs_files/emacs_conf.org::*Startup%20Timing][Startup\ Timing:1]]

(defconst qdot/emacs-start-time (current-time))

;; Startup\ Timing:1 ends here

;; Directory Configuration
;; Setting up a few different variables for the different types of
;; directories we have (configurations, locally stored versus el-get
;; fetched libs, etc...)

;; - Set up the base configuration directory and filename

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(defconst qdot/emacs-conf-dir
  ;;(file-name-directory (or load-file-name (buffer-file-name)))
  "~/.emacs_files/"
  "Directory for emacs configuration")

(defconst qdot/emacs-conf-file
  (expand-file-name (concat qdot/emacs-conf-dir "emacs_conf.org"))
  "Org-babel file for emacs configuration")

;; Directory\ Configuration:1 ends here

;; - Manually installed/maintained elisp directory

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(defconst qdot/emacs-elisp-dir
  (expand-file-name (concat qdot/emacs-conf-dir "elisp/"))
  "Directory for manually installed/maintained elisp files")

;; Directory\ Configuration:1 ends here

;; - package directory

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(defconst qdot/emacs-package-dir
  (expand-file-name (concat qdot/emacs-conf-dir "packages/"))
  "Directory for elisp packages from elpa/melpa")

;; Directory\ Configuration:1 ends here

;; - packages that aren't in MELPA/ELPA/etc that we have to maintain
;;   repos for. Things like CEDET and mu, as well as packages I'm
;;   actively developing at the moment.

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(defconst qdot/emacs-dev-package-dir
  (expand-file-name (concat qdot/emacs-conf-dir "dev-packages/"))
  "Directory for elisp packages for which we pull and/or maintain repos")

;; Directory\ Configuration:1 ends here

;; - yasnippets directory

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(defconst qdot/emacs-snippets-dir
  (expand-file-name (concat qdot/emacs-conf-dir "snippets/"))
  "Directory for snippets for yasnippet")

;; Directory\ Configuration:1 ends here

;; - As of emacs 23, ~/.emacs.d is user-emacs-directory

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(setq custom-file (concat user-emacs-directory "emacs_conf_custom.el"))
(if (not (file-exists-p custom-file))
    (with-temp-buffer
      (write-file custom-file)))
(load-file custom-file)

;; Directory\ Configuration:1 ends here

;; - Add configuration and scripts directories to proper variables

;; [[file:~/.emacs_files/emacs_conf.org::*Directory%20Configuration][Directory\ Configuration:1]]

(add-to-list 'load-path (expand-file-name qdot/emacs-conf-dir))
(add-to-list 'load-path (expand-file-name qdot/emacs-elisp-dir))

;; Directory\ Configuration:1 ends here

;; package.el                                                    :package:
;; Just use the built-in package manager, but add melpa/elpa/bleeding
;; edge package repos.

;; [[file:~/.emacs_files/emacs_conf.org::*package.el][package\.el:1]]

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/"))
      package-user-dir qdot/emacs-package-dir)
(package-initialize)

;; package\.el:1 ends here

;; use-package                                                   :package:
;; use-package keeps packaging loading clean and delayed until the last
;; possible second. Using the :ensure command means that this config file
;; also works as a package manifest when bringing up a new config
;; instance, though that rarely if ever happens.

;; [[file:~/.emacs_files/emacs_conf.org::*use-package][use-package:1]]

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)

;; use-package:1 ends here

;; Load org-mode
;; We need org-mode here to run org-babel on the file. Loading this early
;; is fine, as it's pretty much guaranteed that org-mode will get used
;; during a session.

;; [[file:~/.emacs_files/emacs_conf.org::*Load%20org-mode][Load\ org-mode:1]]

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org_archive\\'"  . org-mode)
         ("\\.org\\'"  . org-mode)))

;; Load\ org-mode:1 ends here

;; Load org configuration file

;; [[file:~/.emacs_files/emacs_conf.org::*Load%20org%20configuration%20file][Load\ org\ configuration\ file:1]]

(add-hook 'after-init-hook
          `(lambda ()
             ;; toggling on debug-on-error just in case there's issues in my
             ;; untangled code
             (setq debug-on-error t)
             (org-babel-load-file "~/.emacs_files/emacs_conf.org")
             ;; if we live through startup, return to normal error throwing
             (setq debug-on-error nil)))

;; Load\ org\ configuration\ file:1 ends here
