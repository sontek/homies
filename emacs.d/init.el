;; init.el --- Emacs configuration entry point
;;
;; Copyright (c) 2013 John Anderson ( sontek )
;;
;; Author: John Anderson < sontek@gmail.com >
;; URL: http://sontek.net

;;; Commentary:
;; This file is not part of GNU Emacs.  It is the personal configuration
;; for John Anderson.

;;; Code:
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "sontek-mode is powering up... Be patient, Young Padawan %s!" current-user)

(when (version< emacs-version "24.1")
  (error "sontek-mode requires at least GNU Emacs 24.1, but you're running %s" emacs-version))


;; Setup the directory structure
(defvar sontek-dir (file-name-directory load-file-name)
  "The root dir of the sontek-mode distribution.")
(defvar sontek-core-dir (expand-file-name "core" sontek-dir)
  "The home of sontek-mode's core functionality.")
(defvar sontek-vendor-dir (expand-file-name "vendor" sontek-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar sontek-savefile-dir (expand-file-name "savefile" sontek-dir)
  "This folder stores all the automatically generated save/history-files.")

;; add sontek-mode's directories to Emacs's `load-path`
(add-to-list 'load-path sontek-core-dir)
(add-to-list 'load-path sontek-vendor-dir)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(require 'sontek-packages)
(require 'sontek-editor)
(require 'sontek-ui)

(message "sontek-mode is ready to do thy bidding, Happy Hacking %s!" current-user)

;;; init.el ends here
