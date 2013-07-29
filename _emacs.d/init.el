;;; init.el --- Emacs configuration entry point
;;
;; Copyright (c) 2013 John Anderson ( sontek )
;;
;; Author: John Anderson < sontek@gmail.com >
;; URL: http://sontek.net


;; This file is not part of GNU Emacs.

; Use a dark color schema

(require 'package)
(package-initialize)

; Require ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 

(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives 
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))

(load-theme 'wombat t)

;; no startup msg
(setq inhibit-startup-message t)

; turn on paren match highlighting
(show-paren-mode 1)

; highlight entire bracket expression
(setq show-paren-style 'expression)

; display line numbers in margin
(global-linum-mode 1)

; display the column and line our cursor is on
(column-number-mode 1)

; stop creating those backup~ files
(setq make-backup-files nil)

; stop creating those #autosave# files
(setq auto-save-default nil) 

; highlight the current line we are editing
(global-hl-line-mode 1)

; disable the toolbar
(tool-bar-mode -1)

; disable the menubar
(menu-bar-mode -1)

; Never insert tabs
(setq-default indent-tabs-mode nil)
