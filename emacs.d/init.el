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
(defun setup-packaging-system ()
  (require 'package)

  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/") t)

  (add-to-list 'load-path "~/.emacs.d")

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  ; packages we need
  (package-install 'flycheck)
)

(defun setup-random-emacs ()
  ;; no startup msg
  (setq inhibit-startup-message t)

  ;; turn on paren match highlighting
  (show-paren-mode 1)

  ;; highlight entire bracket expression
  (setq show-paren-style 'expression)

  ;; display line numbers in margin
  (global-linum-mode 1)

  ;; display the column and line our cursor is on
  (column-number-mode 1)

  ;; stop creating those backup~ files
  (setq make-backup-files nil)

  ;; stop creating those #autosave# files
  (setq auto-save-default nil)

  ;; highlight the current line we are editing
  (global-hl-line-mode 1)

  ;; disable the toolbar
  (tool-bar-mode 0)

  ;; disable the menubar
  (menu-bar-mode 0)

  ;; Never insert tabs
  (setq-default indent-tabs-mode nil)


  ;; setup flycheck globally for all languages
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; This closes Emacs without prompting if we want to close all processes,
  ;; for example, if python shell is running
  (add-hook 'comint-exec-hook
    (lambda ()
      (set-process-query-on-exit-flag
        (get-buffer-process (current-buffer)) nil)))

)

(setup-random-emacs)
(provide 'init)

;;; init.el ends here

