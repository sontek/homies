;;; init.el --- Emacs configuration entry point
;;
;; Copyright (c) 2013 John Anderson ( sontek )
;;
;; Author: John Anderson < sontek@gmail.com >
;; URL: http://sontek.net


;; This file is not part of GNU Emacs.

; Use a dark color schema


(require 'package)
(require 'uniquify)
(require 'whitespace)

; Require ido everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(package-initialize)

; Make sure all our packages are installed
(defvar sontek-packages
  '(clojure-mode gist magit markdown-mode sass-mode scss-mode yaml-mode
        projectile yasnippet undo-tree csv-mode rainbow-mode nose
        pep8 pylint pyflakes pytest git-commit flymake flymake-easy
        flymake-python-pyflakes flymake-cursor rainbow-delimiters
        move-text jedi deferred
   )
  "A list of packages to ensure are installed at launch.")

(dolist (p sontek-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Handle non-unqiue buffers better
(setq uniquify-buffer-name-style 'forward)
(require 'undo-tree)
(global-undo-tree-mode 1)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'move-text)
(move-text-default-bindings)

; Configure whitespace settings to display when we are over 80chars
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 79)
(global-whitespace-mode 1)
(setq-default fill-column 79)

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

; Setup the python checker
(eval-after-load 'flymake '(require 'flymake-cursor))

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
;; (add-hook 'python-mode-hook
;;   (lambda ()
;;          (auto-complete-mode 1)))

;; ; Setup jedis autocompletion
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)                      ; optional
;; (setq jedi:complete-on-dot t)                 ; optional
