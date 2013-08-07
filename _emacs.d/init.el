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
        pytest git-commit rainbow-delimiters move-text jedi deferred
        flycheck flymake flymake-python-pyflakes flymake-easy flymake-cursor
   )
  "A list of packages to ensure are installed at launch.")

(dolist (p sontek-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; On the fly syntax checking
(require 'flycheck)
(global-flycheck-mode)
(setq flycheck-highlighting-mode 'lines)

(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))

(require 'flymake-python-pyflakes)
(eval-after-load 'flymake '(require 'flymake-cursor))

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

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

;(load-theme 'wombat t)

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

;; ; Setup jedis autocompletion
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)                      ; optional
;; (setq jedi:complete-on-dot t)                 ; optional

; Allow creating lines above and below
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-M-return>") 'open-line-above)

(defun add-py-debug ()
      "add debug code and move line down"
    (interactive)
    (move-beginning-of-line 1)
    (insert "import pdb; pdb.set_trace();\n"))

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(add-hook 'python-mode-hook
    (lambda ()
        (define-key python-mode-map (kbd "C-c C-p") 'python-add-breakpoint)))
