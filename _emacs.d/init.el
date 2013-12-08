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
  (when window-system
    (require 'package))
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'load-path "~/.emacs.d")
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))
)

(setq package-refresh-first-time nil)

(defun package-require (package-name)
  "tries to require. If it fails, it retrieves the package and
   tries to require again (y benzap on #emacs)"
  (if (not (require package-name nil t))
      (progn
	(if (not package-refresh-first-time)
	    (save-excursion
	      (package-refresh-contents)
	      (setq package-refresh-contents t)))
	(package-install package-name)
	(require package-name nil t))))

(defun whitespace-cleanup-all ()
  (interactive)
  (setq indent-tab-mode nil)
  (whitespace-cleanup))

(defun whitespace-clean-and-compile ()
  "Cleans up whitespace and compiles. The compile-command is a
   varies with the active mode."
  (interactive)
  (whitespace-cleanup-all)
  (compile compile-command))

(defun setup-cider ()
  (package-require 'cider)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
  (add-hook 'nrepl-connected-hook
	    (defun pnh-clojure-mode-eldoc-hook ()
	      (add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
	      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode) ;;?
	      (cider-enable-on-existing-clojure-buffers)))
  (add-hook 'cider-mode-hook 'subword-mode)
  (package-require 'ac-nrepl)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-mode))
  (add-hook 'cider-mode-hook 'ac-nrepl-setup))

(defun setup-clojure ()
  (package-require 'clojure-mode)
  (setup-cider)
  (package-require 'clojure-test-mode)
  (add-hook 'clojure-mode 'paredit-mode)
  (add-hook 'clojure-mode 'packclojure-test-mode))

(defun setup-frontend ()
  (package-require 'sass-mode)
  (package-require 'scss-mode)
  (package-require 'yaml-mode))

(defun setup-python ()
  (package-require 'pytest)
  (package-require 'virtualenv)
  (package-require 'flymake-python-pyflakes)
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  (setq flymake-python-pyflakes-executable "flake8")

  (defun annotate-pdb ()
    (interactive)
    (highlight-lines-matching-regexp "import p?db")
    (highlight-lines-matching-regexp "p?db.set_trace()"))

  (add-hook 'python-mode-hook 'annotate-pdb)

  (defun python-add-breakpoint ()
    (interactive)
    (newline-and-indent)
    (insert "import pudb; pudb.set_trace()")
    (highlight-lines-matching-regexp "^[ ]*import pudb; pudb.set_trace()"))

  (add-hook 'python-mode-hook
    (lambda ()
      (define-key python-mode-map (kbd "C-c C-p") 'python-add-breakpoint)))

  (setq python-shell-interpreter-args "--colors Linux --no-autoindent")

  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
)

(defun setup-javascript ()
  (package-require 'js2-mode)
  (package-require 'js-comint)
  (setq inferior-js-program-command "node")
  (setq js-prog "node")

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  (setq inferior-js-mode-hook
	(lambda ()
	  ;; We like nice colors
	  (ansi-color-for-comint-mode-on)
	  ;; Deal with some prompt nonsense
	  (add-to-list
	   'comint-preoutput-filter-functions
	   (lambda (output)
	     (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))

  (defun starts-with-js-comment (line)
    (equal 0 (string-match "^[[:space:]]*//" line)))

  (defun remove-js-comments (mystr)
    "Remove lines starting with // and return as single line of JS code.
     See: http://www.emacswiki.org/emacs/ElispCookbook
    "
    (mapconcat 'identity
	       (remove-if 'starts-with-js-comment
			  (split-string mystr "\n"))
	       " "
	       ))

  (defun js-send-region (start end)
    "Send the current region to the inferior Javascript process."
    (interactive "r")

    ;; BSS: echo for node and node_emacs
    (if (or (equal js-prog "node_emacs")
	    (equal js-prog "node"))
	(setq comint-process-echoes t)
      )
    (run-js inferior-js-program-command t)

    (setq curwindow (selected-window))
    (save-excursion (let ((jscode (remove-js-comments (buffer-substring start end))))
		      (set-buffer "*js*")
		      (insert jscode)
		      (comint-send-input)
		      ))
    (select-window curwindow))

  (defun js-send-line ()
    (interactive)
    (save-excursion
      (end-of-line)
      (let ((end (point)))
	(beginning-of-line)
	(js-send-region (point) end))))


  (setq jshint-cli "jshint --show-non-errors ")

  (setq compilation-error-regexp-alist-alist
        (cons '(jshint-cli "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
                1 ;; file
                2 ;; line
                3 ;; column
                )
              compilation-error-regexp-alist-alist))

  (setq compilation-error-regexp-alist
        (cons 'jshint-cli compilation-error-regexp-alist))

  (add-hook 'js2-mode-hook '(lambda ()
    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
    (local-set-key "\C-cb" 'js-send-buffer)
    (local-set-key "\C-cr" 'js-send-region)
    (local-set-key "\C-cl" 'js-send-line)
    (set (make-local-variable 'compile-command)
	 (let ((file buffer-file-name)) (concat jshint-cli file)))
    (set (make-local-variable 'compilation-read-command) nil)
    (local-set-key "\C-c\C-u" 'whitespace-clean-and-compile)
    ))

 )

(defun setup-projectile ()
  (package-require 'projectile)
  (projectile-global-mode)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'grizzl)
  (setq projectile-tags-command
    "ctags -e -R --extra=+fq --exclude=.git --exclude=.tox --exclude=.tests -f ")

  (defun my-find-tag ()
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "TAGS"))
	(visit-project-tags)
      (build-ctags))
    (etags-select-find-tag-at-point))

  (defun visit-project-tags ()
    (interactive)
    (let ((tags-file (concat (projectile-project-root) "TAGS")))
      (visit-tags-table tags-file)
      (message (concat "Loaded " tags-file))))

  (global-set-key (kbd "M-.") 'my-find-tag))

(defun setup-flycheck ()
  (package-require 'flycheck)
  (package-require 'flymake)
  (package-require 'flymake-easy)
  (package-require 'flymake-cursor)

  (global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (eval-after-load 'flymake '(require 'flymake-cursor)))

(defun setup-multiple-cursors ()
  (package-require 'multiple-cursors)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-!") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(defun setup-hide-show ()
  (global-set-key (kbd "C-c S") 'hs-show-all)
  (global-set-key (kbd "C-c s") 'hs-show-block)
  (global-set-key (kbd "C-c h") 'hs-hide-block)
  (global-set-key (kbd "C-c H") 'hs-hide-all)

  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'lisp-mode-hook       'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (add-hook 'python-mode-hook     'hs-minor-mode)
)
(defun setup-remaining-packages ()
  (package-require 'gist)
  (package-require 'magit)
  (package-require 'markdown-mode)
  (package-require 'undo-tree)
  (package-require 'comint)
  (package-require 'rainbow-mode)
  (package-require 'rainbow-delimiters)
  (package-require 'git-commit)
  (package-require 'move-text)
  (package-require 'deferred)
  (package-require 'ack-and-a-half)
  (package-require 'dash)
  (package-require 's)
  (package-require 'etags-select)
  (package-require 'smartscan)
  (package-require 'color-theme-sanityinc-solarized)
  (package-require 'grizzl)

  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)

  (smartscan-mode 1)
  (global-undo-tree-mode)
  (setq uniquify-buffer-name-style 'forward)
  (global-rainbow-delimiters-mode)
  (move-text-default-bindings)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 79)
  (global-whitespace-mode 1)
  (setq-default fill-column 79)

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t)
)

(defun setup-random-emacs ()
  ;; Nice color scheme
  (load-theme 'sanityinc-solarized-light t)

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
  (tool-bar-mode -1)

  ;; disable the menubar
  (menu-bar-mode -1)

  ;; Never insert tabs
  (setq-default indent-tabs-mode nil)

  ;; C indent style
  (setq c-default-style "linux"
	c-basic-offset 4)

  (require 'auto-complete)
  (global-auto-complete-mode t)

  ;; This closes emacs without prompting if we want to close all processes,
  ;; for example, if python shell is running
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (flet ((process-list ())) ad-do-it))
)

(setup-packaging-system)
(setup-flycheck)
(setup-clojure)
(setup-python)
(setup-frontend)
(setup-javascript)
(setup-projectile)
(setup-multiple-cursors)
(setup-hide-show)
(setup-remaining-packages)
(setup-random-emacs)

;;; init.el ends here
