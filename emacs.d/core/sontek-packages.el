;;; Code:
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; set package-user-dir to be relative to sontek-mode install path
(setq package-user-dir (expand-file-name "elpa" sontek-dir))
(package-initialize)

(defvar sontek-packages
  '(diminish
    flycheck
    grizzl
    guru-mode
    move-text
    multi-term
    projectile
    smartparens
    solarized-theme
    undo-tree
    volatile-highlights)
  "A list of packages to ensure are installed at launch.")

(defun sontek-check-all-packages-installed ()
  "Check if all packages in `sontek-packages' are installed."
  (every #'package-installed-p sontek-packages))

(defun sontek-reify-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package sontek-packages)
    (add-to-list 'sontek-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun sontek-install-packages ()
  "Install all packages listed in `sontek-packages'."
  (unless (sontek-check-all-packages-installed)
    ;; check for new packages (package versions)
    (message "%s" "sontek-mode is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'sontek-reify-package sontek-packages)))

(sontek-install-packages)

(provide 'sontek-packages)

;;; sontek-packages.el ends here
