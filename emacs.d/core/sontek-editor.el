;;; Code:

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; but maintain correct appearance
(setq-default tab-width 8)

;; Newline at end of file
(setq require-final-newline t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete (if code is already indented,
;; try to comlete instead)
(setq tab-always-indent 'complete)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)

;; highlight the current line
(global-hl-line-mode +1)

;; Highlight any lines yanked recently
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; whitespace-mode, show any trailing whitespace
(require 'whitespace)
 ;; limit line length
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))
(global-whitespace-mode)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; turn on paren match highlighting
(show-paren-mode 1)

;; highlight entire bracket expression
(setq show-paren-style 'expression)

;; setup flycheck globally for all languages
(add-hook 'after-init-hook #'global-flycheck-mode)

;; allow alt-up/alt-down to move text
(require 'move-text)
(move-text-default-bindings)

(provide 'sontek-editor)
;;; sontek-editor.el ends here
