(fset 'yes-or-no-p 'y-or-n-p)

(pending-delete-mode t)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq warning-minimum-level :error)

(setq initial-scratch-message nil)

(setq inhibit-startup-message t)

(menu-bar-mode -1)

(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(leaf spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

(leaf modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted))

(leaf all-the-icons
  :ensure t)

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'cyslas-ui)
