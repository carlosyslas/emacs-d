;;; Magit for version control

(leaf magit
  :ensure t)

(provide 'cyslas-coding)


;;; Flycheck for diagnostics
(leaf flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; Manage projects with projectile
(leaf projectile
  :ensure t
  :bind
  (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

;;; LSP mode
(leaf lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-ts-mode . lsp)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)


(leaf lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp))))

;;; Python

(leaf python-ts-mode
  :ensure nil
  :mode "\\.py\\'")


(leaf pyvenv
  :ensure t)
