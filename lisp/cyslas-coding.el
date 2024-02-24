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
  ((python-ts-mode . lsp)
   (typescript-ts-mode . lsp)
   )
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(add-hook 'prog-mode-hook #'lsp-deferred)


(leaf lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

;;; Tree sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; Python

(leaf python-ts-mode
  :ensure nil
  :mode "\\.py\\'")

(leaf pyvenv
  :ensure t)

;;; React

(leaf tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")
