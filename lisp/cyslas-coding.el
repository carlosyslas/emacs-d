;;; Auto close pairs

(electric-pair-mode 1)

;;; Expand region

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;;; Magit for version control

(use-package magit
  :ensure t)

;;; Yasnippet

(defun my/get-buffer-camel-case-name ()
  (s-lower-camel-case
   (file-name-sans-extension
    (buffer-name (current-buffer)))))

(defun my/get-buffer-pascal-case-name ()
  (s-upper-camel-case
   (file-name-sans-extension
    (buffer-name (current-buffer)))))


;; WIP
(defun my/identity-with-callback (v cb)
  "Return the same value V it got at the first argument and execute the CB argument."
  (funcall cb)
  v)

;; WIP
(defun my/funcall-after-yas-expansion (cb args)
  "Call CB after yasnippet expansion passing ARGS."
  (my/identity-with-callback yas-text (lambda ()
                                        (when yas-moving-away-p
                                          (apply cb args)))))



(use-package yasnippet
  :ensure t
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

;;; Show TODOs in magit buffer

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode 1))


;;; Show gid diff
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))


;;; Flycheck for diagnostics
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; Manage projects with projectile
(leaf projectile
  :ensure t
  :bind
  (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

;;; Use ripgrep for searching

(use-package rg
  :ensure t)

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

;;; Apheleia for code formatting
(use-package apheleia
  :ensure t
  :config
  ;; TODO: Find out how to properly set custom variable values
  (setq apheleia-formatters-respect-indent-level nil)
  (setf
   (alist-get 'prettier-typescript apheleia-formatters)
   '("npx" "prettier" "--stdin-filepath" filepath "--parser=typescript"
     ))
  (setf
   (alist-get 'tsx-ts-mode apheleia-mode-alist)
   'prettier-typescript)
  (apheleia-global-mode +1))

(setq apheleia-log-only-errors nil)

;;; Python

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'")

(leaf pyvenv
  :ensure t)

;;; React

(customize-set-variable 'indent-tabs-mode nil)

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")


(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")


(provide 'cyslas-coding)
