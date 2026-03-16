;;; init.el --- My emacs custom configuration file. -- Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Basic config:

(use-package emacs
  :init
  ;; Create directory for backup, auto-save and lock files
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
  :custom
  ;; Declutter
  (warning-minimum-level :error)
  (initial-scratch-message nil)
  (inhibit-startup-message t)
  ;; Disable bell sound
  (ring-bell-function 'ignore)
  ;; Disable line wrapping
  (truncate-lines t)
  ;; Prefer spaces over tabs
  (indent-tabs-mode nil)
  ;; Repeat mark pop command with just one C-u
  (set-mark-command-repeat-pop t)
  ;; Backup files
  (backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
  (delete-old-versions t)
  ;; Auto-save files
  (auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
  ;; Lock-files
  (create-lockfiles nil)
  (tab-always-indent 'complete)
  ;; UI cleanup
  (use-short-answers t)
  :config
  ;; Use hippie-expand by default
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (save-place-mode 1)
  (load-theme 'modus-vivendi))


;; Save emacs customizations in a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))

;; Mac specific configuration
(defvar is-mac (equal system-type 'darwin) "Boolean flag to indicate Emacs is running on MacOS or not.")

(when is-mac
  ;; Maximize frame
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
  ;; Set option to nil to enable latin chars
  (setq mac-option-modifier nil)
  ;; Use control from Meta
  (setq ns-command-modifier (quote meta))
  ;; Set right option to control
  (setq mac-right-option-modifier 'control)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Setup package.el
(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Use M+k to kill entire line
(keymap-global-set "M-k" #'kill-whole-line)

;; Crux for quality of life
(use-package crux
  :ensure t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-," . crux-duplicate-current-line-or-region)
         ("M-o" . crux-other-window-or-switch-buffer)
         ("C-c I" . crux-find-user-init-file)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-g" . crux-keyboard-quit-dwim)))

;;;; Coding:

;; Auto close pairs
(electric-pair-mode 1)

;; Corfu for completions
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-cycle t))

;; Cape for completion-at-point backends
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

;; Expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->". mc/mark-next-like-this)
         ("C-<". mc/mark-previous-like-this)
         ("C-c C-<". mc/mark-all-like-this)))

;; Magit for version control
(use-package magit
  :ensure t)

;; YASnippet for snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Show git diff
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))

;; Apheleia for asynchronous code formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; Comment lines without moving the point
(defun my/comment-line ()
  "Comment lines without moving the point."
  (interactive)
  (save-mark-and-excursion
    (comment-line 1)))

(keymap-global-set "M-;" #'my/comment-line)

;; Devdocs
(use-package devdocs
  :ensure t
  :bind (("C-h D" . devdocs-lookup)))

;; Tree sitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  (treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))

;; Eglot for LSP support
(use-package eglot
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (vue-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio"
                             :initializationOptions
                             (:typescript (:tsdk "node_modules/typescript/lib"))))))

;; Python
(defun my/easy-underscore (arg)
  "Insert '_' instead of ';'.  If ARG is provided insert ';'."
  (interactive "P")
  (if arg
      (insert ";")
    (insert "_")))

(use-package python
  :bind (:map python-ts-mode-map
              (";" . my/easy-underscore)))

(use-package pet
  :ensure t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Go
(use-package go-ts-mode)

;; Vue
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'")

;; Markdown
(use-package markdown-mode
  :ensure t)

;; Postgres
(use-package pg
  :vc (:url "https://github.com/emarsden/pg-el/")
  :ensure t)

(use-package pgmacs :vc (:url "https://github.com/emarsden/pgmacs/")
  :ensure t
  :requires pg)

;; Yaml
(use-package yaml-mode
  :ensure t)

(use-package yaml-imenu
  :ensure t
  :config
  (yaml-imenu-enable))

;;;; UI:

(delete-selection-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(menu-bar-mode -1)

(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Dired
(use-package dired
  :custom
  (dired-dwim-target t))

;; Move where I mean
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Move text
(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; Jump to char using avy
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :config
  (setq avy-background t))

;; Eshell
(defun eshell/p ()
  "cd to any of the known projects."
  (eshell/cd
   (completing-read "Project: "
                    (project-known-project-roots))))


;; Treemacs
(use-package treemacs
  :ensure t
  :bind
  (("M-0" . treemacs-select-window)))

;; Use vundo to visualize the undo ring
(use-package vundo
  :ensure t)

;; Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

(use-package all-the-icons-completion
  :ensure t
  :after (all-the-icons)
  :init
  (all-the-icons-completion-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Font family and size
(set-face-attribute 'default nil :height 130)
(set-frame-font "Dank Mono")

;;; init.el ends here
