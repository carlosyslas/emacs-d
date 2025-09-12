;; -*- lexical-binding: t; -*-
;;; init.el --- My emacs custom configuration file.

;;; Comentary:


;;; Code:

;;;; Basic config:

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Backup files configuration
(setq backup-directory-alist `(("." . "~/.saves"))
      delete-old-versions t)

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
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Use M+k to kill entire line
(keymap-global-set "M-k" #'kill-whole-line)

;; Disable line wrapping
(set-default 'truncate-lines t)

;; Use howm to take/organize notes
;; TODO: Use C-tab for other-window?

;;;; Coding:

;; Auto close pairs
(electric-pair-mode 1)

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

;; Yasnippet
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

;; Show git diff
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))


;; Flycheck for diagnostics
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; Use ripgrep for searching
(use-package rg
  :ensure t)

(defun my/ripgrep-symbol-in-project ()
  "Find the symbol at point using ripgrep."
  (interactive)
  (let ((bounds (find-tag-default-bounds))
        (project (project-current))
        )
    (if project
        (cond
         (bounds
          (rg (buffer-substring-no-properties (car bounds) (cdr bounds))
              "*.*" (cdr project)))
         (t
          (message "No symbol at point")))
      (message "You are not in a project"))))


;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((python-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   (typescript-ts-mode . lsp)
   )
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(add-hook 'prog-mode-hook #'lsp-deferred)


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Apheleia for code formatting
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


;; Python
(defun my/easy-underscore (arg)
  "Insert '_' instead of ';'.  If ARG is provided insert ';'."
  ;; Stolen from https://github.com/gopar/.emacs.d/blob/4b5d487f96ad0d3ee2eb54ae11686679804ffbe0/README.org?plain=1#L122-L128
  (interactive "P")
  (if arg
      (insert ";")
    (insert "_")))

(defun my/bind-easy-underscore ()
  "Helper function to bind easy-underscore only in python-buffers."
  (local-set-key (kbd ";") #'my/easy-underscore))

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :hook (python-ts-mode . my/bind-easy-underscore))

(setenv "WORKON_HOME" (expand-file-name "~/.local/share/virtualenvs/"))


(use-package pyvenv
  :ensure t)

;; React

(customize-set-variable 'indent-tabs-mode nil)

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")


(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")


;;;; UI:

;; Declutter
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

;; Dired

(setq dired-dwim-target t)

;; Theme
(load-theme 'wombat)

;; Move where I mean
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Move text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;;; All the icons

(use-package all-the-icons
  :ensure t)

;; move between windows with Meta + Shift
(keymap-global-set "M-H" #'windmove-left)
(keymap-global-set "M-J" #'windmove-down)
(keymap-global-set "M-K" #'windmove-up)
(keymap-global-set "M-L" #'windmove-right)
(keymap-global-set "M-+" #'balance-windows)

;; Bind C-o to #'other-window
(keymap-global-set "C-o" #'other-window)

;; Repeat mark pop command with just one C-u
(setq set-mark-command-repeat-pop t)

;; Jump to char using avy
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :config
  (setq avy-background t))

;; Eshell
(defun eshell/p ()
  "cd to any of the known projectile projects."
  (eshell/cd
   (completing-read "Project: "
                    (project-known-project-roots))))


;; install treemacs for pairing and video recording forms
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

;; Pulse
(defun my/pulse-current-line (&rest _)
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(other-window))
  (advice-add command :after #'my/pulse-current-line))

;; Font family and size
(set-face-attribute 'default nil :height 150)
;; (set-frame-font "Dank Mono")
(set-frame-font "Adwaita Mono")

;;; init.el ends here
