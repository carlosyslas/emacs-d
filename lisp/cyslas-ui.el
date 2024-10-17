;;; Declutter

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

;;; Dired

(setq dired-dwim-target t)

;;; Add padding to all windows

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

;;; Theme
(load-theme 'wombat)

;;; Move where I mean
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;;; Move text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;;; All the icons

(use-package all-the-icons
  :ensure t)

;;; Modeline

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  )

(use-package minions
  :ensure t
  :config
  (minions-mode))

;;; Move between windows with Meta + Shift

(keymap-global-set "M-H" #'windmove-left)
(keymap-global-set "M-J" #'windmove-down)
(keymap-global-set "M-K" #'windmove-up)
(keymap-global-set "M-L" #'windmove-right)
(keymap-global-set "M-+" #'balance-windows)

;;; Bind C-o to #'other-window

(keymap-global-set "C-o" #'other-window)

;;; Repeat mark pop command with just one C-u

(setq set-mark-command-repeat-pop t)


;;; Jump to char using avy

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :config
  (setq avy-background t))

;;; Eshell

(defun eshell/p ()
  "cd to any of the known projectile projects."
  (eshell/cd
   (completing-read "Project: "
                    (project-known-project-roots))))


;;; install treemacs for pairing and video recording forms
(use-package treemacs
  :ensure t
  :bind
  (("M-0" . treemacs-select-window)))


;;; Use vundo to visualize the undo ring
(use-package vundo
  :ensure t)


;;; Completion
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

;;; Pulse
(defun my/pulse-current-line (&rest _)
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(other-window))
  (advice-add command :after #'my/pulse-current-line))

;;; Font family and size

(set-face-attribute 'default nil :height 150)
(set-frame-font "Dank Mono")

(provide 'cyslas-ui)
