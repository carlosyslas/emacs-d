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

;;; Add padding to all windows

(leaf spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

;;; Theme

(leaf modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted))

;;; All the icons

(leaf all-the-icons
  :ensure t)

;;; Modeline

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;; Transient window management keymap
(leaf transient
  :ensure t
  :bind (("C-t" . my/transient-window-management))
  :init
  (transient-define-prefix my/transient-window-management ()
    "Transient for managing windows"
    [["Switch"
      ("h" "←" windmove-left :transient t)
      ("j" "↓" windmove-down :transient t)
      ("k" "↑" windmove-up :transient t)
      ("l" "→" windmove-right :transient t)]
     ["Swap"
      ("H" "←" windmove-swap-states-left :transient t)
      ("J" "↓" windmove-swap-states-down :transient t)
      ("K" "↑" windmove-swap-states-up :transient t)
      ("L" "→" windmove-swap-states-right :transient t)]
     ["Split"
      ("v" "∣" split-window-right :transient t)
      ("s" "—" split-window-below :transient t)
      ("V" "∣" (lambda () (interactive) (select-window (split-window-right))) :transient t)
      ("S" "—" (lambda () (interactive) (select-window (split-window-below))) :transient t)]
     ["Delete"
      ("dh" "←" windmove-delete-left :transient t)
      ("dj" "↓" windmove-delete-down :transient t)
      ("dk" "↑" windmove-delete-up :transient t)
      ("dl" "→" windmove-delete-right :transient t)
      ("dd" "→" delete-window :transient t)
      ("do" "→" delete-other-windows :transient t)]
     ["Resize"
      ("[" "⟷-" shrink-window-horizontally :transient t)
      ("]" "⟷+" enlarge-window-horizontally :transient t)
      ("{" "↕-" shrink-window :transient t)
      ("}" "↕+" enlarge-window :transient t)
      ("=" "≡" balance-windows :transient t)]
     ["Misc"
      ("q" "Close" (lambda () (interactive)))]
     ]))

;;; Move between windows with Meta + Shift

(keymap-global-set "M-H" #'windmove-left)
(keymap-global-set "M-J" #'windmove-down)
(keymap-global-set "M-K" #'windmove-up)
(keymap-global-set "M-L" #'windmove-right)
(keymap-global-set "M-+" #'balance-windows)

;;; Bind C-o to #'other-window

(keymap-global-set "C-o" #'other-window)

;;; Jump to char using avy

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :config
  (setq avy-background t))

;;; Define my editor "forms"

(defun my/pairing-form ()
  "Enable turn on pre-defined features for pair programming."
  (save-selected-window
    (treemacs)
    (global-display-line-numbers-mode 1)))

(defun my/coding-form ()
  "Enable turn on pre-defined features for solo-programming."
  (save-selected-window
    (treemacs-select-window)
    (treemacs-quit)
    (global-display-line-numbers-mode -1)))

(defvar my/editor-forms '(
                          ("👨🏻‍💻 Coding" . my/coding-form)
                          ("🤝🏻 Pairing" . my/pairing-form)
                          ))

(defun my/select-editor-forms ()
  (interactive)
  (funcall (alist-get (completing-read "Editor form: "
                                       my/editor-forms) my/editor-forms nil nil 'equal)))

;;; Eshell

(defun eshell/p ()
  "cd to any of the known projectile projects."
  (eshell/cd
   (completing-read "Project: "
                    (project-known-project-roots))))


;;; install treemacs for pairing and video recording forms

(leaf treemacs
  :ensure t
  :bind
  (("M-0" . treemacs-select-window)))


;;; Use vundo to visualize the undo ring
(leaf vundo
  :ensure t)


;;; Completion

(leaf vertico
  :ensure t
  :init
  (vertico-mode))

(leaf all-the-icons-completion
  :ensure t
  :after (all-the-icons)
  :init
  (all-the-icons-completion-mode))

(leaf savehist
  :ensure t
  :init
  (savehist-mode))

(leaf orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Corfu for inline completion
(leaf corfu
  :ensure t
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle		. t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto		. t)
  (corfu-auto-delay	. 0.1)
  (corfu-auto-prefix	. 3)
  (corfu-preselect	. 'prompt) ;; Always preselect the prompt

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:corfu-map
   ("M-SPC" . corfu-insert-separator)
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))

  :init
  (global-corfu-mode)
  (corfu-history-mode))

;;; Font family and size

(set-face-attribute 'default nil :height 150)
(set-frame-font "Dank Mono")

(provide 'cyslas-ui)
