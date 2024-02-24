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


;;; Ace window to jump between windows with ease
(leaf ace-window
  :ensure t
  :config
  (setq aw-keys '(?h ?j ?k ?l ?u ?i ?o ?p))
  :bind
  (("M-j" . ace-window)))


;;; Use vundo to visualize the undo ring
(leaf vundo
  :ensure t)


;;; Completion

(leaf vertico
  :ensure t
  :init
  (vertico-mode))

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
