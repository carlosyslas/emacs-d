;;; Save emacs customizations in a separate file.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load-file custom-file))

;;; Mac specific configuration

(setq is-mac (equal system-type 'darwin))

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

;;; Setup package.el

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


(provide 'cyslas-base)
