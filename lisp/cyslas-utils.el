(defun my/rebuild-emacs-config ()
  (interactive)
  (require 'org)
  (org-babel-tangle-file (expand-file-name "README.org" user-emacs-directory)))

(provide 'cyslas-utils)
