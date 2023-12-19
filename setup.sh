#!/bin/sh

ln -s $(pwd) ~/.config/emacs

emacs -q --eval '(progn (require '"'"'org) (org-babel-tangle-file "README.org") (save-buffers-kill-terminal t))'
