#!/usr/bin/env bash

rm ~/.emacs

emacs -q  --debug-init --load "build.el"

texhash


cat > ~/.emacs <<- EOM
(load-file "~/.emacs.d/elisp/config-main.el")
EOM

