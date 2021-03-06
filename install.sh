#!/bin/sh

rm ~/.emacs

emacs -q  --debug-init --load "build.el"

texhash


cat > ~/.emacs <<- EOM
(load-file "~/.emacs.d/elisp/config-main.el")
EOM



exit;
# Install reveal.js....



pwd=$(pwd)

revealdir=~/.emacs.d/reveal.js

if [ ! -d $revealdir ]; then
    # Control will enter here if $DIRECTORY doesn't exist.
    printf "\n Installing reveal to: %s !\n\n" $revealdir;
    cd ~/.emacs.d/
    git clone https://github.com/hakimel/reveal.js.git
    cd $pwd 
fi

powerlinedir=~/programs/powerlinefonts
if [ ! -d $powerlinedir ]; then
    printf "\n Installing powerline to: %s !\n\n" $powerlinedir;
    git clone https://github.com/powerline/fonts.git $powerlinedir
    cd $powerlinedir
    ./install.sh
    cd $pwd
fi

# look for oh-myzsh ...

ohmyzshdir=~/.oh-my-zsh
if [ ! -d $ohmyzshdir ]; then
    printf "\n Installing oh-my-zsh to: %s !\n\n" $ohmyzshdir;
    git clone https://github.com/robbyrussell/oh-my-zsh.git $ohmyzshdir
fi


# look for offlineimap

offlineimapdir=~/programs/offlineimap
if [ ! -d $offlineimapdir ]; then
    printf "\n Installing offlineimap to: %s !\n\n" $offlineimapdir;
    git clone https://github.com/OfflineIMAP/offlineimap.git $offlineimapdir
fi

mudir=~/programs/mu

if [ ! -d $mudir ]; then
    printf "\n Installing mu to: %s !\n\n" $mudir;
    git clone https://github.com/djcb/mu.git $mudir
    cd $mudir
    ./autogen.sh
    make
    cd $pwd 
fi

stow emacs -t ~/

stow zsh -t ~/
# make init.el
printf "\n Writing init.el \n\n";

cat > ~/.emacs.d/init_test.el <<- EOM
;; printed by install.sh
(defvar init.org-message-depth 3
  "What depth of init.org headings to message at startup.")

(with-temp-buffer
  (insert-file-contents (expand-file-name "config.org" user-emacs-directory))
  (goto-char (point-min))

  ;; Skip straight to the first elisp code block.
  (re-search-forward "^[\s-]*#\\\+BEGIN_SRC +emacs-lisp$")
  ;; Set point to previous heading
  (re-search-backward (format "\\\*\\\{1,%s\\\} +.*$"
                              init.org-message-depth))
  ;; ;; Alternatively, you can have all elisp code blocks under a single parent heading.
  ;; (search-forward "\n* init.el")

  ;; Begin parsing org file.
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headings
     ((looking-at
       (format "\\\*\\\{1,%s\\\} +.*$"
               init.org-message-depth))
      (message "%s" (match-string 0)))  ;; Messages where currently parsing.
     ;; Evaluate Code Blocks
     ((looking-at "^[\s-]*#\\\+BEGIN_SRC +emacs-lisp$")
      (let ((l (match-end 0)))
        (search-forward "#+END_SRC")
        ;; Write evaluated elisp source blocks to a single file.
        ;;(append-to-file l (match-beginning 0) "testinitorg.el")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 heading
     ((looking-at "^\\\* ")
      (goto-char (point-max)))))
  ;; Startup message.
  (message "Don't Panic."))
EOM
