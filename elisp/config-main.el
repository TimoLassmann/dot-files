
;; To speed things up

(setq gc-cons-threshold 16777216)
(setq gc-cons-percentage 0.1)

;; Package Manager
;; To start we need the package "package"

(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Next let's make sure we have use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Init File Support

;;    Load up a collection of enhancements to Emacs Lisp, including [[https://github.com/magnars/dash.el][dash]],
;;    [[https://github.com/magnars/s.el][s]] for string manipulation, and [[https://github.com/rejeep/f.el][f]] for file manipulation.

(require 'cl)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; Whoami

(setq user-full-name "Timo Lassmann"
      user-mail-address "timo.lassmann@telethonkids.org.au")

;; Emacs directory

(defconst tl/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun tl/emacs-subdirectory (d) (expand-file-name d tl/emacs-directory))

(message "%s" (tl/emacs-subdirectory "elisp"))
(add-to-list 'load-path (tl/emacs-subdirectory "elisp"))

;; remove startup screen

(setq inhibit-startup-message t)

;; remove bars ets

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable bell

(setq ring-bell-function 'ignore)

;; UTF-8

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Turn off arrow keys

(require 'no-easy-keys)
(no-easy-keys 1)

;; Undo / redo
;;    According to this article, I get better functionality than the redo+ plugin (which I can’t seem to get working well).

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Kill this buffer
;;    Assume that I always want to kill the current buffer when hitting C-x k.

(defun tl/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'tl/kill-current-buffer)

;; Tabs 
;;    Never use tabs. Tabs are the devil’s whitespace.

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-always-indent 'complete)

;; Location of mactex (if we are using mac - unlikely...
;;    Tell emacs about the mactex installation...

(setenv "PATH" (concat "/Library/TeX/texbin" ":" (getenv "PATH")))

;; Dired settings 

;;    Taken from: https://github.com/munen/emacs.d


;;    Ability to use =a= to visit a new directory or file in dired instead of using =RET=.
;;    =RET= works just fine, but it will create a new buffer for every interaction
;;    whereas a reuses the current buffer.

(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-listing-switches "-alh")

;; Path

(let ((path-from-shell (shell-command-to-string "/bin/bash -l -c 'echo $PATH'")))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

(setq temporary-file-directory "/tmp")

;; Modernizing Emacs

;;    Found [[https://github.com/wasamasa/dotemacs/blob/master/init.org#init][here]] how to remove the warnings from the GnuTLS library when
;;    using HTTPS... increase the minimum prime bits size:

(setq gnutls-min-prime-bits 4096)

;; Turn off sleep mode 
;;    I keep hitting this by accidental

(global-unset-key (kbd "C-z"))

;; Highligh current line

;;     hl-line is awesome! It’s not very awesome in the terminal version of emacs though, so we don’t use that. Besides, it’s only used for programming.

(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

;; Pretty symbols

;; Changes lambda to an actual symbol and a few others as well, only in the GUI version though.

(when window-system
      (use-package pretty-mode
      :ensure t
      :config
      (global-pretty-mode t)))

;; yes-no to y-n

(defalias 'yes-or-no-p 'y-or-n-p)

;; Async

;; Lets us use asynchronous processes wherever possible, pretty useful.

(use-package async
  :ensure t
  :init (dired-async-mode 1))

;; Projectile

;; Projectile is an awesome project manager, mostly because it recognizes directories with a .git directory as projects and helps you manage them accordingly.
;; Enable projectile globally

;; This makes sure that everything can be a project.

(use-package projectile
  :ensure t
  :init
    (projectile-mode 1))

;; Let projectile call make

(global-set-key (kbd "<f5>") 'projectile-compile-project)

;; Dashboard

;; This is your new startup screen, together with projectile it works in unison and provides you with a quick look into your latest projects and files. Change the welcome message to whatever string you want and change the numbers to suit your liking, I find 5 to be enough.

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner 'official)
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)))
    (setq dashboard-banner-logo-title ""))

;; space-line

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p nil)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

;; No separator!

(setq powerline-default-separator nil)

;; Cursor position

;; Show the current line and column for your cursor. We are not going to have relative-linum-mode in every major mode, so this is useful.

(setq line-number-mode t)
(setq column-number-mode t)

;; Clock

;; If you prefer the 12hr-format, change the variable to nil instead of t.
;; Time format

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")

;; Enabling the mode

;; This turns on the clock globally.

(display-time-mode 1)

;; Battery indicator

;; A package called fancy-battery will be used if we are in GUI emacs, otherwise the built in battery-mode will be used. Fancy battery has very odd colors if used in the tty, hence us disabling it.

(use-package fancy-battery
  :ensure t
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (if window-system
      (fancy-battery-mode)
      (display-battery-mode)))

;; Default shell should be zsh

;;    I don’t know why this is a thing, but asking me what shell to launch every single time I open a terminal makes me want to slap babies, this gets rid of it. This goes without saying but you can replace bash with your shell of choice.

(defvar my-term-shell "/usr/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Easy to remember keybinding

;; In loving memory of bspwm, Super + Enter opens a new terminal, old habits die hard.

(global-set-key (kbd "<s-return>") 'ansi-term)

;; a prerequisite for others packages

(use-package ivy
  :ensure t)

;; scrolling and why does the screen move

;; I don’t know to be honest, but this little bit of code makes scrolling with emacs a lot nicer.

(setq scroll-conservatively 100)

;; which-key and why I love emacs

;; In order to use emacs, you don’t need to know how to use emacs. It’s self documenting, and coupled with this insanely useful package, it’s even easier. In short, after you start the input of a command and stop, pondering what key must follow, it will automatically open a non-intrusive buffer at the bottom of the screen offering you suggestions for completing the command, that’s it, nothing else.

;; It’s beautiful

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

;; windows,panes and why I hate other-window

;; Some of us have large displays, others have tiny netbook screens, but regardless of your hardware you probably use more than 2 panes/windows at times, cycling through all of them with C-c o is annoying to say the least, it’s a lot of keystrokes and takes time, time you could spend doing something more productive.
;; switch-window

;; This magnificent package takes care of this issue. It’s unnoticeable if you have <3 panes open, but with 3 or more, upon pressing C-x o you will notice how your buffers turn a solid color and each buffer is asigned a letter (the list below shows the letters, you can modify them to suit your liking), upon pressing a letter asigned to a window, your will be taken to said window, easy to remember, quick to use and most importantly, it annihilates a big issue I had with emacs. An alternative is ace-window, however by default it also changes the behaviour of C-x o even if only 2 windows are open, this is bad, it also works less well with exwm for some reason.

(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

;; Following window splits

;; After you split a window, your focus remains in the previous one. This annoyed me so much I wrote these two, they take care of it.

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; swiper and why is the default search so lame

;; I like me some searching, the default search is very meh. In emacs, you mostly use search to get around your buffer, much like with avy, but sometimes it doesn’t hurt to search for entire words or mode, swiper makes sure this is more efficient.

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; buffers and why I hate list-buffers

;; Another big thing is, buffers. If you use emacs, you use buffers, everyone loves them. Having many buffers is useful, but can be tedious to work with, let us see how we can improve it.
;; Always murder current buffer

;; Doing C-x k should kill the current buffer at all times, we have ibuffer for more sophisticated thing.

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Kill buffers without asking for confirmation

;; Unless you have the muscle memory, I recommend omitting this bit, as you may lose progress for no reason when working.

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Turn switch-to-buffer into ibuffer

;; I don’t understand how ibuffer isn’t the default option by now. It’s vastly superior in terms of ergonomics and functionality, you can delete buffers, rename buffer, move buffers, organize buffers etc.

(global-set-key (kbd "C-x b") 'ibuffer)

;; IDO

(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ido-everywhere t
               ido-enable-tramp-completion t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (add-to-list 'completion-ignored-extensions ".o")
  (add-to-list 'completion-ignored-extensions ".elc")
  (add-to-list 'completion-ignored-extensions "~")
  (add-to-list 'completion-ignored-extensions ".bin")
  (add-to-list 'completion-ignored-extensions ".bak")
  (add-to-list 'completion-ignored-extensions ".obj")
  (add-to-list 'completion-ignored-extensions ".map")
  (add-to-list 'completion-ignored-extensions ".a")
  (add-to-list 'completion-ignored-extensions ".ln")
  (add-to-list 'completion-ignored-extensions ".mod")
  (add-to-list 'completion-ignored-extensions ".gz")
  (add-to-list 'completion-ignored-extensions ".aux")
  (add-to-list 'completion-ignored-extensions ".tdo")
  (add-to-list 'completion-ignored-extensions ".fmt")
  (add-to-list 'completion-ignored-extensions ".swp")
  (add-to-list 'completion-ignored-extensions ".pdfsync")
  (add-to-list 'completion-ignored-extensions ".pdf")
  (add-to-list 'completion-ignored-extensions ".vrb")
  (add-to-list 'completion-ignored-extensions ".idx")
  (add-to-list 'completion-ignored-extensions ".ind")
  (add-to-list 'completion-ignored-extensions ".bbl")
  (add-to-list 'completion-ignored-extensions ".toc")
  (add-to-list 'completion-ignored-extensions ".blg")
  (add-to-list 'completion-ignored-extensions ".snm")
  (add-to-list 'completion-ignored-extensions ".ilg")
  (add-to-list 'completion-ignored-extensions ".log")
  (add-to-list 'completion-ignored-extensions ".out")
  (add-to-list 'completion-ignored-extensions ".pyc")
  (add-to-list 'completion-ignored-extensions ".DS_Store")
  (add-to-list 'completion-ignored-extensions "-blx.bib")
  (add-to-list 'completion-ignored-extensions ".run.xml")
  (add-to-list 'completion-ignored-extensions ".fls")
  (add-to-list 'completion-ignored-extensions ".fdb_latexmk")
  (add-to-list 'completion-ignored-extensions ".bcf")
  (add-to-list 'completion-ignored-extensions ".rel")
  (add-to-list 'completion-ignored-extensions ".epub")
  )

;; FLX package:

(use-package flx-ido
  :ensure t
  :init (setq ido-enable-flex-matching t
              ido-use-faces t)
  :config (flx-ido-mode 1))

;; According to Ryan Neufeld, we could make IDO work vertically, which is much easier to read. For this, I use ido-vertically:

(use-package ido-vertical-mode
  :ensure t
  :init               ; I like up and down arrow keys:
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode 1))

;; This sorts an IDO filelist by mtime instead of alphabetically.

(defun ido-sort-mtime ()
  "Reorder the IDO file list to sort from most recently modified."
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (ignore-errors
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; SMEX

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind 
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;; switch buffers

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; iBuffers

(global-set-key (kbd "C-x b") 'ibuffer)

;; Avy

(use-package avy
  :ensure t
  :bind
    ("M-s" . avy-goto-char))

;; Beacon

(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

;; Rainbow

(use-package rainbow-mode
  :ensure t
  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

;; Flyspell config 

;; Installing aspell on linux:

;; #+BEGIN_EXAMPLE sh
;; apt install aspell aspell-en
;; #+END_EXAMPLE

;; on mac: 

;; #+BEGIN_EXAMPLE
;; brew install aspell 
;; #+END_EXAMPLE

;; Note in the config below I assume aspell is installed in =/usr/bin/= !.

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq ispell-program-name "aspell"
        ispell-local-dictionary "en_GB"
        ;;ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
        ispell-list-command "--list"
        ispell-local-dictionary-alist '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
                                      t ; Many other characters
                                      ("-d" "en_GB") nil utf-8))))

;; Writegood mode 
;;    This does not work - there is a wring gpg signature in melpa...

(when (file-exists-p "/home/user/programs/writegood-mode")
(message "Loading writegood-mode")
(add-to-list 'load-path "/home/user/programs/writegood-mode")
(require 'writegood-mode)
    (add-hook 'text-mode-hook 'writegood-mode)
     (add-hook 'org-mode-hook 'writegood-mode)
)

;; LangTool
   
;;    I added the Emacs-langtool code from:
   
;;    https://github.com/mhayashi1120/Emacs-langtool
   
;;    To my =/elisp/= directory. 
   
;;    To install langtool install =maven= package, java 8 then:

;;    #+BEGIN_EXAMPLE sh
;;    cd ~/programs
;;    git clone https://github.com/languagetool-org/languagetool.git
;;    ./build.sh languagetool-standalone package

;;    #+END_EXAMPLE
;;    This does not work! 
   
;;    I now simply download the pre-compiles zip package... 

;;    To load:

(require 'langtool)
(setq langtool-language-tool-jar "/home/user/programs/langtool/LanguageTool-4.0/languagetool-commandline.jar")

;; General setup

;;    load org mode

(use-package org
  :init
  (setq org-use-speed-commands t
        org-return-follows-link t
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil))

;; Directory, inbox ..

(setq org-directory "~/Dropbox")
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))
(setq org-index-file (org-file-path "/capture/inbox.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

;; Multiple files for agenda source:

;;   (setq org-agenda-files (list org-index-file))
(setq org-agenda-files '("~/Dropbox/capture"
                         "~/Dropbox/work"
                         "~/Dropbox/planning"
                         "~/Dropbox/life"))

;; Refile targets / create new targets if necessary

;;(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-targets '(("~/Dropbox/work/work-todo.org" :maxlevel . 2)
                           ("~/Dropbox/life/life-todo.org" :maxlevel . 2)
                           ))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Drawers

(setq org-log-into-drawer t)

;; Add the REPORT drawer
(setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "REPORT"))

;; Hitting C-c C-x C-s will mark a todo as done and move it to an appropriate place
;;    in the archive.

(defun tl/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

;;    (define-key org-mode-map (kbd "C-c C-x C-s") 'tl/mark-done-and-archive)

;; Record the time that a todo was archived.

(setq org-log-done 'time)

;; Capture
;;    Capture templates..

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline org-index-file "Inbox")
               "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
              ("n" "note" entry (file+headline org-index-file "Inbox")
               "* %?\n\n  %i\n\n  See: %a" :empty-lines 1)
              ("r" "respond" entry (file+headline org-index-file "Inbox")
               "* TODO Respond to %:from on %:subject\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
              ("m" "Mail" entry (file+headline org-index-file "Inbox")
               "* TODO %?\n%a   %:from %:fromname %:fromaddress" :prepend t :jump-to-captured t)
              ("p" "Daily Plan" plain (file+datetree "~/Dropbox/planning/daily-plan.org")
               "+ [ ] The 3 most important tasks [/]
                - [ ] 
                - [ ] 
                - [ ] 
              + [ ] Other tasks that are in the system [/]
                - [ ] 
              + [ ] ToDos which are not tracked by my system [/]
                - [ ] " :immediate-finish t)
              )))

;; Taking Meeting Notes

;; directly from https://github.com/howardabrams/dot-files/blob/master/emacs-org.org)

;; I’ve notice that while I really like taking notes in a meeting, I don’t always like the multiple windows I have opened, so I created this function that I can easily call to eliminate distractions during a meeting.

(defun meeting-notes ()
  "Call this after creating an org-mode heading for where the notes for the meeting
should be. After calling this function, call 'meeting-done' to reset the environment."
  (interactive)
  (outline-mark-subtree)                              ;; Select org-mode section
  (narrow-to-region (region-beginning) (region-end))  ;; Only show that region
  (deactivate-mark)
  (delete-other-windows)                              ;; Get rid of other windows
  (text-scale-set 3)                                  ;; Text is now readable by others
  (fringe-mode 0)
  (message "When finished taking your notes, run meeting-done."))

;; Of course, I need an ‘undo’ feature when the meeting is over…

(defun meeting-done ()
  "Attempt to 'undo' the effects of taking meeting notes."
  (interactive)
  (widen)                                       ;; Opposite of narrow-to-region
  (text-scale-set 0)                            ;; Reset the font size increase
  (fringe-mode 1)
  (winner-undo))                                ;; Put the windows back in place

;; Coding

;;    Allow babel to evaluate C ...

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (R . t)
   (dot . t)
   (emacs-lisp . t)
   (sh . t) 
   (makefile . t)
   (latex .t)
   ))

;; Don’t ask before evaluating code blocks.

(setq org-confirm-babel-evaluate nil)

;; smart brackets in export

(setq org-export-with-smart-quotes t)

;; Export

;;    Export packages...

(require 'ox-latex)
(require 'ox-beamer)

;; Htmlize required for reveal...

(use-package htmlize
  :ensure t)

;; Flyspell
;;    Enable spell-checking in Org-mode.

(add-hook 'org-mode-hook 'flyspell-mode)

;; Color and display
   
;;    Use syntax highlighting in source blocks while editing.

(setq org-src-fontify-natively t)

;; Make TAB act as if it were issued in a buffer of the language’s major mode.

(setq org-src-tab-acts-natively t)

;; When editing a code snippet, use the current window rather than popping open a
;;    new one (which shows the same information).

(setq org-src-window-setup 'current-window)

;; Image preview 

;;    Inline images support:

(setq org-latex-create-formula-image-program 'imagemagick)

(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
(setq org-latex-create-formula-image-program 'imagemagick)


(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)

;; Keybindings


;;    Standard bindings

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Quickly open index file

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

;; undef a key

(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined))
          'append)

;; Org-ref

(use-package org-ref
  :ensure t)

;; Define format for bibtex entries

;; variables that control bibtex key format for auto-generation
;; I want firstauthor-year-title-words
;; this usually makes a legitimate filename to store pdfs under.
(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

;; Where are the refs?

(setq reftex-default-bibliography '("~/Dropbox/work/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/work/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/work/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/work/bibliography/bibtex-pdfs/")

(setq org-ref-completion-library 'org-ref-ivy-cite)

;; Latex templates
;;    Latex templates

(setq org-latex-listings t)

;;(setq org-latex-to-pdf-process '("xelatex %f && bibtex %f && xelatex %f && xelatex %f"))
(defun sk-latexmk-cmd (backend)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (when (org-export-derived-backend-p backend 'latex)
    (let ((texcmd)))
    ;; default command: xelatex
    (setq texcmd "jobname=$(basename %f | sed 's/\.tex//');latexmk -xelatex -shell-escape -quiet %f && mkdir -p latex.d && mv ${jobname}.* latex.d/. && mv latex.d/${jobname}.{org,pdf,fdb_latexmk,aux} .")
    ;; pdflatex -> .pdf
    (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
        (setq texcmd "latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex %f"))
    ;; xelatex -> .pdf
    (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        (setq texcmd "latexmk -pdflatex='xelatex -interaction nonstopmode' -pdf -bibtex -f  %f"))
    ;; LaTeX compilation command
    (setq org-latex-pdf-process (list texcmd))))

(org-add-hook 'org-export-before-processing-hook 'sk-latexmk-cmd)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

;; NHMRC project grant

(add-to-list 'org-latex-classes
             '("NHMRC_project_grant"
               "\\documentclass[12pt,table,names]{article}
\\usepackage{\\string~\"/.emacs.d/latex_templates/NHMRC_grant\"}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Rebuttal...

(add-to-list 'org-latex-classes
             '("NHMRC_project_grant_rebuttal"
               "\\documentclass[12pt,table,names]{article}
  \\usepackage{\\string~\"/.emacs.d/latex_templates/NHMRC_grant\"}
  [NO-DEFAULT-PACKAGES]
  [NO-PACKAGES]"
               ("\\subsection{%s}" . "\\section*{%s}")
               ("\\subsubsection{%s}" . "\\subsection*{%s}")q
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ARC Discovery Grant

(add-to-list 'org-latex-classes
             '("ARC_discovery_grant"
               "\\documentclass[12pt]{article}
\\usepackage{\\string~\"/.emacs.d/latex_templates/ARC_discovery\"}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; Internal report

(add-to-list 'org-latex-classes
             '("report"
               "\\documentclass[12pt]{article}
\\usepackage{\\string~\"/.emacs.d/latex_templates/report\"}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Simple presentation

(add-to-list 'org-latex-classes
             '("simplepresentation"
               "\\documentclass[aspectratio=169,18pt,t]{beamer}
\\usepackage{\\string~\"/.emacs.d/latex_templates/simple\"}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; General

;;    I like shallow indentation, but tabs are displayed as 8 characters by default. This reduces that.

(setq-default tab-width 2)

;; Treating terms in CamelCase symbols as separate words makes editing a little
;;    easier for me, so I like to use subword-mode everywhere.

(global-subword-mode 1)

;; Compilation output goes to the *compilation* buffer. I rarely have that window
;;    selected, so the compilation output disappears past the bottom of the window.
;;    This automatically scrolls the compilation window so I can always see the
;;    output.

;;(setq compilation-scroll-output t)
 (setq compile-command "make")
 (setq compilation-scroll-output 'first-error)
 (setq compilation-always-kill t)
 (setq compilation-disable-input t)
(add-hook 'compilation-mode-hook 'visual-line-mode)

;; Flycheck

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'c-mode-hook (lambda () (setq flycheck-clang-language-standard "c11")))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Magit 

;;    I played with this before..

(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restnore-window-configuration t)

  :bind ("C-x g" . magit-status))

;; Aggressive Auto Indention
   
;;    Automatically indent without use of the tab found in this article, and seems to be quite helpful for many types of programming languages.
   
;;    To begin, we create a function that can indent a function by calling indent-region on the beginning and ending points of a function.

(defun indent-defun ()
  "Indent current defun.
Do nothing if mark is active (to avoid deactivaing it), or if
buffer is not modified (to avoid creating accidental
modifications)."
  (interactive)
  (unless (or (region-active-p)
              buffer-read-only
              (null (buffer-modified-p)))
    (let ((l (save-excursion (beginning-of-defun 1) (point)))
          (r (save-excursion (end-of-defun 1) (point))))
      (cl-letf (((symbol-function 'message) #'ignore))
        (indent-region l r)))))

;; Next, create a hook that will call the indent-defun with every command call:

(defun activate-aggressive-indent ()
  "Locally add `ha/indent-defun' to `post-command-hook'."
  (add-hook 'post-command-hook
            'indent-defun nil 'local))

;; Auto Complete

(use-package company-c-headers
       :ensure t
       )

     (use-package company-math 
       :ensure t
       )

     (use-package company-shell 
       :ensure t
       )

     (use-package company
       :ensure t
       :init
       (setq company-dabbrev-ignore-case t
             company-show-numbers t)
       (add-hook 'after-init-hook 'global-company-mode)
       :config
       (setq company-idle-delay 0)
       (setq company-minimum-prefix-length 3)

       (add-to-list 'company-backends 'company-math-symbols-unicode)
       (add-to-list 'company-backends 'company-c-headers)
       (add-to-list 'company-backends 'company-shell)
       :bind ("C-:" . company-complete)  ; In case I don't want to wait
       :diminish company-mode)

     (use-package company-quickhelp
       :ensure t
       :config
       (company-quickhelp-mode 1))

     (add-hook 'c-mode-hook 'company-mode)

     (use-package company-statistics
       :ensure t 
       )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

;; hippie expand

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line))

;; Yasnippet

(use-package yasnippet
   :ensure t
   :init
   (yas-global-mode 1)
   :config
(use-package yasnippet-snippets
  :ensure t)
(yas-reload-all));

;; Comments

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

;; compile

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; ggtags

(use-package ggtags
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode)
                (ggtags-mode 1))))
  :config

                                        ; This must be set to the location of gtags (global)
  ;;(setq ggtags-executable-directory "~/global-6.5.6/bin/")
                                        ; Allow very large database files
  (setq ggtags-oversize-limit 104857600)
  (setq ggtags-sort-by-nearness t)
  (setq ggtags-use-idutils t)
  (setq ggtags-use-project-gtagsconf nil)

  :bind (
         ;;("M-," . gtags-pop-stack)
         ;; ("M-/" . ggtags-find-reference)
         ;;("M-]" . ggtags-idutils-query)

         :map ggtags-navigation-map
                                        ;Ergo
         ("M-u" . ggtags-navigation-previous-file)
         ("M-o" . ggtags-navigation-next-file)
         ("M-l" . ggtags-navigation-visible-mode)
         ("M-j" . ggtags-navigation-visible-mode)
         ("M-k" . next-error)
         ("M-i" . previous-error)
         ) ; end :bind
  )

;; Counsel-gtags

(use-package counsel-gtags
  :ensure t
  ;;:bind (
    ;;   ("M-t" . counsel-gtags-find-definition)
      ;; ("M-r" . counsel-gtags-find-reference)
       ;;("M-s" . counsel-gtags-find-symbol)
      ;; ("M-," . counsel-gtags-go-backward)
      ;; )
  :init 
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
)

;; Smartparens

(use-package smartparens
  :ensure t
  :init (add-hook 'c-mode-hook 'smartparens-mode))

;; SET BSD indent style

(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "bsd")
             ))

;; Don't ask with make command to run...

(setq compilation-read-command nil)

;; ESS (emacs speaks statistics... ).

(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Email

(require 'starttls)
(setq starttls-use-gnutls t)

(require 'smtpmail)
(setq send-mail-function  'smtpmail-send-it
      message-send-mail-function    'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials  '(("smtp.office365.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-smtp-server  "smtp.office365.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

;; Mu4e 

;;    On a mac install mu via brew:

;;    #+BEGIN_EXAMPLE sh
;;    brew install mu --with-emacs --HEAD
;;    #+END_EXAMPLE

;;    and make sure the path below points to the same =HEAD= directory!

(cond
      ((string-equal system-type "windows-nt") ; Microsoft Windows
       (progn
         (message "Microsoft Windows")))
      ((string-equal system-type "darwin") ; Mac OS X
       (progn
         (add-to-list 'load-path "/usr/local/Cellar/mu/HEAD-7d6c30f/share/emacs/site-lisp/mu/mu4e")
         (setq mu4e-mu-binary "/usr/local/bin/mu")
         ))
      ((string-equal system-type "gnu/linux") ; linux
       (progn
       ;;  (add-to-list 'load-path "~/programs/mu/mu4e")
;;          (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;;         (setq mu4e-mu-binary "/usr/local/bin/mu")
         )))

     ;;  (add-to-list 'load-path "~/programs/mu/mu4e")

     ;;         (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")   

     ;; the modules
     ;;(if (file-exists-p mu4e-mu-binary)
     ;; 

   (message "Loading Mu4e...")

       
       (if (not (require 'mu4e nil t))
       (message "`mu4e' not found")

       ;;(require 'mu4e)
       (require 'org-mu4e)


       (message "Loading Mu4e...")

       (setq mu4e-maildir (expand-file-name "~/Maildir"))

       (setq mu4e-sent-folder "/office365/sent")
       (setq mu4e-drafts-folder "/drafts")
       (setq mu4e-refile-folder "/office365/Archive")   ;; saved messages
       (setq mu4e-trash-folder "/office365/trash")


       (setq message-kill-buffer-on-exit t)
       (setq mu4e-change-filenames-when-moving t)
       (setq mu4e-confirm-quit nil)
       (setq mail-user-agent 'mu4e-user-agent)

       (setq mu4e-sent-messages-behavior 'sent)

       (setq mu4e-view-show-addresses t)

       (setq mu4e-attachment-dir "~/Downloads")


       (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
       (define-key mu4e-view-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)

       (setq mu4e-get-mail-command "offlineimap")

       (setq mu4e-compose-signature
                          "Associate Professor Timo Lassmann
Feilman Fellow
Academic Head of Computational Biology, Telethon Kids Institute
Adjunct Associate Professor, Center for Child Health Research
University of Western Australia

Telethon Kids Institute
100 Roberts Road, Subiaco, Western Australia, 6008
PO Box 855, West Perth, Western Australia, 6872

https://scholar.google.com.au/citations?user=7fZs_tEAAAAJ&hl=en

Visiting Scientist, RIKEN Yokohama Institute, Japan
Division of Genomic Technology,
RIKEN Center for Life Science Technologies,
Yokohama Institute,1-7-22 Suehiro-cho,
Tsurumi-ku, Yokohama, 230-0045 JAPAN")
)

;; Spell check

(add-hook 'mu4e-compose-mode-hook
          'flyspell-mode)
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; TRAMP

(use-package tramp
    :ensure t
    :config
    (with-eval-after-load 'tramp-cache
      (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
    (setq tramp-default-method "ssh")
    (setq tramp-use-ssh-controlmaster-options nil) 
    (message "tramp-loaded"))

;; Autoinsert templates 
  
;;   Again from Howards Abrams:

(use-package autoinsert
  :ensure t
  :init
  (setq auto-insert-directory (tl/emacs-subdirectory "templates/"))
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1))

;; Use yes snippet for templates.

(defun tl/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; Set templates

(use-package autoinsert 
  :config
  (define-auto-insert "\\.org$" ["default-orgmode.org" tl/autoinsert-yas-expand]))

;; Eshell 

;; Set up environment.

(setenv "LD_LIBRARY_PATH" "/usr/local/lib")

(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

(use-package eshell
  :init
  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t))

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "top"))))

;; Alias

(add-hook 'eshell-mode-hook (lambda ()
    (eshell/alias "e" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "ee" "find-file-other-window $1")

    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "d" "dired $1")

    ;; The 'ls' executable requires the Gnu version on the Mac
    (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                  "/usr/local/bin/gls"
                "/bin/ls")))
      (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

;; End


;;   Run client

(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f) (require 'init-client) ))))
  (require 'init-client) )

(require 'init-local nil t)

;; Fill Mode
;;    Automatically wrapping when you get to the end of a line (or the fill-region):

(use-package fill
  :bind (("C-c T f" . auto-fill-mode)
         ("C-c T t" . toggle-truncate-lines))
  :init (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :diminish auto-fill-mode)
