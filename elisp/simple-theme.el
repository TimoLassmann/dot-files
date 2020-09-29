(deftheme simple
  "The basis for a my custom theme.")

(set-face-attribute 'default nil :height 140)

(defgroup simple-theme nil
  "Simple theme."
  :group 'faces
  :prefix "simple-"
  :link '(url-link :tag "GitHub" "NADA")
  :tag "Simple theme")

(defcustom zenburn-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar zenburn-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")


(defvar zenburn-default-colors-alist
  '(("bg"               . "#000F14")
    ("fg"               . "#DCDCCC")
    ("alert-main"       . "#A6341B")
    ("alert-second"     . "#2AA889")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-fg-05"    . "#989890")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg+2"     . "#FFFFFD")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-08"    . "#303030")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburn-default-colors-alist
                           zenburn-override-colors-alist))
         (z-variable-pitch (if zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))


(zenburn-with-color-variables
  (custom-theme-set-faces
   'simple
   `(default ((t (:foreground ,fg :background ,bg))))
   `(error ((t (:foreground ,alert-main))))
   `(success ((t (:foreground ,alert-second))))
   `(warning ((t (:foreground ,alert-second))))
   `(shadow ((t (:foreground ,alert-second))))
   '(fringe ((t (:foreground "#DCDCCC" :background "#000F14"))))

   '(vertical-border ((t (:foreground "#1a99e7")))) ;;"#166755"))))

   '(minibuffer-prompt ((t (:foreground "#1a8da7"))))

   '(region ((t (:background "#3C3C3F"))))

   '(trailing-whitespace (( t (:background "#600D1A"))))
   ;;don't know what these do
   '(window-divider ((t(:foreground "#1a99e7"))))
   '(window-divider-first-pixel ((t(:foreground "#1a99e7"))))
   '(window-divider-last-pixel ((t(:foreground "#1a99e7"))))

   `(compilation-mode-line-exit  ((t (:foreground ,alert-second))))
   `(compilation-mode-line-fail  ((t (:foreground ,alert-main))))
   `(compilation-mode-line-run ((t (:foreground ,alert-second))))


   '(magit-branch-local ((t (:foreground "#10adee"))))
   '(magit-tag          ((t (:foreground "#2aa889"))))
   '(magit-branch-current ((t (:foreground "#1a8da7"))))
   '(magit-branch-remote    ((t (:foreground "#1083be"))))
   '(magit-section-heading ((t (:foreground "#10adee"))))
   '(magit-section-highlight ((t (:background  "#002E41"))))
   '(magit-diff-hunk-heading ((t (:foreground "#008ED1" :background "#002E41" :extend t))))
   '(magit-diff-context-highlight ((t (:background "#122022" :extend t))))


   '(font-lock-builtin-face              ((t (:foreground "#106590"))))
   '(font-lock-comment-face              ((t (:foreground "#105f89" :italic t ))))
   '(font-lock-comment-delimiter-face    ((t (:foreground "#1078a2" :italic t ))))
   '(font-lock-doc-face                  ((t (:inherit (font-lock-string-face)))))
   '(font-lock-function-name-face        ((t (:foreground "#1083be"))))
   '(font-lock-keyword-face              ((t (:foreground "#1a8da7"))))
   '(font-lock-preprocessor-face         ((t (:foreground "#2aa889"))))
   '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   '(font-lock-string-face               ((t (:foreground "#10adee"))))
   '(font-lock-constant-face             ((t (:foreground "#1a99e7"))))
   '(font-lock-type-face                 ((t (:foreground "#1f5e8a"))))
   '(font-lock-variable-name-face        ((t (:foreground "#1e8eb8"))))
   `(font-lock-warning-face              ((t (:weight bold :foreground ,alert-main))))
   '(font-lock-negation-char-face        ((t (:inherit font-lock-warning-face))))

   '(highlight ((t (:background "#002333"))))
   '(mode-line ((t (:background "#001520" :box nil  :box "#1a99e7"))))
   '(mode-line-inactive ((t (:weight light :box nil :background "#001520" :foreground "#105f89" :inherit (mode-line)))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
   '(mode-line-buffer-id ((t (:weight bold :foreground "#1a99e7"))))

   '(link ((t (:foreground "#1e8eb8" :underline t))))
   '(link-visited ((t (:foreground "#105f89"))))

   `(flycheck-error   ((t (:underline (:style wave :color ,alert-main)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,alert-second)))))

   '(eshell-prompt ((t (:weight bold  :foreground  "#105f89"))))
   '(eshell-ls-executable ((t (:foreground "#2aa889"))))
   '(eshell-ls-archive ((t (:foreground "#AAAAAA"))))
   '(eshell-ls-backup ((t (:foreground "#1f5e8a" :weight bold))))
   ;; '(eshell-ls-clutter ((t (:foreground base5))))
   '(eshell-ls-directory ((t (:foreground "#1a99e7" :weight bold))))

   ;; '(eshell-ls-missing ((t (:foreground red :weight bold))))
   '(eshell-ls-product ((t (:foreground "#105f89"))))
   ;; '(eshell-ls-readonly ((t (:foreground red))))
   ;; '(eshell-ls-special ((t (:foreground orange :weight bold))))
   ;; '(eshell-ls-symlink ((t (:foreground blue :weight bold))))
   ;; '(eshell-ls-unreadable ((t (:foreground red))))


   `(ivy-current-match ((t (:background "#002E41" :foreground ,alert-second))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,alert-main ))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,alert-main ))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,alert-main ))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,alert-main ))))
   `(ivy-remote ((t (:foreground ,alert-main))))
   '(ivy-grep-info ((t (:foreground "#008ED1"))))
   '(ivy-posframe ((t (:background "#151F23"))))
   `(avy-lead-face   ((t (:weight bold :background "#999999" :foreground ,alert-main))))
   `(avy-lead-face-0 ((t (:weight bold :background "#999999" :foreground ,alert-main))))
   `(avy-lead-face-1 ((t (:weight bold :background "#999999" :foreground ,alert-main))))
   `(avy-lead-face-2 ((t (:weight bold :background "#999999" :foreground ,alert-main))))


   '(company-preview ((t (:background "#002E41" :foreground "#008ED1" :underline t))))
   '(company-preview-common ((t (:inherit company-preview))))
   '(company-preview-search ((t (:inherit company-preview))))

   '(company-tooltip ((t (:background "#151F23" :foreground "#008ED1"))))
   `(company-tooltip-selection ((t (:background "#002E41" :foreground ,alert-second))))
   `(company-tooltip-annotation ((t (:background "#151F23" :foreground ,alert-second))))
   `(company-tooltip-annotation-selection ((t (:background "#002E41" :foreground ,alert-main))))
   `(company-tooltip-common ((t (:background "#151F23" :foreground ,alert-main))))
   `(company-tooltip-common-selection  ((t (:background "#002E41" :foreground ,alert-main))))

   `(company-template-field ((t (:background "#002E41" :foreground ,alert-main))))
   '(company-scrollbar-fg ((t (:background "#008ED1"))))
   '(company-scrollbar-bg ((t (:background "#151F23"))))

   '(compilation-info ((t (:foreground "#008ED1" :weight bold))))
   '(org-block-begin-line ((t (:foreground "#008ED1" :background "#002E41" :extend t))))
   '(org-block-background ((t (:background "#000000" :extend t))))
   '(org-block            ((t (:background "#122022" :extend t))))
   '(org-block-end-line   ((t (:foreground "#008ED1" :background "#002E41" :extend t))))
   '(org-level-1 ((t (:bold t :height 1.3  :foreground "#1083be" ))))
   '(org-level-2 ((t (:bold t :height 1.2  :foreground "#1078a2" ))))
   '(org-level-3 ((t (:bold nil :height 1.1  :foreground "#106590"  ))))
   '(org-level-4 ((t (:bold nil :height 1.0  :foreground "#105f89" ))))
   `(org-todo ((t (:weight bold  :foreground ,alert-main))))
   `(org-done ((t (:weight bold  :foreground ,alert-second)))))

   '(mode-line ((t (:foreground "#599cab" :background "#091f2e" :box nil))))
   '(mode-line-inactive ((t (:foreground "#245361" :background  "#11151c" :box nil))))
   '(mode-line-highlight ((t (:foreground  "#99d1ce"))))
   '(mode-line-buffer-id ((t (:weight bold))))



   ;; '(powerline-active1 ((t (:foreground "#599cab" :background "#091f2e"))))
   ;; '(powerline-active2 ((t (:foreground "#599cab" :background "#0a3749"))))
   ;; '(powerline-inactive1 ((t (:foreground  "#245361" :background "#11151c"))))
   ;; '(powerline-inactive2 ((t (:foreground  "#245361" :background "#091f2e"))))

   '(query-replace ((t (:inherit highlight))))
   )



(provide-theme 'simple)

(provide 'simple-theme)


  ;; (custom-theme-set-variables
  ;;  'simple
  ;;  '(beacon-color ((t ,alert-main ))
