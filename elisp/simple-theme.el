(deftheme simple
  "The basis for a my custom theme.")


(set-face-attribute 'default nil :height 140)

(defgroup simple-theme nil
  "Simple theme."
  :group 'faces
  :prefix "simple-"
  :link '(url-link :tag "GitHub" "NADA")
  :tag "Simple theme")


(defcustom simple-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'simple-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))


(defvar simple-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar simple-default-colors-alist
  '(
    ("bg" . "#000F14")
    ("fg" . "#FFAA00")

    ("a" . "#106590")
    ("a" . "#10adee")
    ("a" . "#105f89")
    ("a" . "#1078a2")
    ("a" . "#1083be")
    ("a" . "#1a8da7")
    ("a" . "#1a99e7")
    ("a" . "#1f5e8a")
    ("a" . "#1e8eb8")
    ("a" . "#1050a0")
    ("d" . "#103453")
    ("d" . "#103450"))
    "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")



(defmacro simple-with-color-variables (&rest body)
  "`let' bind all colors defined in `simple-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append simple-default-colors-alist
                           simple-override-colors-alist))
         (z-variable-pitch (if simple-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))


(simple-with-color-variables
  (custom-theme-set-variables
   'simple
   '(beacon-color "#A6341B"))

  (custom-theme-set-faces
   'simple
   '(default ((t (:foreground "#DCDCCC" :background "#000F14"))))
   '(fringe ((t (:foreground "#DCDCCC" :background "#000F14"))))

   '(vertical-border ((t (:foreground "#166755"))))

   '(minibuffer-prompt ((t (:foreground "#1a8da7"))))

   '(region ((t (:background "#3C3C3F"))))

   '(trailing-whitespace (( t (:background "#600D1A"))))
   ;;don't know what these do
   '(window-divider ((t(:foreground "#306630"))))
   '(window-divider-first-pixel ((t(:foreground "#106610"))))
   '(window-divider-last-pixel ((t(:foreground "#309930"))))


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
   '(font-lock-warning-face              ((t (:weight bold :foreground "#A6341B"))))
   '(font-lock-negation-char-face        ((t (:inherit font-lock-warning-face))))

   '(highlight ((t (:background "#002333"))))
   '(mode-line ((t (:background "#001520" :box nil :foreground "#DCDCCC"))))
   '(mode-line-inactive ((t (:weight light :box nil :background "#001520" :foreground "#000000" :inherit (mode-line)))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
   '(mode-line-buffer-id ((t (:weight bold :box nil))))

   '(link ((t (:foreground "#1e8eb8" :underline t))))
   '(link-visited ((t (:foreground "#105f89"))))

   '(flycheck-error   ((t (:underline (:style wave :color "#A6341B")))))
   '(flycheck-warning ((t (:underline (:style wave :color "#8A7811")))))

   '(eshell-prompt ((t (:weight bold  :foreground "#105f89"))))

   `(ivy-posframe ((t (:background "#151F23"))))
   )
  )



(provide-theme 'simple)

(provide 'simple-theme)
