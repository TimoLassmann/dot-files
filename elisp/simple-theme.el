(deftheme simple
  "The basis for a my custom theme.")


(set-face-attribute 'default nil :height 140)

(defgroup simple-theme nil
  "Simple theme."
  :group 'faces
  :prefix "simple-"
  :link '(url-link :tag "GitHub" "NADA")
  :tag "Simple theme")


  (let ((class '((class color) (min-colors 89)))
        (bg "#000F14")
        (fg "#DCDCCC")
        (alert-main "#A6341B")
        (alert-second "#8A7811")
        (blue "#000077"))

  (custom-theme-set-variables
   'simple
   `(beacon-color ,alert-main ))

  (custom-theme-set-faces
   'simple
   `(default ((,class :foreground ,fg :background ,bg)))
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
   `(font-lock-warning-face              ((,class :weight bold :foreground ,alert-main)))
   '(font-lock-negation-char-face        ((t (:inherit font-lock-warning-face))))

   '(highlight ((t (:background "#002333"))))
   '(mode-line ((t (:background "#001520" :box nil :foreground "#DCDCCC"))))
   '(mode-line-inactive ((t (:weight light :box nil :background "#001520" :foreground "#000000" :inherit (mode-line)))))
   '(mode-line-emphasis ((t (:weight bold))))
   '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
   '(mode-line-buffer-id ((t (:weight bold :box nil))))

   '(link ((t (:foreground "#1e8eb8" :underline t))))
   '(link-visited ((t (:foreground "#105f89"))))

   `(flycheck-error   ((,class :underline (:style wave :color ,alert-main))))
   `(flycheck-warning ((,class :underline (:style wave :color ,alert-second))))

   '(eshell-prompt ((t (:weight bold  :foreground "#105f89"))))

   '(ivy-posframe ((t (:background "#151F23"))))

   `(avy-lead-face   ((,class :weight bold :background "#999999" :foreground ,alert-main)))
   `(avy-lead-face-0 ((,class :weight bold :background "#999999" :foreground ,alert-main)))
   `(avy-lead-face-1 ((,class :weight bold :background "#999999" :foreground ,alert-main)))
   `(avy-lead-face-2 ((,class :weight bold :background "#999999" :foreground ,alert-main)))


   '(company-preview ((t (:background "#002E41" :foreground "#008ED1" :underline t))))
   '(company-preview-common ((t (:inherit company-preview))))
   '(company-preview-search ((t (:inherit company-preview))))

   '(company-tooltip ((t (:background "#151F23" :foreground "#008ED1"))))
   `(company-tooltip-selection ((,class :background "#002E41" :foreground ,alert-second)))
   `(company-tooltip-annotation ((,class :background "#151F23" :foreground ,alert-second)))
   `(company-tooltip-annotation-selection ((,class :background "#002E41" :foreground ,alert-main)))
   `(company-tooltip-common ((,class :background "#151F23" :foreground ,alert-main)))
   `(company-tooltip-common-selection  ((,class :background "#002E41" :foreground ,alert-main)))

   `(company-template-field ((,class :background "#002E41" :foreground ,alert-main)))
   '(company-scrollbar-fg ((t (:background "#008ED1"))))
   '(company-scrollbar-bg ((t (:background "#151F23"))))

   )
  )

(provide-theme 'simple)

(provide 'simple-theme)
