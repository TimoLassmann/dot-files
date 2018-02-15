;; Display Settings

(setq ring-bell-function 'ignore);; turn everything offf

(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (menu-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))            ;; Scrollbars are waste screen estate

;; Set default font size..

(set-face-attribute 'default nil :height 140)

;; I love syntax highlighting.


(global-font-lock-mode 1)

;; I think so. I keep changing my
;;   font based on the monospace du jour... While I [[http://mplus-fonts.sourceforge.jp/mplus-outline-fonts/download/index.html][M+]] because it is
;;   thinner and has more white space between lines, but [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][Source Code Pro]]
;;   is so attractive, oh, and then there is Anonymous Pro...

;;   While thicker, [[https://github.com/tonsky/FiraCode][Fira]] does symbol ligatures. However, [[https://github.com/i-tu/Hasklig][Hasklig]] is a
;;   nice font that is thinner and easier to read, with /some/ symbolic
;;   ligatures that doesn't interfere with my org-mode header bullets.


(defvar tl/fixed-font-family
  (cond ((x-list-fonts "Hasklig")         "Hasklig")
        ((x-list-fonts "Source Code Pro") "Source Code Pro")
        ((x-list-fonts "Anonymous Pro")   "Anonymous Pro")
        ((x-list-fonts "M+ 1mn")          "M+ 1mn"))
  "My fixed width font based on what is installed, `nil' if not defined.")

;; Since the headers are based on Adobe’s open source font pair of the
;;   proportional font, [[https://github.com/adobe-fonts/source-sans-pro/releases/tag/2.010R-ro/1.065R-it][Source Sans Pro]], will match the non-proportional
;;   font, [[https://github.com/adobe-fonts/source-code-pro/][Source Code Pro]].


(defvar tl/variable-font-tuple
  (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
        ((x-list-fonts "Verdana")         '(:font "Verdana"))
        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))
  "My variable width font available to org-mode files and whatnot.")

;; Resizing

;;    According to [[http://emacsninja.com/posts/making-emacs-more-presentable.html][this article]], we can update the =text-scale-XYZ=
;;    functions to work on a frame instead of just a buffer:


(defun tl/text-scale-frame-change (fn)
  (let* ((current-font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name current-font-name))
         (font-size (string-to-number (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (int-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun tl/text-scale-frame-increase ()
  (interactive)
  (tl/text-scale-frame-change '1+))

(defun tl/text-scale-frame-decrease ()
  (interactive)
  (tl/text-scale-frame-change '1-))

(global-set-key (kbd "C-+") 'tl/text-scale-frame-increase)
(global-set-key (kbd "C-=") 'tl/text-scale-frame-increase)
(global-set-key (kbd "C--") 'tl/text-scale-frame-decrease)

;; Color Theme
  
;;   Use the color theme project by following [[http://www.nongnu.org/color-theme/][these instructions]].
;;   We now can do =M-x color-theme-<TAB> RET=


(use-package gotham-theme
      :ensure t
      )

(set-face-attribute 'ido-virtual  nil :foreground "#00003f")


  
;; add some missing fonts.. 


(defun tl-color-scheme-additions ()
         "Some extra colors I added..."
         (interactive)
         (custom-set-faces
          '(ido-virtual
            ((t (:foreground "#008ED1" :background "#000000"))))))



;; Make dark blocks... 


(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#000000"))))
   '(org-block
     ((t (:background "#122028"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41"))))))



;; New theme.


(deftheme tl/org-theme "Sub-theme to beautify org mode")

(defun tl/change-theme (theme org-block-style additional)
  "Changes the color scheme and reset the mode line."
  (load-theme theme t)
  (funcall org-block-style)
  (funcall additional)

  (let* ((tl/fixed-font-tuple (list :font tl/fixed-font-family))
                                    ; (ha/varible-font-tuple (list :font ha/variable-font-family))
         (base-font-color     (face-foreground 'default nil 'default))
         (background-color    (face-background 'default nil 'default))
         (primary-color       (face-foreground 'mode-line nil))
         (secondary-color     (face-background 'secondary-selection nil 'region))
         (base-height         (face-attribute 'default :height))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (when tl/fixed-font-family
      (set-frame-font tl/fixed-font-family)
      (set-face-attribute 'default nil :font tl/fixed-font-family :height 140)
      (set-face-font 'default tl/fixed-font-family))


    (custom-theme-set-faces 'tl/org-theme
                            `(org-agenda-structure ((t (:inherit default :height 2.0 :underline nil))))
                            `(org-verbatim ((t (:inherit 'fixed-pitched :foreground "#aef"))))
                            `(org-table ((t (:inherit 'fixed-pitched))))
                            `(org-block ((t (:inherit 'fixed-pitched))))
                            `(org-block-background ((t (:inherit 'fixed-pitched))))
                            `(org-block-begin-line ((t (:inherit 'fixed-pitched))))
                            `(org-block-end-line ((t (:inherit 'fixed-pitched))))
                            `(org-level-8 ((t (,@headline ,@tl/variable-font-tuple))))
                            `(org-level-7 ((t (,@headline ,@tl/variable-font-tuple))))
                            `(org-level-6 ((t (,@headline ,@tl/variable-font-tuple))))
                            `(org-level-5 ((t (,@headline ,@tl/variable-font-tuple))))
                            `(org-level-4 ((t (,@headline ,@tl/variable-font-tuple
                                                          :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@tl/variable-font-tuple
                                                          :height 1.1))))
                            `(org-level-2 ((t (,@headline ,@tl/variable-font-tuple
                                                          :height 1.1))))
                            `(org-level-1 ((t (,@headline ,@tl/variable-font-tuple
                                                          :height 2.1))))
                            `(org-document-title ((t (,@headline ,@tl/variable-font-tuple :height 1.5 :underline nil)))))))



;; And the default startup goes to…night…


(tl/change-theme 'gotham 'org-src-color-blocks-dark 'tl-color-scheme-additions)

(provide 'init-client)
