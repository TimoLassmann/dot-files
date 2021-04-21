;;; package --- My font setting
;;; Comentary: Just

(defun tl/setup-fonts ()
  "Setup my fonts."
  (interactive)
  ;; Main typeface
  (set-face-attribute 'default nil :family "Iosevka extended" :height 140)
  ;; Proportionately spaced typeface
  (set-face-attribute 'variable-pitch nil :family "LinuxLibertine O" :height 1.5)
  ;; Monospaced typeface
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed" :height 1.0)

  (dolist (face '(mode-line mode-line-inactive minibuffer-prompt))
    (set-face-attribute face nil :family "Iosevka term" :height 160))
  )


(provide 'tl-fonts)
;;; prot-diff.el ends here
