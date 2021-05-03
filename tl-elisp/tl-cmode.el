;;; tl-cmode.el --- My C configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Timo Lassmann

;; Author: Timo Lassmann <timo.lassmann@telethonkids.org.au>
;; Keywords: c

(require 'ggtags)

(defun tl/setup-c-mode ()
  (setq c-default-style "bsd")
  (setq-default tab-width 4)

  (setq flycheck-clang-language-standard "c11")
  ;; Modes I want
  ;;(ggtags-mode 1)
  (setq-local company-backends
              (append '((company-clang  company-gtags  company-c-headers company-dabbrev))))


  ;; (smartparens-mode 1)
  ;; (hl-line-mode 1)


  ;; (org-indent-mode -1)
  ;; (org-roam-mode -1)
  )

(provide 'tl-cmode)
