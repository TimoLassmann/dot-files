;;; tl-cmode.el --- My C configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Timo Lassmann

;; Author: Timo Lassmann <timo.lassmann@telethonkids.org.au>
;; Keywords: c

(require 'ggtags)

(defun tl/setup-c-mode ()
  (setq c-default-style "bsd")
  (setq-default tab-width 4)

  (lambda ()
    (set (make-local-variable 'company-backends)
         '(company-clang  company-gtags  company-c-headers company-dabbrev )))
  (lambda ()
    (when (derived-mode-p 'c-mode)
      (ggtags-mode 1)))
  (lambda () (add-hook 'before-save-hook 'whitespace-cleanup))
  (lambda () (setq flycheck-clang-language-standard "c11"))
  ;; Modes I want 
  (ggtags-mode 1)
  (smartparens-mode 1)
  (hl-line-mode 1)
  (clean-aindent-mode 1)
  (dtrt-indent-mode 1)

  ;; Modes I don't want
  (org-indent-mode -1)
  (org-roam-mode -1))

(provide 'tl-cmode)
