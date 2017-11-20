;;; 51_emacs_lisp.el --- For elisp                   -*- lexical-binding: t; -*-

(defun emacs-lisp-mode-hooks ()
  "For emacslisp coding"
  (interactive "P")
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t))

(use-package emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hooks))
