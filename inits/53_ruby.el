;;; 53_ruby.el ---                                   -*- lexical-binding: t; -*-

(defun my/ruby-mode-hooks ()
  "ruby mode hook"
  (interactive "P")
  (smart-newline-mode t)
  (setq tab-width 2
        indent-tabs-mode nil
        ruby-indent-level tab-width))

(use-package ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode)
  ("Capfile\\'" . ruby-mode)
  ("Gemfile\\'" . ruby-mode)
  :interpreter
  ("ruby" . ruby-mode)
  :init
  (add-hook 'ruby-mode-hook 'my/ruby-mode-hooks)
  :config
  (use-package ruby-end)
  (use-package ruby-block
    :config
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  (use-package inf-ruby)
  (use-package rbenv
    :config
    (global-rbenv-mode)
    (setq rbenv-installation-dir "~/.rbenv/bin/rbenv"))
  (bind-keys :map ruby-mode-map
             ("|" . my/vertical-bar-pair)))
