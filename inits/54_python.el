;;; 54_python.el --- For python                      -*- lexical-binding: t; -*-

(defun my/python-mode-hooks ()
  (setq indent-tabs-mode nil
        indent-level 4
        python-indent 4
        python-pylint t
        tab-width 4)
  (smart-newline-mode t)
  (use-package jedi-core
    :config
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t)
    (add-hook 'python-mode-hook 'jedi:setup))
  (add-to-list 'company-backends 'company-jedi)
  (use-package py-autopep8
    :config
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)))

(use-package python-mode
  :mode
  ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'my/python-mode-hooks))
