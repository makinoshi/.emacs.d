;;; Lua lang

(defun my/lua-mode-hooks ()
  (interactive)
  (setq indent-tabs-mode nil
        indent-level 2)
  (use-package company-lua
    :config
    (add-to-list 'company-backends 'company-lua)))

(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode)
  :init
  (add-hook 'lua-mode-hook 'company-mode)
  (add-hook 'lua-mode-hook 'my/lua-mode-hooks))
