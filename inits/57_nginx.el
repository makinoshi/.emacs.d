;;; 57_nginx.el --- For nginx                        -*- lexical-binding: t; -*-

;;; Code:
(defun my/nginx-mode-hooks ()
  "For nginx.conf."
  (interactive "P")
  (setq nginx-indent-level 4
        nginx-indent-tabs-mode nil
        tab-width 4)
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
  (smart-newline-mode t))

(use-package nginx-mode
  :init
  (add-hook 'nginx-mode 'my/nginx-mode-hooks))
