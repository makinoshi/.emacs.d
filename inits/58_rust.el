;;; Add path to racer and rustfmt
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(defun my/rust-mode-hook ()
  (racer-mode)
  (flycheck-rust-setup))

(defun my/racer-mode-hook ()
  (company-mode)
  (set (make-variable-buffer-local 'company-idle-delay) 0.1)
  (set (make-variable-buffer-local 'company-minimum-prefix-length) 0))

(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-mode)
  :init
  (add-hook 'rust-mode-hook 'my/rust-mode-hook)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook 'my/racer-mode-hook)
  :config
  (setq-default rust-format-on-save t))
