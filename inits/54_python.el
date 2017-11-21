;;; 54_python.el --- For python                      -*- lexical-binding: t; -*-

(defun set-pyenv-version-path ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

(defun my/python-mode-hooks ()
  (setq indent-tabs-mode nil
        indent-level 4
        python-indent 4
        python-pylint t
        tab-width 4)
  (smart-newline-mode t)
  (use-package pyenv-mode
    :config
    (add-hook 'find-file-hook 'set-pyenv-version-path)))

(defun my/python-cleanup ()
  (interactive)
  (elpy-importmagic-fixup)
  (elpy-format-code))

(defun my/elpy-init ()
  (interactive)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'before-save-hook 'my/python-cleanup)
  (elpy-enable)
  (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi"))

(use-package python-mode
  :mode
  ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'my/python-mode-hooks)
  :config
  (my/elpy-init))
