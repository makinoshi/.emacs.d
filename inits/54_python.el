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

(use-package python-mode
  :mode
  ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'my/python-mode-hooks)
  :config
  (elpy-enable)
  (add-hook 'before-save-hook 'elpy-format-code))
