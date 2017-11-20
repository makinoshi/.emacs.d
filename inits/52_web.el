;;; 52_web.el --- For Web(HTML/CSS/JS)                   -*- lexical-binding: t; -*-

(use-package emmet-mode
  :config
  ;; C-j は newline のままにしておく
  (eval-after-load "emmet-mode" '(define-key emmet-mode-keymap (kbd "C-j") nil))
  ;;C-i と Tabの被りを回避
  (keyboard-translate ?\C-i ?\H-i)
  ;; C-i で展開
  (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line)
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t))

(defun my/web-mode-hooks ()
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-comment-style 2
        web-mode-html-offset 2
        web-mode-css-offset 2
        web-mode-script-offset 2)
  (when (equal web-mode-content-type "jsx")
    (add-to-list 'web-mode-comment-formats '("jsx" . "// " ))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-mode t))
  (smart-newline-mode t)
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t))

(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.jsx?\\'" . web-mode)
  ("\\.css?\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook 'company-mode)
  ;; (add-hook 'web-mode-hook 'smartchr-keybindings-web)
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'web-mode-hook 'emmet-mode) ;; web-modeで使う
  (add-hook 'web-mode-hook 'my/web-mode-hooks))

(defun scss-mode-hooks ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil))
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
  (smart-newline-mode t))

(use-package scss-mode
  :mode
  ("\\.scss\\'" . scss-mode)
  :init
  (add-hook 'scss-mode-hook 'company-mode)
  (add-hook 'scss-mode-hook 'scss-mode-hooks)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'emmet-mode))
