;;; 61_markdown.el --- For markdown                  -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'emmet-mode))
