;;; linux-base.el --- linux config                 -*- lexical-binding: t; -*-

(when (eq system-type 'gnu/linux)
  ;; input method
  (use-package mozc
    :config
    (set-language-environment "UTF-8")
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay))
  ;; font
  (set-frame-font "ricty-11"))
