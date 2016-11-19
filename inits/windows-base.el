;;; windows_base.el --- windows config                    -*- lexical-binding: t; -*-

(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)
  (set-frame-font "consolas"))
