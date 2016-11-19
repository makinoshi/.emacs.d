;;; cocoa-emacs.el --- for mac                -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (global-set-key (kbd "C-M-¥") 'indent-region)
  ;; デフォルトの文字コード
  (set-default-coding-systems 'utf-8-unix)
  ;; テキストファイル／新規バッファの文字コード
  (prefer-coding-system 'utf-8-unix)
  ;; ファイル名の文字コード
  (use-package ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  ;; キーボード入力の文字コード
  (set-keyboard-coding-system 'utf-8-unix)
  ;; サブプロセスのデフォルト文字コード
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))
  ;; font
  (let* ((size 15)
         (asciifont "Ricty")
         (jpfont "Ricty")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))
