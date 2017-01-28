;;; 50_coding_support.el --- For coding support libs                 -*- lexical-binding: t; -*-

(use-package smart-newline
  :bind
  ("C-m" . smart-newline))

(defadvice smart-newline (around C-u activate)
  "C-uを押したら元のC-mの挙動をするようにした。org-modeなどで活用。
  http://emacs.rubikitch.com/smart-newline/"
  (if (not current-prefix-arg)
      ad-do-it
    (let (current-prefix-arg)
      (let (smart-newline-mode)
        (call-interactively (key-binding (kbd "C-m")))))))

;; キーボード同時押しでコマンドを実行する
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode 1))

(use-package key-combo)

;; 「スペースから始まるkey-chord」を定義するEmacs Lisp
(use-package space-chord
  :init
  (unless (require 'space-chord nil t)
    (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/space-chord.el"))
  :config
  (setq space-chord-delay 0.08))

;; 括弧の自動挿入の挙動をオレオレ設定できるflex-autopair.el
(use-package flex-autopair
  :config
  (flex-autopair-mode 1))

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
