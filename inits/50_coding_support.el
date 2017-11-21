;;; 50_coding_support.el --- For coding support libs                 -*- lexical-binding: t; -*-

;; ファイル作成時にテンプレートを挿入
(auto-insert-mode)
;; 次に指定したディレクトリをロードする (最後の/は必須)
(setq auto-insert-directory (concat user-emacs-directory "insert/"))
;; 次で"\\.rb$"の代わりに'ruby-modeにすると、メジャーモードがruby-modeのときに挿入してくれる
;;(define-auto-insert "\\.rb\\'" "ruby-template.rb")

;;; indentを基本spaceで
(setq-default indent-tabs-mode nil)

;; redoの設定
(use-package redo+
  :bind ("C-S-/" . redo)
  :config
  ;; 大量のredoに耐えられるようにする
  (setq undo-limit 600000
        undo-strong-limit 900000))

;; cua-modeの設定(矩形選択を可能に)
(when (cua-mode t)
  ;; CUAキーバインドを無効にする
  (setq cua-enable-cua-keys nil)
  ;; C-RETがC-jになるため、C-c C-SPCに矩形選択モードを割り当て
  (define-key global-map (kbd "C-c C-SPC") 'cua-set-rectangle-mark))

;; Completion
(use-package company
  :config
  ;; (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t)

  (bind-keys :map company-mode-map
             ("C-i" . company-complete))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;; hippie-expand
(bind-key "M-/" 'hippie-expand)
;; 補完候補探索順を指定
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))


;; 補完時に大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; undohistの設定
(use-package undohist
  :config
  (undohist-initialize))

;; undo-treeの設定
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; flycheck
(use-package flycheck)

;; highlight
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

(use-package highlight-symbol
  :bind
  ([(control f3)] . highlight-symbol-at-point)
  ([f3] . highlight-symbol-next)
  ([shift f3] . highlight-symbol-prev)
  ([(meta f3)] . highlight-symbol-query-replace))

(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S."))

;; バッファをコンパイル・実行
(use-package quickrun
  :config
  (push '("*quickrun*") popwin:special-display-config)
  :bind
  ("C-c C-q" . quickrun)
  ("C-c q" . quickrun-with-arg))

;; ctags
;; 注意！exuberant-ctagsを指定する必要がある
;; Emacs標準のctagsでは動作しない！！
(setq ctags-update-command "/usr/bin/ctags")
;; 使う言語で有効にしよう

(use-package subword)

(bind-key "C-S-k" 'just-one-space)

(use-package paredit
  :bind
  ("C-j" . paredit-newline))

(defun my/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun my/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
  Does not indent buffer, because it is used for a before-save-hook, and that
  might be bad."
  (interactive)
  (my/untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
  Including my/indent-buffer, which should not be called automatically on save."
  (interactive)
  (my/cleanup-buffer-safe)
  (my/indent-buffer))

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

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  ("C-'" . yas-expand))
