;;; 10_buffuer.el --- Improve buffuer view           -*- lexical-binding: t; -*-

(setq hl-line-face 'my/hl-line-face)

(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))

(use-package popwin
  :config
  (setq pop-up-windows nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom))

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; 使わないバッファを自動的に消す
(use-package tempbuf
  :config
  ;; ファイルを開いたら自動でtempbufを有効にする
  (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
  ;; diredバッファに対してtempbufを有効にする
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))
