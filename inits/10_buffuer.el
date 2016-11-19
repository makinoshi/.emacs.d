;;; 10_buffuer.el --- Improve buffuer view           -*- lexical-binding: t; -*-

(setq hl-line-face 'my/hl-line-face)

(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
