;;; 03_myfn.el --- def my functions                  -*- lexical-binding: t; -*-

;;; 現在行のハイライト
(defface my/hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "Navy" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")

;; カーソル移動が重くなる原因に対処
;; http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))

(defun my/isearch-forward-or-swiper (use-swiper)
  (interactive "P")
  (let (current-prefix-arg)
    (call-interactively (if use-swiper 'swiper 'isearch-forward))))
