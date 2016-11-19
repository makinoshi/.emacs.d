;;; 00_base.el --- emacs default config        -*- lexical-binding: t; -*-

;;; Emacsからの質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;; 新しい方を見る
(setq load-prefer-newer t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - frame                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Frame
(setq default-frame-alist
      (append '(
                ;; (width                . 204) ; フレーム幅
                (height               . 60 ) ; フレーム高
                ;;              (left                 . 70 ) ; 配置左位置
                ;;              (top                  . 28 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 5  ) ; 左フリンジ幅
                (right-fringe         . 5  ) ; 右フリンジ幅
                ;;                (menu-bar-lines       . 1  ) ; メニューバー
                ;;                (tool-bar-lines       . 1  ) ; ツールバー
                ;;                (vertical-scroll-bars . 1  ) ; スクロールバー
                ;;                (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . bar) ; カーソル種別
                (alpha                . 90 ) ; 透明度
                ) default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; ターミナル以外はツールバー、スクロールバーを非表示
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; メニューバーを非表示
(menu-bar-mode 0)

;; 行番号の表示
(line-number-mode t)
;; 列番号の表示
(column-number-mode t)
;; ファイルサイズを表示
(size-indication-mode t)
;; 時計を表示（好みに応じてフォーマットを変更可能）
(setq display-time-day-and-date t) ; 曜日・月・日を表示
(setq display-time-24hr-format t)  ; 24時表示
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
;; http://d.hatena.ne.jp/sonota88/20110224/1298557375
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ;; これだとエコーエリアがチラつく
    ;;(count-lines-region (region-beginning) (region-end))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; file名の補完で大文字小文字の区別をしない
(setq completion-ignore-case t)

;; 他でファイルが編集されたときに、bufferを再読み込みさせる
(global-auto-revert-mode 1)

;;; ファイル名補完キーマップで?をそのまま入力できるようにする
(define-key minibuffer-local-filename-completion-map (kbd "?") nil)
;;; ffapでワイルドカードを指定するとdiredを開くようにする
(setq ffap-pass-wildcards-to-dired t)
;;; C-x C-fなどをffap関係のコマンドに割り当てる
(ffap-bindings)

(global-hl-line-mode t)

;; 括弧の対応関係のハイライト
;; paren-mode：対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; カーソルの点滅
(blink-cursor-mode 0)
;; 非アクティブウィンドウのカーソル表示
(setq-default cursor-in-non-selected-windows t)
;; 論理行 (画面上の改行)単位ではなく物理行 (改行文字まで)単位で移動する
(setq line-move-visual nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search t)
