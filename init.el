;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Cask                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (and (not (equal window-system 'w32))
           (or (require 'cask nil t) 	; for MacOS X (homebrew)
               (require 'cask "~/.cask/cask.el" t))) ;for Linux (install by curl)
  (cask-initialize)
  (require 'pallet))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ el-get                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (when load-file-name
;;   (setq user-emacs-directory (file-name-directory load-file-name)))

;; ;; enabling el-get (install el-get from GitHub if not installed)
;; (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (load "~/.emacs.d/package-bundle")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ PATH                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  "引数に与えたパスをロードする"
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" ".cask/24.4.1/elpa")
;; (add-to-load-path "elisp")

;; include PATH from Shell
(exec-path-from-shell-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Emacs Lisp conding                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; Emacs Lisp ライブラリ
(require 'cl)
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'paredit)
(require 'slime)
(require 'bind-key)
(require 'use-package)

;; use-packageがない場合何もしない
(unless (require 'use-package nil t)
  (defmacro use-package (&reset args)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Package control                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; auto-installの設定
(use-package auto-install
  :config
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory (concat user-emacs-directory "elisp/"))
  ;; EmacsWikiに登録されているelisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; package管理の設定
(use-package package
  :config
  ;; パッケージリポジトリにMarmaladeを追加
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA"	. "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade"	. "http://marmalade-repo.org/packages/"))
  ;; インストールしたパッケージにロードパスを通してロードする
  (package-initialize))

;; Emacsからの質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;; 起動時にバイトコンパイルする(emacsは古くても.elcファイルを優先的にロードするため)
(use-package auto-async-byte-compile
  :init
  (unless (require 'auto-async-byte-compile nil t)
    (install-elisp-from-emacswiki "auto-async-byte-compile.el"))
  :config
  ;; 自動コンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)
;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ only for mac                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (global-set-key (kbd "C-M-¥") 'indent-region)
  ;; ファイル名の文字コード
  (use-package ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  ;; キーボード入力の文字コード
  (set-keyboard-coding-system 'utf-8-unix)
  ;; サブプロセスのデフォルト文字コード
  (setq default-process-coding-system '(undecided-dos . utf-8-unix)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ only for ubuntu                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (eq system-type 'gnu/linux)
  ;; input method
  (use-package mozc
    :config
    (set-language-environment "UTF-8")
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay))
  ;; font
  (set-frame-font "ricty-11"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ only for windows                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)
  (set-frame-font "consolas"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - frame                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq default-frame-alist
      (append '((width                . 204)  ; フレーム幅
                (height               . 60 ) ; フレーム高
                ;;		(left                 . 70 ) ; 配置左位置
                ;;		(top                  . 28 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 5  ) ; 左フリンジ幅
                (right-fringe	      .	5  ) ; 右フリンジ幅
                ;;                (menu-bar-lines       . 1  ) ; メニューバー
                ;;                (tool-bar-lines       . 1  ) ; ツールバー
                ;;                (vertical-scroll-bars . 1  ) ; スクロールバー
                ;;                (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . bar) ; カーソル種別
                (alpha                . 90 ) ; 透明度
                ) default-frame-alist) )
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

;; popwin
(use-package popwin
  :init
  (setq pop-up-windows nil)
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom))

;; e2wm(IDE likeなフレームを提供)
(use-package e2wm
  :config
  (use-package e2wm-bookmark)
  (autoload 'e2wm:start-management "e2wm-vcs" "load e2wm-vcs" t)
  (autoload 'e2wm:dp-vcs "e2wm-vcs" "load e2wm-vcs" t)
  (autoload 'e2wm:start-management "e2wm-bookmark" "load e2wm-bookmark" t)
  (use-package e2wm-R)
  :bind
  ("M-+" . e2wm:start-management)
  ("C-c R" . e2wm:start-R-code))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

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

;; 使わないバッファを自動的に消す
(use-package tempbuf
  :config
  ;; ファイルを開いたら自動でtempbufを有効にする
  (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
  ;; diredバッファに対してtempbufを有効にする
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode))

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
(setq hl-line-face 'my/hl-line-face)

;; カーソル移動が重くなる原因に対処
;; http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(global-hl-line-mode t)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
(global-hl-line-mode 0)
;; (cancel-timer global-hl-line-timer)

;; 括弧の対応関係のハイライト
;; paren-mode：対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; 行の折り返し表示の切替
(bind-key "C-c l" 'toggle-truncate-lines)

;; color-theme
;; (load-theme 'solarized-dark t)
(load-theme 'zenburn t)

;; 色を表す文字列に色をつける
(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook  'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'php-mode-hook  'rainbow-mode)
  (add-hook 'web-mode-hook  'rainbow-mode))

;; かっこに色をつける
(defun my/rainbow-delimiters-mode-turn-on ()
  (rainbow-delimiters-mode t))
(add-hook 'css-mode-hook  'my/rainbow-delimiters-mode-turn-on)
(add-hook 'scss-mode-hook 'my/rainbow-delimiters-mode-turn-on)
(add-hook 'php-mode-hook  'my/rainbow-delimiters-mode-turn-on)
(add-hook 'web-mode-hook  'my/rainbow-delimiters-mode-turn-on)
(add-hook 'js2-mode-hook  'my/rainbow-delimiters-mode-turn-on)
(add-hook 'ruby-mode-hook  'my/rainbow-delimiters-mode-turn-on)
(add-hook 'ess-mode-hook  'my/rainbow-delimiters-mode-turn-on)

;; ediff
(use-package ediff
  :config
  ;; コントロール用のバッファを同一フレーム内に表示
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのバッファを上下ではなく左右に並べる
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package expand-region
  :bind
  ("C-@" . er/expand-region)
  ("C-`" . er/contract-region))
(use-package multiple-cursors)

;; 最初のキーに続けて入力したものは指定したコマンドとして扱う
(use-package smartrep
  :config
  (smartrep-define-key
      global-map "C-." '(("C-n" . 'mc/mark-next-like-this)
                         ("C-p" . 'mc/mark-previous-like-this)
                         ("*"   . 'mc/mark-all-like-this))))

;; リージョンに対するマイナーモードを提供
(use-package region-bindings-mode
  :config
  ;; multiple-cursorsと連携
  (bind-keys :map region-bindings-mode-map
             ("a" . mc/mark-all-like-this)
             ("p" . mc/mark-previous-like-this)
             ("n" . mc/mark-next-like-this)
             ("m" . mc/mark-more-like-this))
  (region-bindings-mode-enable))
  ;; 発動させてくないモードを設定
  ;; (setq region-bindings-mode-disabled-modes '(foo-mode bar-mode))

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
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package linum)

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

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

;; 現在行の行番号をハイライト
(use-package hlinum)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:foreground "black" :background "red")))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - elscreen                                             ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; C-z C-c 新しいelscreenを作る
;; C-z C-k 現在のelscreenを削除する
;; C-z M-k 現在のelscreenをバッファごと削除する
;; C-z K   ほかの全elscreenを削除する！
;; C-z C-n 次のelscreenを選択
;; C-z C-p 前のelscreenを選択
;; C-z C-a 直前に選択したelscreenを選択
;; C-z C-f 新しいelscreenでファイルを開く
;; C-z b   新しいelscreenでバッファを開く
;; C-z d   新しいelscreenでdiredを開く
(use-package elscreen
  :config
  ;; プレフィクスキーはC-z
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  (elscreen-persist-mode 1)
  ;; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-kill-screen nil)
  ;; header-lineの先頭に[<->]を表示しない
  (setq elscreen-tab-display-control nil)
  ;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
  (setq elscreen-buffer-to-nickname-alist
	'(("^dired-mode$" .
	   (lambda ()
	     (format "Dired(%s)" dired-directory)))
	  ("^Info-mode$" .
	   (lambda ()
	     (format "Info(%s)" (file-name-nondirectory Info-current-file))))
	  ("^mew-draft-mode$" .
	   (lambda ()
	     (format "Mew(%s)" (buffer-name (current-buffer)))))
	  ("^mew-" . "Mew")
	  ("^irchat-" . "IRChat")
	  ("^liece-" . "Liece")
	  ("^lookup-" . "Lookup")))
  (setq elscreen-mode-to-nickname-alist
	'(("[Ss]hell" . "shell")
	  ("compilation" . "compile")
	  ("-telnet" . "telnet")
	  ("dict" . "OnlineDict")
	  ("*WL:Message*" . "Wanderlust"))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; 選択範囲をisearch
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search t)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook 'isearch-mode-hook
          '(lambda() (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook
          '(lambda() (setq w32-ime-composition-window nil)))

;; anzuの設定
;; 検索文字列が現在のバッファでいくつマッチするのかという情報と現在の位置をモードラインに表示するマイナーモードを提供
;; バッファが巨大だとanzuが低速化の原因となる場合がある
;; そのときは、http://emacs-jp.github.io/packages/mode-line/anzu.html を参照
(use-package anzu
  :config
  (global-anzu-mode +1)
  (setq anzu-use-migemo t)
  (setq anzu-search-threshold 1000)
  (setq anzu-minimum-input-length 3)
  ;; keybindの設定
  ;; (global-set-key (kbd "C-c a") 'anzu-query-replace)
  ;; (global-set-key (kbd "C-c A") 'anzu-query-replace-regexp)
  )

(defun isearch-forward-or-swiper (use-swiper)
  (interactive "P")
  (let (current-prefix-arg)
    (call-interactively (if use-swiper 'swiper 'isearch-forward))))
;;; バックエンドのivyがスペースを".*"に置換してしまうため、無効にする
;;; これをしないと純粋に正規表現isearchの置き換えにならない
(use-package ivy
  :init
  (bind-key "C-s" 'isearch-forward-or-swiper)
  :config
  (fset 'ivy--regex 'identity))

;; ace-jump-mode
(use-package ace-jump-mode
  :config
  (setq ace-jump-mode-gray-background nil)
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
  (ace-pinyin-global-mode 1)
  :bind
  ("C-j" . ace-jump-word-mode)
  ;; ("C-j" . ace-jump-char-mode) ;うまく動作しない
  ("C-c j" . ace-jump-line-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ replace                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 置換のキーバイドを変更
(bind-key "C-c r" 'query-replace)
;; 正規表現置換のキーバイドを変更
;; (bind-key "C-c C-r" 'query-replace-regexp)
(bind-key "C-c C-r" 'vr/query-replace)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - migemo                                               ;;;
;;;   https://github.com/emacs-jp/migemo                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package migemo
  :config
  (defvar migemo-command nil)
  (setq migemo-command "cmigemo")

  (defvar migemo-options nil)
  (setq migemo-options '("-q" "--emacs"))

  (defvar migemo-dictionary nil)
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

  (defvar migemo-user-dictionary nil)

  (defvar migemo-regex-dictionary nil)

  (defvar migemo-coding-system nil)
  (setq migemo-coding-system 'utf-8-unix)

  (load-library "migemo"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ File Manager                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; direx
(use-package direx
  :bind
  ("C-x j" . direx:jump-to-directory-other-window))

;; http://blog.shibayu36.org/entry/2013/02/12/191459
(defun direx:jump-to-project-directory ()
  (interactive)
  (let ((result (ignore-errors
                  (direx-project:jump-to-project-root-other-window)
                  t)))
    (unless result
      (direx:jump-to-directory-other-window))))
(global-set-key (kbd "C-x j") 'direx:jump-to-project-directory)
;; direx:direx-modeのバッファをウィンドウ左辺に幅25でポップアップ
;; :dedicatedにtを指定することで、direxウィンドウ内でのバッファの切り替えが
;; ポップアップ前のウィンドウに移譲される
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)
;; ツリーの表示で使われる罫線の形状を変更する
;; (setq direx:leaf-icon "  "
;;       direx:open-icon "▼"
;;       direx:closed-icon "▶")

;; ディレクトリ内のファイル名をそのまま編集する
;; diredバッファでrを押し、ファイル名を変更後C-c C-cまたはC-x C-sで保存.C-c C-kでキャンセル
(use-package wdired
  :init
  (bind-keys :map dired-mode-map
             ("r" . wdired-change-to-dired-mode)))

;; image+
(use-package image+
  :config
  (imagex-auto-adjust-mode 1)
  (imagex-global-sticky-mode 1))

;; image-dired+
(use-package image-dired+
  :config
  (image-diredx-async-mode 1))

;; cacoo
(use-package cacoo
  :bind
  ("M--" . toggle-cacoo-minor-mode))

;; 非アクティブなバッファをマスク
(use-package hiwin
  :config
  ;; hiwin-modeを有効化
  (hiwin-activate))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ server                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; emacs-server起動
(require 'server)
(unless (server-running-p)
  (server-start))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Edit support                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; カーソルの移動履歴を保存する
;; point-undoの設定
(use-package point-undo
  :bind
  ("M-[" . point-undo)
  ("M-]" . point-redo))

;; 最後の変更箇所にジャンプする
(use-package goto-chg
  :bind
  ("<f5>" . goto-last-change)
  ("S-<f5>" . goto-last-change-reverse))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ keybinds                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; C-hをBackSpaceにする
;; 入力されるキーシーケンスを置き換える
;; ?\C-?はDELのキーシケンス
(keyboard-translate ?\C-h ?\C-?)
;; 代わりにC-c h をヘルプに
(bind-key "C-c h" 'help-command)

;; 改行とインデントをRET(C-m)でできるように改善
(use-package smart-newline
  :bind
  ("C-m" . smart-newline))

(defadvice smart-newline (around C-u activate)
  "C-uを押したら元のC-mの挙動をするようにした。org-modeなどで活用。"
  (if (not current-prefix-arg)
      ad-do-it
    (let (current-prefix-arg)
      (let (smart-newline-mode)
        (call-interactively (key-binding (kbd "C-m")))))))

;; 折り返しトグルコマンド
(bind-key "C-c l" 'toggle-truncate-lines)

;; "C-t" でウィンドウを切り替える。分割していない時は左右分割して移動
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(bind-key* "C-t" 'other-window-or-split)

;; goto-lineコマンドをM-g M-g からM-gへ
(bind-key "M-g" 'goto-line)

;; 同じコマンドを連続実行したときの振る舞いを変更する
;; C-a C-a はバッファ先頭、C-e C-eはバッファ末尾
;; M-uは大文字、M-lは小文字
(use-package sequential-command-config
  :config
  (sequential-command-setup-keys))

;; 括弧の自動挿入の挙動をオレオレ設定できるflex-autopair.el
(use-package flex-autopair
  :config
  (flex-autopair-mode 1))

;; キーボード同時押しでコマンドを実行する
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode 1))

;; 「スペースから始まるkey-chord」を定義するEmacs Lisp
(use-package space-chord
  :init
  (unless (require 'space-chord nil t)
    (install-elisp-from-emacswiki "space-chord.el"))
  :config
  (setq space-chord-delay 0.08))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ my macros                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; "{"の挿入に対して改行と"}"を挿入
(defun my/curly-brace ()
  (interactive)
  (insert "{}")
  (backward-char)
  (smart-newline)
  (smart-newline))

;; ";"の入力に対して改行を自動挿入
(defun my/semicolon ()
  (interactive)
  (insert ";")
  (smart-newline))

;; "|"をpairにする
(defun my/vertical-bar-pair ()
  ;; (add-to-list 'flex-autopair-pairs '(?| . ?|))
  (interactive)
  (insert "||")
  (backward-char))

;; HTMLに挿入するテンプレートの構文を追加
(defun my/underscore-html-template ()
  "insert template syntax to HTML"
  (interactive)
  (insert "<% %>")
  (backward-char 3))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ helm                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (require 'helm)
(use-package helm-config
  :config
  (helm-mode 1)
  ;; キーバインドを設定
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-m")     'helm-mini)
  (global-set-key (kbd "C-x f")   'helm-find)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c o")   'helm-swoop)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (global-set-key (kbd "C-c C-s") 'helm-ag)
  (space-chord-define global-map "f"     'helm-for-files)
  (space-chord-define global-map "i"     'helm-imenu)
  (space-chord-define global-map "b"     'helm-descbinds)
  (space-chord-define global-map "r"     'helm-resume)
  ;; 検索wordをhelm-swoopで一覧化してくれる設定。isearchの時にC-oを押すと一覧が出る。
  (define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern))))))
  ;; helmの出る位置を設定
  (setq helm-split-window-default-side 'below)
  ;; popwinに登録
  (push '("^\*helm .+\*\\'" :regexp t) popwin:special-display-config))

;; helm-migemo
(use-package helm-migemo)
;; http://rubikitch.com/2014/12/19/helm-migemo/
(eval-after-load "helm-migemo"
  '(defun helm-compile-source--candidates-in-buffer (source)
     (helm-aif (assoc 'candidates-in-buffer source)
         (append source
                 `((candidates
                    . ,(or (cdr it)
                           (lambda ()
                             ;; Do not use `source' because other plugins
                             ;; (such as helm-migemo) may change it
                             (helm-candidates-in-buffer (helm-get-current-source)))))
                   (volatile) (match identity)))
       source)))

;; ace-isearch
;; 1文字→ace-jump-mode
;; 2〜5文字→isearch
;; 6文字以上→helm-swoop
(global-ace-isearch-mode 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package magit
  :bind
  ("C-c g" . magit-status))
;; :config
;; (push '("^\*magit*" :regexp t) popwin:special-display-config))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ coding support                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ファイル作成時にテンプレートを挿入
(auto-insert-mode)
;; 次に指定したディレクトリをロードする (最後の/は必須)
(setq auto-insert-directory (concat user-emacs-directory "insert/"))
;; 次で"\\.rb$"の代わりに'ruby-modeにすると、メジャーモードがruby-modeのときに挿入してくれる
;;(define-auto-insert "\\.rb\\'" "ruby-template.rb")

;; redoの設定
(use-package redo+
  :bind ("C-S-/" . redo))

;; 大量のredoに耐えられるようにする
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;; cua-modeの設定(矩形選択を可能に)
(cua-mode t)
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする
;; C-RETがC-jになるため、C-c C-SPCに矩形選択モードを割り当て
(define-key global-map (kbd "C-c C-SPC") 'cua-set-rectangle-mark)

;; auto-complete
(use-package auto-complete-config
  :config
  (bind-keys :map ac-mode-map
             ("TAB" . auto-complete))
  (bind-keys :map ac-completing-map
             ("C-n" . ac-next)
             ("C-p" . ac-previous))
  (add-to-list 'ac-dictionary-directories
               (concat user-emacs-directory "elisp/ac-dict/"))
  (ac-config-default)
  (global-auto-complete-mode t)
  (robe-mode))

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
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; ツールチップに表示
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; flycheck-pos-tip
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; highlight
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

(use-package highlight-symbol
  :bind
  ([(control f3)] . highlight-symbol-at-point)
  ([f3] . highlight-symbol-next)
  ([shift f3] . highlight-symbol-prev)
  ([(meta f3)] . highlight-symbol-query-replace)
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'web-mode-hook 'highlight-symbol-mode)
  (add-hook 'ruby-mode-hook 'highlight-symbol-mode)
  (add-hook 'js2-mode-hook 'highlight-symbol-mode)
  (add-hook 'python-mode-hook 'highlight-symbol-mode))

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ smartchr                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 同一キの入力を補助
(use-package smartchr
  :init
  (unless (require 'smartchr nil t)
    (install-elisp
     "https://raw.githubusercontent.com/imakado/emacs-smartchr/master/smartchr.el")))

(defun my/smartchr-braces ()
  "Insert a pair of braces."
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "{\n\n}")
                  (indent-region beg (point))
                  (forward-line -1)
                  (indent-according-to-mode)
                  (goto-char (point-at-eol))
                  (setq end (save-excursion
                              (re-search-forward "[[:space:][:cntrl:]]+}" nil t))))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my/smartchr-semicolon ()
  "Insert ';' and newline-and-indent"
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (indent-according-to-mode)
                  (setq beg (point))
                  (insert ";")
                  (newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my/smartchr-arrow ()
  "Insert ' ->' and newline-and-indent"
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert " ->")
                  (coffee-newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my/smartchr-fat-arrow ()
  "Insert ' =>' and newline-and-indent"
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert " =>")
                  (coffee-newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun smartchr-keybindings-ruby ()
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " " === " "=")))
  (local-set-key (kbd "~")  (smartchr '(" =~ " "~")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " " += ")))
  (local-set-key (kbd "-")  (smartchr '("-" " - " " -= ")))
  (local-set-key (kbd "<")  (smartchr '("<" " < " " << " " <= ")))
  (local-set-key (kbd ">")  (smartchr '(">" " => " " > " " >= " "->")))
  (local-set-key (kbd "%")  (smartchr '("%" " % " " %= ")))
  (local-set-key (kbd "!")  (smartchr '("!" " != " " !~ ")))
  (local-set-key (kbd "&")  (smartchr '(" & " " && " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " "**" )))
  (local-set-key (kbd "|")  (smartchr '("|`!!'|" " ||= " " || " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" "/`!!'/" " / " "// ")))
  (local-set-key (kbd "#")  (smartchr '("#{`!!'}" "#")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" "{|`!!'|  }" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-c/c++ ()
  (local-set-key (kbd ";")  (smartchr '(my/smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '(" - " "--" " -= " "-")))
  (local-set-key (kbd ">")  (smartchr '(" > " " >> " " >= " "->" ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= ")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<`!!'>" "<")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" " / " " /= ")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '(my/smartchr-braces "{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-awk ()
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "~")  (smartchr '(" ~ " " !~ " "~")))
  (local-set-key (kbd ">")  (smartchr '(" > " " >= " ">")))
  (local-set-key (kbd "<")  (smartchr '(" < " " <= " "<")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '(" - " "--" " -= " "-")))
  (local-set-key (kbd "|")  (smartchr '(" || " "|")))
  (local-set-key (kbd "&")  (smartchr '(" && " "&")))
  (local-set-key (kbd "/")  (smartchr '("/`!!'/" " / " "/")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" my/smartchr-braces "{")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-php ()
  (local-set-key (kbd ";")  (smartchr '(my/smartchr-semicolon ";")))
  (local-set-key (kbd ".")  (smartchr '("." " . " " .= ")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " " === " "=")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '(" - " "--" " -= " "-")))
  (local-set-key (kbd ">")  (smartchr '(" > " "->" " => " " >= " ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "*")  (smartchr '(" * " " *= " "**" "*")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '(" / " " /= " "/`!!'/" "/")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '(my/smartchr-braces "{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-web ()
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " "++" " += " )))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= " "**")))
  (local-set-key (kbd "%")  (smartchr '("%" " % " " %= ")))
  (local-set-key (kbd "=")  (smartchr '("=" " = " " === " )))
  (local-set-key (kbd "<")  (smartchr '("<" " < " " << " " <= ")))
  (local-set-key (kbd ">")  (smartchr '(">" " > " " => " " >= ")))
  (local-set-key (kbd "!")  (smartchr '("!" " !== " " != ")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" " / " " /= " "/`!!'/")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" my/smartchr-braces "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-js ()
  ;; (local-set-key (kbd ";")  (smartchr '(my/smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " === " "=" " == ")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " "++" " += " )))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= " "**")))
  (local-set-key (kbd "%")  (smartchr '("%" " % " " %= ")))
  (local-set-key (kbd "<")  (smartchr '("<" " < " " << " " <= ")))
  (local-set-key (kbd ">")  (smartchr '(">" " > " " => " " >= ")))
  (local-set-key (kbd "!")  (smartchr '("!" " !== " " != ")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" " / " " /= " "/`!!'/")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" my/smartchr-braces "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-coffee ()
  (local-set-key (kbd ";")  (smartchr '(my/smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '(" - " "--" " -= " "-")))
  (local-set-key (kbd ">")  (smartchr '(" > " my/smartchr-arrow " >= " my/smartchr-fat-arrow ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "?")  (smartchr '("?" " ?= ")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= ")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<`!!'>" "<")))
  (local-set-key (kbd "|")  (smartchr '("|" " ||= ")))
  (local-set-key (kbd "/")  (smartchr '("/" " / " " /= ")))
  (local-set-key (kbd "#")  (smartchr '("#{`!!'}" "#")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-R ()
  (local-set-key (kbd ";")  (smartchr '(my/smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " " === " "=")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " "++" " += " )))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd ">")  (smartchr '(" > " " => " " >= " ">")))
  (local-set-key (kbd "<")  (smartchr '(" <- " "<")))
  (local-set-key (kbd "%")  (smartchr '("%" " % " " %= ")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "*")  (smartchr '(" * " " *= " "**" "*")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '(" / " " /= " "/`!!'/" "/")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '(my/smartchr-braces "{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-web-erb ()
  (local-set-key (kbd "<") (smartchr '("<%= `!!' %>" "<% `!!' %>" "<`!!'>" "<")))
  (local-set-key (kbd "&") (smartchr '("&" " && " " & " " &= "))))

(add-hook 'c++-mode-hook 'smartchr-keybindings-c/c++)
(add-hook 'awk-mode-hook 'smartchr-keybindings-awk)
(add-hook 'php-mode-hook 'smartchr-keybindings-php)
(add-hook 'projectile-rails-mode-hook
          '(lambda ()
             (and (eq major-mode 'web-mode) (smartchr-keybindings-web-erb))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ yasnippet                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (use-package react-snippets)
  :bind
  ("C-:" . yas-expand))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ emacs lisp                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun emacs-lisp-mode-hooks ()
  "Hooks for emacs lisp"
  (setq indent-tabs-mode nil))

(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ HTML & CSS                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun web-mode-hooks ()
  "Hooks for web-mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.js?\\'" . web-mode)
  ("\\.jsx?\\'" . web-mode)
  ("\\.css?\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook 'web-mode-hooks)
  :config
  (add-hook 'web-mode-hook 'smartchr-keybindings-web)
  (bind-keys :map web-mode-map
             ("C-c t" . my/underscore-html-template)))

;; Emment
(defun emmet-mode-hooks ()
  "Hooks for emmet-mode"
  (setq emmet-indentation 2
        emmet-move-cursor-between-quotes t))

(use-package emmet-mode
  :init
  (add-hook 'emmet-mode-hook 'emmet-mode-hooks)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
  (add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
  (add-hook 'web-mode-hook  'emmet-mode) ;; web-modeで使う
  (add-hook 'scss-mode-hook 'emmet-mode)
  ;; C-j は newline のままにしておく
  (eval-after-load "emmet-mode" '(define-key emmet-mode-keymap (kbd "C-j") nil))
  ;;C-i と Tabの被りを回避
  (keyboard-translate ?\C-i ?\H-i)
  ;; C-i で展開
  (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scss                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun scss-mode-hooks ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)))

(use-package scss-mode
  :mode
  ("\\.scss\\'" . scss-mode)
  :config
  (add-hook 'scss-mode-hook 'scss-mode-hooks)
  ;; (bind-keys :map scss-mode-map
  ;;            ("{" . my/curly-brace))
             ;; (";" . my/semicolon))
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ JavaScript                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun js-indent-hook ()
  (setq js-indent-level 2
        js-expr-indent-offset 2
        indent-tabs-mode nil)
  ;; switchのcaseラベルをインデントする関数を定義
  (defun my/js-indent-line ()
    (interactive)
    (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status)))
      (back-to-indentation)
      (if (looking-at "case\\s-")
          (indent-line-to (+ indentation 2))
        (js-indent-line))
      (when (> offset 0) (forward-char offset))))
  ;; caseラベルのインデント処理をセット
  (set (make-local-variable 'indent-line-function) 'my/js-indent-line))
(add-hook 'js-mode-hook 'js2-minor-mode)

(use-package ac-js2
  :config
  (add-hook 'js2-mode 'ac-js2-mode)
  (setq ac-js2-evaluate-calls t))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

(use-package js2-mode
  :mode
  ;; ("\\.js\\'" . js2-mode)
  :interpreter
  ("node" . js2-mode)
  :init
  (add-hook 'js-mode-hook 'js-indent-hook)
  (add-hook 'js2-mode-hook 'smartchr-keybindings-js)
  (add-hook 'js2-mode-hook 'smart-newline-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'emmet-mode)
  :config
  (add-hook 'js2-mode-hook 'js-indent-hook)
  (use-package jquery-doc
    :config
    (add-hook 'js2-mode-hook 'jquery-doc-setup))
  (use-package tern-auto-complete
    :config
    (tern-ac-setup)))

;; (add-hook 'coffee-mode-hook 'smartchr-keybindings-coffee)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ markdown                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'emmet-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Ruby                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode)
  ("Capfile\\'" . ruby-mode)
  ("Gemfile\\'" . ruby-mode)
  :interpreter
  ("ruby" . ruby-mode)
  :init
  (add-hook 'ruby-mode-hook 'smart-newline-mode)
  (add-hook 'ruby-mode-hook 'smartchr-keybindings-ruby)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  :config
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq ruby-indent-level tab-width)
  (use-package ruby-end)
  (use-package ruby-block
    :config
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  (use-package inf-ruby)
  (use-package rbenv
    :config
    (global-rbenv-mode)
    (setq rbenv-installation-dir "~/.rbenv/bin/rbenv"))
  (bind-keys :map ruby-mode-map
             ("|" . my/vertical-bar-pair)))

(use-package chef-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Python                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun python-mode-hooks ()
  "python mode hooks"
  (setq indent-tabs-mode nil
	python-indent 4
	python-pylint t
	tab-width 4))
(add-hook 'python-mode-hook 'python-mode-hooks)

(use-package ac-python
  :config
  (add-to-list 'ac-modes 'python-2-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Java                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun java-mode-hooks ()
  "Java mode hooks"
  (setq tab-width 4
        indent-tabs-mode nil))
(add-hook 'java-mode-hook 'java-mode-hooks)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Scala                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Clojure                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package clojure-mode)
(put 'letfn 'clojure-backtracking-indent '((2) 2))
(put 'macrolet 'clojure-backtracking-indent '((2) 2))
;; cider
(add-hook 'clojure-mode-hook 'cider-mode)
;; mini bufferに関数の引数を表示させる
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; 'C-x b' した時に *nrepl-connection* と *nrepl-server* のbufferを一覧に表示しない
(setq nrepl-hide-special-buffers t)
;; RELPのbuffer名を 'project名:nREPLのport番号' と表示する
;; project名は project.clj で defproject した名前
(setq nrepl-buffer-name-show-port t)

(autoload 'ac-cider "ac-cider" nil t)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Teach compile the syntax of the kibit output
(autoload 'compile "compile" nil t)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))

(add-hook 'clojure-mode-hook
	  (lambda ()
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ sql                                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(unless (require 'sql nil t)
  (install-elisp "http://www.emacswiki.org/emacs/download/sql.el"))
(unless (require 'sql-indent nil t)
  (install-elisp "http://www.emacswiki.org/emacs/download/sql-indent.el"))
;; (unless (require 'sql-complete nil t)
;;   (install-elisp "http://www.emacswiki.org/emacs/download/sql-complete.el"))
(unless (require 'sql-transform nil t)
  (install-elisp "http://www.emacswiki.org/emacs/download/sql-transform.el"))

;; C-c C-c : 'sql-send-paragraph
;; C-c C-r : 'sql-send-region
;; C-c C-s : 'sql-send-string
;; C-c C-b : 'sql-send-buffer
;; C-c C-c : 'sql-send-paragraph
;; C-c C-r : 'sql-send-region
;; C-c C-s : 'sql-send-string
;; C-c C-b : 'sql-send-buffer
(require 'sql)

(add-hook 'sql-interactive-mode-hook
          #'(lambda ()
              (interactive)
              (set-buffer-process-coding-system 'sjis-unix 'sjis-unix )
              (setq show-trailing-whitespace nil)))

;; starting SQL mode loading sql-indent / sql-complete
(eval-after-load "sql"
  '(progn
     (load-library "sql-indent")
     ;; (load-library "sql-complete")
     (load-library "sql-transform")))

(setq auto-mode-alist
      (cons '("\\.sql\\'" . sql-mode) auto-mode-alist))

(sql-set-product-feature
 'ms :font-lock 'sql-mode-ms-font-lock-keywords)

(defcustom sql-ms-program "sqlcmd"
  "Command to start sqlcmd by SQL Server."
  :type 'file
  :group 'SQL)

(sql-set-product-feature
 'ms :sql-program 'sql-ms-program)
(sql-set-product-feature
 'ms :sqli-prompt-regexp "^[0-9]*>")
(sql-set-product-feature
 'ms :sqli-prompt-length 5)

(defcustom sql-ms-login-params
  '(user password server database)
  "Login parameters to needed to connect to mssql."
  :type '(repeat (choice
                  (const user)
                  (const password)
                  (const server)
                  (const database)))
  :group 'SQL)

(defcustom sql-ms-options '("-U" "-P" "-S" "-d")
  "List of additional options for `sql-ms-program'."
  :type '(repeat string)
  :group 'SQL)

(defun sql-connect-ms ()
  "Connect ti SQL Server DB in a comint buffer."
  ;; Do something with `sql-user', `sql-password',
  ;; `sql-database', and `sql-server'.
  (let ((f #'(lambda (op val)
               (unless (string= "" val)
                 (setq sql-ms-options
                       (append (list op val) sql-ms-options)))))
        (params `(("-U" . ,sql-user)("-P" . ,sql-password)
                  ("-S" . ,sql-server)("-d" . ,sql-database))))
    (dolist (pair params)
      (funcall f (car pair)(cdr pair)))
    (sql-connect-1 sql-ms-program sql-ms-options)))

(sql-set-product-feature
 'ms :sqli-login 'sql-ms-login-params)
(sql-set-product-feature
 'ms :sqli-connect 'sql-connect-ms)

(defun run-mssql ()
  "Run mssql by SQL Server as an inferior process."
  (interactive)
  (sql-product-interactive 'ms))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ R                                                             ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 拡張子が r, R の場合に R-mode を起動
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
;; R-mode を起動する時に ess-site をロード
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;; R を起動する時に ess-site をロード
(autoload 'R "ess-site" "start R" t)

(add-hook 'R-mode-hook 'smartchr-keybindings-R)

;; R-mode, iESS を起動する際に呼び出す初期化関数
(setq ess-loaded-p nil)
(defun ess-load-hook (&optional from-iess-p)
  ;; インデントの幅を4にする（デフォルト2）
  (setq ess-indent-level 4)
  ;; インデントを調整
  (setq ess-arg-function-offset-new-line (list ess-indent-level))
  ;; comment-region のコメントアウトに # を使う（デフォルト##）
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)

  ;; 最初に ESS を呼び出した時の処理
  (when (not ess-loaded-p)
    ;; アンダースコアの入力が " <- " にならないようにする
    (ess-toggle-underscore nil)
    ;; 補完機能を有効にする
    (setq ess-use-auto-complete t)
    ;; anything を使いたいので IDO は邪魔
    (setq ess-use-ido nil)
    ;; キャレットがシンボル上にある場合にもエコーエリアにヘルプを表示する
    (setq ess-eldoc-show-on-symbol t)
    ;; 起動時にワーキングディレクトリを尋ねられないようにする
    (setq ess-ask-for-ess-directory nil)
    ;; # の数によってコメントのインデントの挙動が変わるのを無効にする
    (setq ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      ;; ウィンドウが1つの状態で *.R を開いた場合はウィンドウを縦に分割して R を表示する
      (when (one-window-p)
        (split-window-horizontally)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf)))
      ;; R を起動する前だと auto-complete-mode が off になるので自前で on にする
      ;; cf. ess.el の ess-load-extras
      (when (and ess-use-auto-complete (require 'auto-complete nil t))
        (add-to-list 'ac-modes 'ess-mode)
        (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
                '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
        (setq ac-sources '(ac-source-R ac-source-filename)))))

  (if from-iess-p
      ;; R のプロセスが他になければウィンドウを分割する
      (if (> (length ess-process-name-list) 0)
          (when (one-window-p)
            (split-window-horizontally)
            (other-window 1)))
    ;; *.R と R のプロセスを結びつける
    ;; これをしておかないと補完などの便利な機能が使えない
    (ess-force-buffer-current "Process to load into: ")))

;; R-mode 起動直後の処理
(add-hook 'R-mode-hook 'ess-load-hook)
;; R 起動直前の処理
(add-hook 'ess-pre-run-hook (lambda () (ess-load-hook t)))

;; helm-R
(use-package helm-R
  :config
  (bind-keys :map ess-mode-map
             ("C-c h" . helm-for-R))
  (bind-keys :map inferior-ess-mode-map
             ("C-c h" . helm-for-R)))

;;
(use-package ess-R-data-view
  :config
  (define-key ess-mode-map (kbd "C-c v") 'ess-R-dv-pprint)
  (push '("*R data view*") popwin:special-display-config))

;; C-c C-g で オブジェクトの内容を確認できるようにする
(use-package ess-R-object-popup
  :config
  (define-key ess-mode-map (kbd "C-c C-g") 'ess-R-object-popup))

(defvar e2wm:close-popup-window-timer nil
  "Timer of closing the popup window.")

(defun e2wm:start-close-popup-window-timer ()
  (or e2wm:close-popup-window-timer
      (setq e2wm:close-popup-window-timer
            (run-with-timer popwin:close-popup-window-timer-interval
                            popwin:close-popup-window-timer-interval
                            'e2wm:close-popup-window-timer))))

(defun e2wm:close-popup-window-timer ()
  (condition-case var
      (e2wm:close-popup-window-if-necessary
       (e2wm:should-close-popup-window-p))
    (error (message "e2wm:close-popup-window-timer: error: %s" var))))

(defun e2wm:close-popup-window-if-necessary (&optional force)
  "Close the popup window if another window has been selected. If
FORCE is non-nil, this function tries to close the popup window
immediately if possible, and the lastly selected window will be
selected again."
  (when (wlf:get-window (e2wm:pst-get-wm) 'sub)
    (let* ((window (selected-window))
           (minibuf-window-p (eq window (minibuffer-window)))
           (other-window-selected
            (and (not (eq window (wlf:get-window (e2wm:pst-get-wm) 'sub)))
                 (not (eq window (wlf:get-window (e2wm:pst-get-wm) 'main)))))
           ;; (not-stuck-or-closed
           ;;  (or (not popwin:popup-window-stuck-p)
           ;;      (not (popwin:popup-window-live-p))))
           )
      (when (and (not minibuf-window-p)
                 (or force other-window-selected))
        (wlf:hide (e2wm:pst-get-wm) 'sub)
        (e2wm:pst-window-select-main-command)
        (e2wm:stop-close-popup-window-timer)
        ;; (popwin:close-popup-window other-window-selected)
        ))))

(defun e2wm:should-close-popup-window-p ()
  "Return t if popwin should close the popup window
immediately. It might be useful if this is customizable
function."
  (and (wlf:get-window (e2wm:pst-get-wm) 'sub)
       (and (eq last-command 'keyboard-quit)
            (eq last-command-event ?\C-g))))

(defun e2wm:stop-close-popup-window-timer ()
  (when e2wm:close-popup-window-timer
    (cancel-timer e2wm:close-popup-window-timer)
    (setq e2wm:close-popup-window-timer nil)))

(setq e2wm:prefix-key "C-q ")

(setq e2wm:c-two-right-default 'prev)

(setq e2wm:def-plugin-files-sort-key 'time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Perspective Definition

;;; code / Code editing perspective
;;;--------------------------------------------------
(cond
 ((equal system-name "x60")
  (setq e2wm:c-code-recipe
        '(| (:left-max-size 35)
            (- (:upper-size-ratio 0.4)
               files
               (- (:upper-size-ratio 0.5)
                  bookmarks history))
            (- (:upper-size-ratio 0.7)
               (| (:right-max-size 30)
                  main imenu)
               sub)))
  (setq e2wm:c-code-winfo
        '((:name main)
          (:name bookmarks :plugin bookmarks-list)
          (:name files :plugin files :plugin-args (:sort time :show-hidden nil))
          (:name history :plugin history-list)
          (:name sub :buffer "*info*" :default-hide t)
          (:name imenu :plugin imenu :default-hide t))))

 ((equal system-name "desktop")
  (setq e2wm:c-R-code-recipe
        '(| (:left-max-size 60)
            (- (:upper-size-ratio 0.3)
               R-graphics-list
               (- (:upper-size-ratio 0.5)
                  R-graphics
                  history))
            (- (:upper-size-ratio 0.7)
               (| (:right-max-size 60)
                  (- (:upper-size-ratio 0.7)
                     main
                     proc)
                  (- (:upper-size-ratio 0.3)
                     R-dired
                     imenu))
               sub)))

  (setq e2wm:c-code-recipe
        '(| (:left-max-size 60)
            (- (:upper-size-ratio 0.4)
               files
               (- (:upper-size-ratio 0.5)
                  bookmarks history))
            (- (:upper-size-ratio 0.7)
               (| (:right-max-size 60)
                  main imenu)
               sub)))
  (setq e2wm:c-code-winfo
        '((:name main)
          (:name bookmarks :plugin bookmarks-list)
          (:name files :plugin files :plugin-args (:sort time :show-hidden nil))
          (:name history :plugin history-list)
          (:name sub :buffer "*info*" :default-hide t)
          (:name imenu :plugin imenu :default-hide nil)))))

(defun e2wm:dp-htwo ()
  (interactive)
  (e2wm:pst-change 'htwo))

;; キーバインド
(eval-after-load 'R-mode
  '(progn
     (setq e2wm:pst-minor-mode-keymap
           (e2wm:define-keymap
            '(("C-S-p"    . e2wm:pst-history-forward-command) ; 履歴を進む
              ("C-S-n"    . e2wm:pst-history-back-command) ; 履歴をもどる
              ("M-m"      . e2wm:pst-window-select-main-command)
              ("prefix q" . e2wm:stop-management)
              ("prefix l" . e2wm:pst-update-windows-command)
              ("prefix 1" . e2wm:dp-code)
              ("prefix 2" . e2wm:dp-two)
              ("prefix 3" . e2wm:dp-htwo)
              ("prefix 4" . e2wm:dp-doc)
              ("prefix 5" . e2wm:dp-array)
              ("prefix 6" . e2wm:dp-R-code)
              ("prefix 7" . e2wm:dp-R-view)
              ("prefix v" . e2wm:dp-vcs))
            e2wm:prefix-key))
     (setq e2wm:dp-code-minor-mode-map
           (e2wm:define-keymap
            '(("prefix I" . e2wm:dp-code-imenu-toggle-command)
              ("prefix S" . e2wm:dp-code-sub-toggle-command)
              ("prefix C" . e2wm:dp-code-toggle-clock-command)
              ("prefix c" . e2wm:dp-code-toggle-svg-clock-command)
              ("prefix M" . e2wm:dp-code-main-maximize-toggle-command)
              ("prefix h" . e2wm:dp-code-navi-history-command)
              ("prefix f" . e2wm:dp-code-navi-files-command)
              ("prefix i" . e2wm:dp-code-navi-imenu-command)
              ("prefix s" . e2wm:dp-code-navi-sub-command)
              ("C-c m"    . e2wm:dp-code-popup-messages)
              ("prefix b" . e2wm:dp-code-navi-bookmarks-command))
            e2wm:prefix-key))
     (e2wm:add-keymap
      e2wm:def-plugin-files-mode-map
      '(("k" . e2wm:def-plugin-files-updir-command)
        ("C-S-n"     . e2wm:dp-two-right-history-down-command)
        ("C-S-p"     . e2wm:dp-two-right-history-up-command)
        ("prefix h"  . e2wm:dp-two-navi-history-command)
        ("prefix l"  . e2wm:pst-update-windows-command)
        ("prefix j"  . e2wm:dp-two-navi-left-command)
        ("prefix k"  . e2wm:dp-two-navi-right-command)
        ("prefix d"  . e2wm:dp-two-double-column-command)
        ("prefix S"  . e2wm:dp-two-sub-toggle-command)
        ("prefix -"  . e2wm:dp-two-swap-buffers-command)
        ("prefix H"  . e2wm:dp-two-history-toggle-command)
        ("prefix M"  . e2wm:dp-two-main-maximize-toggle-command))
      e2wm:prefix-key)
     (setq e2wm:def-plugin-history-list-mode-map
           (e2wm:define-keymap
            '(("k" . previous-line)
              ("j" . next-line)
              ("p" . previous-line)
              ("n" . next-line)
              ("d" . e2wm:def-plugin-history-list-kill-command)
              ("<SPC>" . e2wm:def-plugin-history-list-show-command)
              ("C-m"   . e2wm:def-plugin-history-list-select-command)
              ("q"     . e2wm:pst-window-select-main-command)
              )))))

(custom-set-faces
 '(e2wm:face-history-list-normal  ((t (:foreground "#f0dfaf" ))))
 (custom-set-faces
  '(e2wm:face-history-list-select1 ((t (:foreground "#cc9393")))))
 (custom-set-faces
  '(e2wm:face-history-list-select2 ((t (:foreground "#8cd0d3" ))))))

;; (when win-p
;;   (setq e2wm:def-plugin-clock-download-file "D:/tmp/wmclock.jpg")
;;   (setq e2wm:def-plugin-clock-resized-file  "D:/tmp/wmclock.jpg"))
;;↑cygwin環境の場合は "C:/cygwin/tmp/wmclock.jpg" とかにすると良いかも

(defun e2wm:dp-code-popup-messages ()
  (interactive)
  (e2wm:dp-code-popup-sub "*Messages*")
  (e2wm:start-close-popup-window-timer)
  (e2wm:pst-window-select-main-command))

(defun e2wm:dp-code-popup (buf)
  ;;とりあえず全部subで表示してみる
  (let ((cb (current-buffer)))
    (e2wm:message "#DP CODE popup : %s (current %s / backup %s)"
                  buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm)))
    (cond
     ((e2wm:history-recordable-p buf)
      (e2wm:pst-show-history-main)
      ;;記録対象なら履歴に残るのでupdateで表示を更新させる
      t)
     ((and e2wm:override-window-cfg-backup
           (eq (selected-window) (wlf:get-window wm 'sub)))
      ;;現在subならmainに表示しようとする
      ;;minibuffer以外の補完バッファは動きが特殊なのでbackupをnilにする
      (setq e2wm:override-window-cfg-backup nil)
      ;;一時的に表示するためにset-window-bufferを使う
      ;;(prefix) C-lなどで元のバッファに戻すため
      (set-window-buffer (wlf:get-window wm 'main) buf)
      t)
     ((and e2wm:c-code-show-main-regexp
           (string-match e2wm:c-code-show-main-regexp buf-name))
      (e2wm:pst-buffer-set 'main buf t)
      t)
     ((string-match (or " *auto-async-byte-compile*" "*Backtrace*") buf-name)
      (e2wm:dp-code-popup-sub buf)
      (e2wm:start-close-popup-window-timer)
      t)
     ((string-match "*Help*" buf-name)
      ;;(e2wm:dp-code-popup-sub buf)
      (e2wm:start-close-popup-window-timer)
      (let ((wm (e2wm:pst-get-wm))
            (not-minibufp (= 0 (minibuffer-depth))))
        (e2wm:pst-buffer-set 'sub buf t not-minibufp))(sit-for 10)
        (select-window (wlf:get-window (e2wm:pst-get-wm) 'sub))
        ;;(other-window 1)
        t)
     (t
      (e2wm:dp-code-popup-sub buf)
      t))))

(add-hook 'e2wm:post-stop-hook
          (lambda ()
            (setq display-buffer-function 'popwin:display-buffer)))

(eval-after-load "elscreen"
  '(progn
     ;; overrides storages for elscreen
     (defadvice e2wm:frame-param-get (around e2wm:ad-override-els (name &optional frame))
       ;; frame is not used...
       (e2wm:message "** e2wm:frame-param-get : %s " name) ;
       (let ((alst (cdr (assq 'e2wm-frame-prop
                              (elscreen-get-screen-property
                               (elscreen-get-current-screen))))))
         (setq ad-return-value (and alst (cdr (assq name alst))))))
     (defadvice e2wm:frame-param-set (around e2wm:ad-override-els (name val &optional frame))
       (e2wm:message "** e2wm:frame-param-set : %s / %s" name val)
       (let* ((screen (elscreen-get-current-screen))
              (screen-prop (elscreen-get-screen-property screen))
              (alst (cdr (assq 'e2wm-frame-prop screen-prop))))
         (set-alist 'alst name val)
         (set-alist 'screen-prop 'e2wm-frame-prop alst)
         (elscreen-set-screen-property screen screen-prop)
         (setq ad-return-value val)))
     ;; grab switch events
     (defun e2wm:elscreen-define-advice (function)
       (eval
        `(defadvice ,function (around e2wm:ad-override-els)
           (e2wm:message "** %s vvvv" ',function)
           (when (e2wm:managed-p)
             (e2wm:message "** e2wm:managed")
             (let ((it (e2wm:Pst-Get-Instance)))
               (e2wm:pst-method-call e2wm:$pst-class-leave it (e2wm:$pst-wm it)))
             (e2wm:pst-minor-mode -1))
           (e2wm:message "** ad-do-it ->")
           ad-do-it
           (e2wm:message "** ad-do-it <-")
           (e2wm:message "** e2wm:param %s"
                         (cdr (assq 'e2wm-frame-prop
                                    (elscreen-get-screen-property
                                     (elscreen-get-current-screen)))))
           (when (e2wm:managed-p)
             (e2wm:message "** e2wm:managed")
             (let ((it (e2wm:pst-get-instance)))
               (e2wm:pst-method-call e2wm:$pst-class-start it (e2wm:$pst-wm it)))
             (e2wm:pst-minor-mode 1))
           (e2wm:message "** %s ^^^^^" ',function)
           )))
     (defadvice elscreen-create (around e2wm:ad-override-els)
       (let (default-wcfg)
         (when (e2wm:managed-p)
           (loop for screen in (reverse (sort (elscreen-get-screen-list) '<))
                 for alst = (cdr (assq 'e2wm-frame-prop
                                       (elscreen-get-screen-property screen)))
                 for wcfg = (and alst (cdr (assq 'e2wm-save-window-configuration alst)))
                 if wcfg
                 do (setq default-wcfg wcfg) (return)))
         ad-do-it
         (when default-wcfg
           (set-window-configuration default-wcfg))))

     ;; apply defadvices to some elscreen functions
     (loop for i in '(elscreen-goto
                      elscreen-kill
                      elscreen-clone
                      elscreen-swap)
           do (e2wm:elscreen-define-advice i))
     (defun e2wm:elscreen-override ()
       (ad-activate-regexp "^e2wm:ad-override-els$")
       (setq e2wm:override-window-ext-managed t))
     (defun e2wm:elscreen-revert ()
       (ad-deactivate-regexp "^e2wm:ad-override-els$")
       (setq e2wm:override-window-ext-managed nil))
     ;; start and exit
     (add-hook 'e2wm:pre-start-hook 'e2wm:elscreen-override)
     (add-hook 'e2wm:post-stop-hook 'e2wm:elscreen-revert)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ nginx                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(defun nginx-mode-hooks ()
  (setq nginx-indent-level 2)
  (setq nginx-indent-tabs-mode nil))

(use-package nginx-mode
  :config
  (add-hook 'nginx-mode-hook 'nginx-mode-hooks))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ org-mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-hook 'org-mode-hook 'smart-newline-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ term                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; multi-term→起動しない
(use-package multi-term)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tramp                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package tramp
  :config
  (setq tramp-default-method "sshx")
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

;; 自動でsudoする設定
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" File))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ English                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; aspell
;; Have to install
;; brew install aspell --lang=en
;; echo "lang en_US" > ~/.aspell.conf
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  ;; 日本語混じりでも使用可能にする
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; (setq-default ispell-program-name "/usr/local/bin/aspell")

;; google-translate
(use-package google-translate
  :config
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja"))
  (push '("*Google Translate*") popwin:special-display-config)
  :bind
  ("C-c C-t" . google-translate-at-point))
