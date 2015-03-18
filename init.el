
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Cask                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (and (not (equal window-system 'w32))
	   (or (require 'cask nil t) 	; for MacOS X (homebrew)
	       (require 'cask "~/.cask/cask.el" t))) ;for Linux (install by curl)
  (cask-initialize))
(require 'pallet)


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
;; (add-to-load-path "elisp" ".cask/24.4.1/elpa")
(add-to-load-path "elisp")

;; include PATH from Shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Package control                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; install-elisp の関数を利用可能にする
  (auto-install-compatibility-setup))

;; package管理の設定
(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeを追加
  (add-to-list 'package-archives '("marmalade"	. "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa"	. "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA"	. "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通してロードする
  (package-initialize))

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

;; Emacsからの質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;; 起動時にバイトコンパイルする(emacsは古くても.elcファイルを優先的にロードするため)
(use-package auto-async-byte-compile
  :init
  (install-elisp-from-emacswiki "auto-async-byte-compile.el")
  :config
  ;; 自動コンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ only mac                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta))
  (global-set-key (kbd "C-M-¥") 'indent-region)
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)
;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-unix)
;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)
;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)
;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

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

;; ffap.elにより、現在位置のファイル/URLを開く
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
;;; @ screen - tabbar                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package tabbar
  :config
  (call-interactively 'tabbar-mode t)
  ;; ボタン非表示
  (dolist (btn '(tabbar-buffer-home-button
		 tabbar-scroll-left-button
		 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil) (cons "" nil)))
    )
  ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
  ;; (call-interactively 'tabbar-mwheel-mode -1)
  ;; (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
  ;; (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)
  ;; タブグループを使用（t：有効，nil：無効）
  (defvar tabbar-buffer-groups-function nil)
  (setq tabbar-buffer-groups-function nil)
  ;; タブの表示間隔
  (defvar tabbar-separator nil)
  (setq tabbar-separator '(1.0))
  
  :bind
  ;; タブ切り替え
  ("<C-tab>" . tabbar-forward-tab)
  ("<C-S-tab>" . tabbar-backward-tab))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 大文字・小文字を区別しないでサーチ
(setq-default case-fold-search t)

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll t)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ File Manager                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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
  :config
  (bind-keys :map dired-mode-map
	     ("r" . wdired-change-to-dired-mode)))

;; 非アクティブなバッファをマスク
(use-package hiwin
  :config
  ;; hiwin-modeを有効化
  (hiwin-activate))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ replace                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 置換のキーバイドを変更
(bind-key "C-c r" 'query-replace)
;; 正規表現置換のキーバイドを変更
(bind-key "C-c C-r" 'query-replace-regexp)

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

  (load-library "migemo")
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ server                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; emacs-server起動
(use-package server
  :config
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (setq server-socket-dir "~/.emacs.d")
  (unless (server-running-p)
    (server-start))
  )

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
(bind-key "C-t" 'other-window-or-split)

;; goto-lineコマンドをM-g M-g からM-gへ
(bind-key "M-g" 'goto-line)

;; 同じコマンドを連続実行したときの振る舞いを変更する
;; (auto-install-batch sequential-command)
;; C-a C-a はバッファ先頭、C-e C-eはバッファ末尾
;; M-uは大文字、M-lは小文字
(use-package sequential-command-config
  :config
  (sequential-command-setup-keys))

;; "に対して""を挿入することなどを実現
(use-package key-combo
  :config
  (key-combo-load-default))

;; 参考設定(http://qiita.com/akisute3@github/items/0141c92dca0992732af8)
;; (key-combo-mode 1)
;; ;;; 各モードに対するキー設定
;; (setq key-combo-lisp-mode-hooks
;;       '(lisp-mode-hook
;; 	emacs-lisp-mode-hook
;; 	lisp-interaction-mode-hook
;; 	inferior-gauche-mode-hook
;; 	scheme-mode-hook))

;; (setq key-combo-lisp-default
;;       '(("."  . " . ")
;; 	(","  . (key-combo-execute-orignal))
;; 	(",@" . " ,@")
;; 	(";"  . (";;;; " ";"))
;; 	(" = "	. (" =	" "eq " "equal "))
;; 	(" >= " . " >= ")))

;; (setq key-combo-common-mode-hooks
;;       '(c-mode-common-hook
;;         php-mode-hook
;;         ruby-mode-hook
;;         cperl-mode-hook
;;         javascript-mode-hook
;;         js-mode-hook
;;         js2-mode-hook
;; 	))

;; (setq key-combo-common-default
;;       '((","  . (", " ","))
;;         (" = "  . ("  =  " "  ==  " "  ===  " " = "))
;;         (" => " . "  =>  ")
;;         (" = ~" . "  = ~ ")
;;         (" = *" . "  = * ")
;;         ("+"  . (" + " " + =  " "+"))
;;         ("+ = " . " + =  ")
;;         ("-"  . (" - " " - =  " "-"))
;;         ("- = " . " - =  ")
;;         ("->" . " -> ")
;;         (">"  . (" > " "  =>  " "  >=  " ">"))
;;         (" >= " . "  >=  ")
;;         ("%"  . (" % " " % =  " "%"))
;;         ("% = "  . " % =  ")
;;         ("!" . (" ! =  " " !~ " "!"))
;;         ("! = "  . " ! =  " )
;;         ("!~" . " !~ ")
;;         ("~" . ("  = ~ " "~"))
;;         ("::" . " :: ")
;;         ("&"  . (" & " " && " "&"))
;;         ("& = " . " & =  ")
;;         ("&& = " . " && =  ")
;;         ("*"  . (" * " "**" "*"))
;;         ("* = "  . " * =  " )
;;         ("<" . (" < " " < =  " "<"))
;;         ("< = " . " < =  ")
;;         ("<< = " . " << =  ")
;;         ("<-" . " <- ")
;;         ("|"  . (" || =  " " || " "|"))
;;         ("| = " . " | =  ")
;;         ("|| = " . " || =  ")
;;         ("/" . ("/`!!'/" " / " "// "))
;;         ("/ = " . " / =  ")
;;         ("/*" . "/* `!!' */")
;;         ("{" . ("{`!!'}" "{"))
;;         ("{|" . "{ |`!!'|  }")
;;         ("\"" . ("\"`!!'\"" "\""))
;;         ("'" . ("'`!!''" "'"))
;;         ("(" . ("(`!!')" "("))))

;; (key-combo-define-hook key-combo-common-mode-hooks
;;                        'key-combo-common-load-default
;;                        key-combo-common-default)
;; (key-combo-define-hook key-combo-lisp-mode-hooks
;;                        'key-combo-lisp-load-default
;;                        key-combo-lisp-default)

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
  (install-elisp-from-emacswiki "space-chord.el")
  :config
  (setq space-chord-delay 0.08))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ helm                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (require 'helm)
(use-package helm-config
  :config
  (helm-mode 1)
  ;; キーバインドを設定
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "C-;")     'helm-mini)
  (global-set-key (kbd "C-x f")   'helm-find)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c p")   'helm-c-Apropos)
  (global-set-key (kbd "C-c o")   'helm-swoop)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
  (space-chord-define global-map "f"     'helm-for-files)
  (space-chord-define global-map "i"     'helm-imenu)
  (space-chord-define global-map "b"     'helm-descbinds)
  (space-chord-define global-map "s"     'helm-ag)
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
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  )

;; helm-migemo
(use-package helm-migemo
  :config
  (setq helm-use-migemo t))

;; 候補が表示されないときがあるので
;; migemoらないように設定
(defadvice helm-c-apropos
  (around ad-helm-apropos activate)
  (let ((helm-use-migemo nil))
    ad-do-it))
(defadvice helm-M-x
  (around ad-helm-M-x activate)
  (let ((helm-use-migemo nil))
    ad-do-it))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package magit)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ coding support                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; ファイル作成時にテンプレートを挿入
(auto-insert-mode)
;; 次に指定したディレクトリをロードする (最後の/は必須)
(setq auto-insert-directory "~/.emacs.d/insert/")
;; 次で"\\.rb$"の代わりに'ruby-modeにすると、メジャーモードがruby-modeのときに挿入してくれる
;;(define-auto-insert "\\.rb$" "ruby-template.rb")

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

;; auto-compliete
(use-package auto-complete-config
  :config
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (ac-config-default)
  (robe-mode)
  (bind-keys :map ac-mode-map
	     ("TAB" . auto-complete))
  (bind-keys :map ac-completing-map
	     ("C-n" . ac-next)
	     ("C-p" . ac-previous)))

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
(add-hook 'after-init-hook #'global-flycheck-mode)
;; ツールチップに表示
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; highlight
(use-package highlight-symbol
  :bind
  ([(control-f3)]      . highlight-symbol-at-point)
  ([f3]	       . highlight-symbol-next)
  ([shift f3] . highlight-symbol-prev)
  ([(meta f3)]  . highlight-symbol-query-replace))

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S."))

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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ yasnippet                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; (require 'yasnippet)
;; (yas-global-mode 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ emacs lisp                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ HTML & CSS                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package web-mode
  :mode(
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.html?\\'" . web-mode)))

;; Emment(Zen-coding後継)
;; (auto-install-from-url "https://raw.github.com/smihica/emmet-mode/master/emmet-mode.el")
(use-package emmet-mode
  :config
  (setq emmet-indentation 2)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
  (add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
  (add-hook 'web-mode-hook  'emmet-mode) ;; web-modeで使う
  (setq emmet-move-cursor-between-quotes t) ;; 最初のクオートの中にカーソルをぶちこむ
  ;; C-j は newline のままにしておく
  (eval-after-load "emmet-mode" '(define-key emmet-mode-keymap (kbd "C-j") nil)) 
  (keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
  (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開
  )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scss                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package scss-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ markdown                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package markdown-mode
  :mode
  ("¥¥.md¥¥'" . markdown-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Ruby                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'smart-newline-mode))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Javascript                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package js2-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ org-mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(add-hook 'org-mode-hook 'smart-newline-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tramp                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package tramp
  :config
  (setq tramp-default-method "ssh"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(magit-use-overlays nil))
