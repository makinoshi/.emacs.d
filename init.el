;; C-hをBackSpaceにする
;; 入力されるキーシーケンスを置き換える
;; ?\C-?はDELのキーシケンス
(keyboard-translate ?\C-h ?\C-?)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; cask config
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

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
(add-to-load-path "elisp")

;; Load other init file
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; include PATH from Shell
(exec-path-from-shell-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; color-theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; 色を表す文字列に色をつける
(use-package rainbow-mode)

;; かっこに色をつける
(use-package rainbow-delimiters
  :config
  (use-package color
    :config
    (--each (number-sequence 1 rainbow-delimiters-max-face-count)
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" it))))
        (callf color-saturate-name (face-foreground face) 90)))))

;; ediff
(use-package ediff
  :config
  ;; コントロール用のバッファを同一フレーム内に表示
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; diffのバッファを上下ではなく左右に並べる
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package expand-region
  :commands
  (er/expand-region er/contract-region)
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
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package linum
  :config
  (setq linum-format "%4d"))

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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


;;; バックエンドのivyがスペースを".*"に置換してしまうため、無効にする
;;; これをしないと純粋に正規表現isearchの置き換えにならない
(use-package ivy
  :init
  (bind-key "C-s" 'my/isearch-forward-or-swiper)
  :config
  (fset 'ivy--regex 'identity))

;; ace-jump-mode
(use-package ace-jump-mode
  :config
  (setq ace-jump-mode-gray-background nil)
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
  ;; (ace-pinyin-global-mode 1)
  :bind
  ("H-i" . ace-jump-word-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ replace                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 置換のキーバイドを変更
(bind-key "C-c r" 'vr/query-replace)
;; 正規表現置換のキーバイドを変更
;; (bind-key "C-c C-r" 'query-replace-regexp)

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

;; バッファの内容を自動保管 (秒)
(use-package auto-save-buffers-enhanced
  :init
  (unless (require 'auto-save-buffers-enhanced nil t)
    (install-elisp "https://raw.githubusercontent.com/kentaro/auto-save-buffers-enhanced/master/auto-save-buffers-enhanced.el"))
  :config
  (setq auto-save-buffers-enhanced-interval 10) ; 指定のアイドル秒で保存
  (auto-save-buffers-enhanced t))

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;;; dired-x
(use-package dired-x)

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
             ("r" . wdired-change-to-wdired-mode)))

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
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

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
;; 代わりにC-c h をヘルプに
(use-package bind-key
  :config
  (bind-keys :map global-map
             ("C-h" . delete-backward-char)
             ("C-c h" . help-command)))

;; 行の折り返し表示の切替
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ my macros                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
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
  (helm-migemo-mode 1)
  (require 'helm-files)
  (use-package helm-ls-git
    :config
    (custom-set-variables
     '(helm-truncate-lines t)
     '(helm-delete-minibuffer-contents-from-point t)
     '(helm-mini-default-sources '(helm-source-buffers-list
                                   helm-source-files-in-current-dir
                                   helm-source-ls-git
                                   helm-source-recentf))))
  ;; helm-miniに表示するものをカスタマイズ
  ;; キーバインドを設定
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-m")     'helm-mini)
  (global-set-key (kbd "C-x f")   'helm-find)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-;")     'helm-recentf)
  (global-set-key (kbd "C-c b")   'helm-browse-project)
  (global-set-key (kbd "C-c o")   'helm-swoop)
  (global-set-key (kbd "C-c s")   'helm-ag)
  (global-set-key (kbd "C-c C-s") 'helm-do-ag-project-root)
  (global-set-key (kbd "M-y")     'helm-show-kill-ring)
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

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ git                                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(use-package magit
  :bind
  ("C-c g" . magit-status)
  :config
  (push '("^\*magit*" :regexp t :height 0.5) popwin:special-display-config))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (smartrep-define-key
      global-map "C-x" '(("p" . 'git-gutter:previous-hunk)
                         ("n" . 'git-gutter:next-hunk))))

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

(defun smartchr-keybindings-web ()
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " "++" " += " )))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= " "**")))
  (local-set-key (kbd "%")  (smartchr '("%" "%`!!'%" " % " " %= ")))
  (local-set-key (kbd "=")  (smartchr '("=" " = " " === " )))
  (local-set-key (kbd "<")  (smartchr '("<" " < " " << " " <= ")))
  (local-set-key (kbd ">")  (smartchr '(">" " > " " => " " >= ")))
  (local-set-key (kbd "!")  (smartchr '("!" " !== " " != ")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" "/`!!'/" " / " " /= ")))
  (local-set-key (kbd "(")  (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[")  (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" "{{ `!!' }}" "{")))
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
;;; @ Other                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; edit-server
;; edit browser contents usgin emacs with Google Chrome
(use-package edit-server
  :init
  (add-hook 'edit-server-mode #'markdown-mode)
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

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


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ After loaded                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; 画面最大化
(set-frame-parameter nil 'fullscreen 'maximized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-files-in-current-dir helm-source-ls-git helm-source-recentf)))
 '(helm-truncate-lines t t)
 '(package-selected-packages
   (quote
    (rust-mode key-chord key-combo init-loader auto-highlight-symbol flex-autopair hlinum midje-mode python-mode company zenburn-theme yaxception yasnippet yascroll yaml-mode window-layout wgrep weblogger web-mode volatile-highlights visual-regexp-steroids use-package undo-tree tuareg swiper spinner solarized-theme smooth-scroll smex smartparens slamhound skewer-mode scss-mode scala-mode ruby-end rspec-mode robe rbenv rainbow-mode rainbow-delimiters quickrun queue powerline popwin php-mode peg paredit pallet nginx-mode multiple-cursors markdown-mode magit log4e jsx-mode json-mode jedi inflections image-dired+ image+ ido-ubiquitous hydra htmlize ht highlight-symbol helm-themes helm-swoop helm-robe helm-projectile helm-migemo helm-ls-git helm-descbinds helm-ag haskell-mode groovy-mode google-translate gitignore-mode gitconfig-mode git-gutter-fringe gist flycheck-pos-tip expand-region exec-path-from-shell)))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(zou.framework.repl/cljs-repl)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
