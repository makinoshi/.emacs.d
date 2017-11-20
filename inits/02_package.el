;;; 02_package.el --- package.el configuration       -*- lexical-binding: t; -*-

;; package管理の設定
(setq package-enable-at-startup nil)
(use-package package
  :config
  ;; パッケージリポジトリにMarmaladeを追加
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA"      . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通してロードする
  (fset 'package-desc-vers 'package--ac-desc-version)
  (package-initialize))

(use-package auto-async-byte-compile)
