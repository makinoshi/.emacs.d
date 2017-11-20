;;; 55_clojure.el --- For Clojure                    -*- lexical-binding: t; -*-

(defun my/clojure-mode-hook ()
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
  (use-package cider-mode
    :init
    (add-hook 'cider-mode-hook 'clj-refactor-mode)
    (add-hook 'cider-mode-hook 'company-mode)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-repl-mode-hook 'eldoc-mode)
    :diminish subword-mode
    :config
    (setq nrepl-log-messages t
          cider-repl-display-in-current-window t
          cider-repl-use-clojure-font-lock t
          cider-save-file-on-load t
          cider-font-lock-dynamically '(macro core function var)
          cider-overlays-use-font-lock t))
  (use-package clj-refactor
    :config
    (cljr-add-keybindings-with-prefix "C-c j")
    (setq cljr-favor-prefix-notation nil))
  (use-package midje-mode))

(defun my/zou-go ()
  "zou go commnad"
  (interactive)
  (with-current-buffer (cider-current-connection "clj")
    (if current-prefix-arg
        (progn
          (save-some-buffers)
          (cider-interactive-eval
           "(zou.framework.repl/reset)"))
      (cider-interactive-eval
       "(zou.framework.repl/go)"))))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode)
  (add-hook 'clojure-mode-hook 'auto-highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'highlight-symbol-mode)
  (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)
  :bind
  ("C-c C-g" . my/zou-go)
  :config
  (put-clojure-indent 'fnk 'defun)
  (put-clojure-indent 'defnk 'defun)
  (put-clojure-indent 'for-map 1)
  (put-clojure-indent 'instance 2)
  (put-clojure-indent 'inline 1)
  (put-clojure-indent 'letk 1)
  (put-clojure-indent 'when-letk 1)
  (put-clojure-indent 'go-loop 1)
  (put-clojure-indent 'this-as 'defun)
  (put-clojure-indent 'when-some '1)
  (put-clojure-indent 'if-some '1)
  (put-clojure-indent 'try+ 0)
  (put 'specify 'clojure-backtracking-indent '((2)))
  (put 'specify! 'clojure-backtracking-indent '((2)))
  (put 'defcomponent 'clojure-backtracking-indent '((2)))
  (put 'defcomponentk 'clojure-backtracking-indent '((2)))
  (put 'defmixin 'clojure-backtracking-indent '((2)))
  (put 'clojure.core/defrecord 'clojure-backtracking-indent '(4 4 (2)))
  (put 's/defrecord 'clojure-backtracking-indent '(4 4 (2)))
  (put 's/defrecord+ 'clojure-backtracking-indent '(4 4 (2)))
  (put 'potemkin/deftype+ 'clojure-backtracking-indent '(4 4 (2)))
  (put 'potemkin/defrecord+ 'clojure-backtracking-indent '(4 4 (2))))
