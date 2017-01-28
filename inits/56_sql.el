;;; 56_sql.el --- For sql                            -*- lexical-binding: t; -*-

;; C-c C-c : 'sql-send-paragraph
;; C-c C-r : 'sql-send-region
;; C-c C-s : 'sql-send-string
;; C-c C-b : 'sql-send-buffer
;; C-c C-c : 'sql-send-paragraph
;; C-c C-r : 'sql-send-region
;; C-c C-s : 'sql-send-string
;; C-c C-b : 'sql-send-buffer
(defun sql-mode-hooks ()
  "For sql coding."
  (interactive "P")
  (setq sql-indent-offset 2
        indent-tabs-mode nil)
  (sql-set-product "postgres")
  (smart-newline-mode t)
  (use-package sql-indent))

(use-package sql
  :init
  (add-hook 'sql-mode-hook 'sql-mode-hooks))
