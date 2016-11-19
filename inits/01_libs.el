;;; 01_libs.el --- require emacs lisp librarys        -*- lexical-binding: t; -*-

(require 'cl)
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'slime)
(require 'bind-key)

(unless (require 'use-package nil t)
  (defmacro use-package (&reset args)))
