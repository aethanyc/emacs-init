;;; init.el --- Entry point of Aethanyc's configuration

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets default paths and requires various modules.

;;; Code:

(defvar aethanyc-modules-dir (locate-user-emacs-file "modules/")
  "This folder contains all the customization files.")

(defvar aethanyc-site-lisp-dir (locate-user-emacs-file "site-lisp/")
  "This folder contains other packages that are not installed throught MELPA.")

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path aethanyc-modules-dir)
(let ((default-directory aethanyc-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Reduce the frequency of garbage collection.
(setq gc-cons-threshold (* 50 1024 1024))

(require 'aethanyc-packages)
(require 'aethanyc-core)
(when (eq system-type 'darwin) (require 'aethanyc-osx))
(require 'aethanyc-ui)
(require 'aethanyc-editor)
(require 'aethanyc-programming)
(require 'aethanyc-keybindings)
(require 'aethanyc-alias)


;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
