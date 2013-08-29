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

(defvar aethanyc-savefiles-dir (locate-user-emacs-file "savefiles/")
  "This folder contains all the automatically generated files.")

(defvar aethanyc-site-lisp-dir (locate-user-emacs-file "site-lisp/")
  "This folder contains other packages that are not installed
throught MELPA.")

;; Create directory if it does not exist.
(unless (file-exists-p aethanyc-savefiles-dir)
  (make-directory aethanyc-savefiles-dir))

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path aethanyc-modules-dir)
(add-to-list 'load-path aethanyc-site-lisp-dir)

(defvar user-init-files '(
                          "mode-hooks"
                          )
  "The names of my customization files.")

;; Set coding system to utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)

;; Use common-lisp utilities.
(require 'cl)

(require 'aethanyc-packages)

(require 'aethanyc-core)

(when (eq system-type 'darwin)
    (require 'aethanyc-osx))

(require 'aethanyc-ui)

(require 'aethanyc-editor)

(require 'aethanyc-keybindings)

(require 'aethanyc-alias)

;; Load all modules.
(dolist (file user-init-files)
  (load (concat aethanyc-modules-dir file)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
