;;; init.el --- Entry point of my configuration -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets default paths and requires various modules.

;;; Code:

(defvar aethanyc-modules-dir (locate-user-emacs-file "modules/")
  "This folder contains all the customization files.")

;; Add directories to Emacs's `load-path'.
(add-to-list 'load-path aethanyc-modules-dir)

;; Reduce the frequency of garbage collection.
(setq gc-cons-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(require 'aethanyc-packages)
(require 'aethanyc-core)
(when (eq system-type 'darwin) (require 'aethanyc-osx))
(require 'aethanyc-ui)
(require 'aethanyc-editor)
(require 'aethanyc-programming)
(require 'aethanyc-keybindings)
(require 'aethanyc-alias)

;; Set custom-file path to avoid cluttering init.el.
(setq custom-file (no-littering-expand-var-file-name "custom.el"))
(if (not (file-readable-p custom-file))
    (with-temp-buffer
      (write-file custom-file)))
(load custom-file)


;; Local Variables:
;; coding: utf-8
;; End:

;;; init.el ends here
