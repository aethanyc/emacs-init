;;; aethanyc-packages.el --- Initialize package system -*- lexical-binding:t -*-

;; Copyright (C) 2013-2023 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets up Emacs package system and adds popular archives
;; such as MELPA.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (defvar use-package-enable-imenu-support t)
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

(use-package diminish
  :ensure t)

(use-package no-littering
  :config
  (no-littering-theme-backups)
  :ensure t)

(provide 'aethanyc-packages)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-packages.el ends here
