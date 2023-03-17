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

;; `use-package' is essential. Ensure it's installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (defvar use-package-enable-imenu-support t)
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

;; To see the summary of all the personal keybinds:
;; M-x describe-personal-keybindings
(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

(use-package no-littering
  :ensure t)

(provide 'aethanyc-packages)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-packages.el ends here
