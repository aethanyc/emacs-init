;;; aethanyc-packages.el --- Initialize package system

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets up Emacs 24 package system and adds popular archives
;; such as MELPA.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Emacs will activated packages after reading the init file, but it is too
;; late. We disable it and call (package-initialize) ourselves.
(setq package-enable-at-startup nil)
(package-initialize)


;; Require use-package that are essential to customization.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; To see the summary of all the personal keybinds:
;; M-x describe-personal-keybindings
(use-package bind-key
  :ensure bind-key)

(provide 'aethanyc-packages)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-packages.el ends here
