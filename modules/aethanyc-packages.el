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

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; On-demand installation of packages
;; From https://github.com/purcell/emacs.d/blob/master/init-elpa.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Emacs will activated packages after reading the init file, but it is too
;; late. We disable it and call (package-initialize) ourselves.
(setq package-enable-at-startup nil)
(package-initialize)

;; Require use-package that are essential to customization.
(require-package 'use-package)
(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

;; To see the summary of all the personal keybinds:
;; M-x describe-personal-keybindings
(use-package bind-key
  :ensure bind-key)

(provide 'aethanyc-packages)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-packages.el ends here
