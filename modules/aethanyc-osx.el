;;; aethanyc-osx.el --- Settings for Mac OS X

;; Copyright (C) 2013-2021 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has settings specific to Mac OS X.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; When Emacs is launched by UI, it does not seen the PATH from the
;; shell. Let's fix this.
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(setq mouse-wheel-scroll-amount '(0.01))

;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
(add-to-list 'completion-ignored-extensions ".DS_Store")

(provide 'aethanyc-osx)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-osx.el ends here
