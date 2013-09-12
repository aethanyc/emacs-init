;;; aethanyc-osx.el --- Settings for Mac OS X

;; Copyright (C) 2013 Ting-Yu Lin

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
  :init (exec-path-from-shell-initialize)
  :ensure exec-path-from-shell)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-right-option-modifier 'control)

(setq mouse-wheel-scroll-amount '(0.01))

(provide 'aethanyc-osx)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-osx.el ends here
