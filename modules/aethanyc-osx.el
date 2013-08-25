;;; aethanyc-osx.el --- Settings for Mac OS X

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file has settings specific to Mac OS X.

;;; Code:

;; When Emacs is launched by UI, it does not seen the PATH from the
;; shell. Let's fix this.
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-right-option-modifier 'control)

(setq mouse-wheel-scroll-amount '(0.01))

(provide 'aethanyc-osx)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-osx.el ends here
