;;; aethanyc-alias.el --- Function aliases -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Give frequently used functions shorter names.

;;; Code:

;; y or n is enough.
(defalias 'yes-or-no-p #'y-or-n-p)

;; Use ibuffer.
(defalias 'list-buffers #'ibuffer)


(provide 'aethanyc-alias)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-alias.el ends here
