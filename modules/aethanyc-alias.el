;;; aethanyc-alias.el --- Function aliases

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Give frequently used functions shorter names.

;;; Code:

;; y or n is enough.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use ibuffer.
(defalias 'list-buffers 'ibuffer)

;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(defalias 'gl 'goto-line)
(defalias 'lp 'list-packages)
(defalias 'mb 'magit-blame-mode)
(defalias 'rf 'restore-frame)
(defalias 'sl 'sort-lines)

(provide 'aethanyc-alias)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-alias.el ends here
