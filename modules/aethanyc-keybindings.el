;;; aethanyc-keybindings.el --- Useful Keybindings

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets a lot of useful keybindings.

;;; Code:

;; Basic keys
(global-set-key (kbd "RET") 'newline-and-indent)

;; Frame operations
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-0") 'aethanyc-delete-window-or-frame)
(define-key ctl-x-map "0" 'aethanyc-delete-window-or-frame)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'ctl-x-4-prefix)
(global-set-key (kbd "M-5") 'ctl-x-5-prefix)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Apropos more info.
(define-key 'help-command "a" 'apropos)
;;(setq-default apropos-do-all t)

;; Open user init files quickly
(global-set-key (kbd "<f12>") 'aethanyc-find-user-init-file)

;; `hippie-expand' is more powerful than dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Set keybindings to the functions in ErgoEmacs's functions.el
;; (global-set-key (kbd "C-z") 'ergoemacs-toggle-letter-case)
;; (global-set-key (kbd "M-6") 'ergoemacs-select-current-line)
;; (global-set-key (kbd "M-7") 'ergoemacs-select-current-block)
;; (global-set-key (kbd "C-w") 'ergoemacs-cut-line-or-region)
;; (global-set-key (kbd "M-w") 'ergoemacs-copy-line-or-region)

;; C-c keybindings
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c J") 'join-line)
(global-set-key (kbd "C-c j") 'aethanyc-join-next-line)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Expand-region
(global-set-key (kbd "M-8") 'er/expand-region)

;; multiple-cursors
(global-set-key (kbd "M-9") 'mc/mark-next-like-this)


(provide 'aethanyc-keybindings)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-keybindings.el ends here