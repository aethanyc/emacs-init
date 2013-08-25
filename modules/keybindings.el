;; -*- coding: utf-8-unix; -*-

;;;-------------------------------------------------------------------
;;; Keybindings

;; Basic keys
(global-set-key (kbd "RET") 'newline-and-indent)

;; Frame operations
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window-or-frame)
(define-key ctl-x-map "0" 'delete-window-or-frame)
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

;; Start an eshell or switch to it if there is one.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if there is an active one.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Apropos more info.
(define-key 'help-command "a" 'apropos)
;;(setq-default apropos-do-all t)

;; Open user init files quickly
(global-set-key (kbd "<f12>") 'find-user-init-file)

;; `hippie-expand' is more powerful than dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Set keybindings to the functions in ErgoEmacs's functions.el
;; (global-set-key (kbd "C-z") 'ergoemacs-toggle-letter-case)
;; (global-set-key (kbd "M-6") 'ergoemacs-select-current-line)
;; (global-set-key (kbd "M-7") 'ergoemacs-select-current-block)
;; (global-set-key (kbd "C-w") 'ergoemacs-cut-line-or-region)
;; (global-set-key (kbd "M-w") 'ergoemacs-copy-line-or-region)

;; Navigating forward and backward on marks
(global-set-key (kbd "C-M-b") 'back-button-global-backward)
(global-set-key (kbd "C-M-f") 'back-button-global-forward)

;; C-c keybindings
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c J") 'join-line)
(global-set-key (kbd "C-c j") 'join-next-line)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Highlight symbol
(global-set-key (kbd "<C-f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f3>") 'highlight-symbol-prev)
(global-set-key (kbd "<M-S-f3>") 'highlight-symbol-query-replace)

;; Auto Complete Mode
(global-set-key (kbd "<C-tab>") 'auto-complete)

;; Expand-region
(global-set-key (kbd "M-8") 'er/expand-region)

;; multiple-cursors
(global-set-key (kbd "M-9") 'mc/mark-next-like-this)
