;;;-------------------------------------------------------------------
;;; Keybindings

;; Frame operations
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'ctl-x-4-prefix)
(global-set-key (kbd "M-5") 'ctl-x-5-prefix)
(global-set-key (kbd "M-o") 'other-window)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current buffer.
(global-set-key (kbd "C-x C-i") 'imenu)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Start an eshell or switch to it if there is one.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if there is an active one.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Apropos more info.
(define-key 'help-command "a" 'apropos)
;;(setq-default apropos-do-all t)

;; Bind <apps> to the more powerful execute-extended-command.
;; <apps> is the key to the right of the right windows key with a menu on it.
(global-set-key (kbd "<apps>") 'smex)

;; Open user-init-file
(global-set-key (kbd "<f12>")
                (lambda ()
                  (interactive)
                  (find-file user-init-file)))
