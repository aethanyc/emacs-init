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

;; Bind <apps> to the more powerful execute-extended-command - smex
;; <apps> is the key to the right of the right windows key with a menu on it.
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "<apps>") 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-<menu>") 'smex-major-mode-commands)
(global-set-key (kbd "M-<apps>") 'smex-major-mode-commands)

;; Open user-init-file
(global-set-key (kbd "<f12>")
                (lambda ()
                  (interactive)
                  (find-file user-init-file)))

(global-set-key (kbd "M-/") 'hippie-expand)

;; Toggle from 3 cases: UPPER CASE, lower case, and Title Case.
(global-set-key (kbd "C-z") 'toggle-letter-case)

;; Using S-arrow keys to move between windows.
(windmove-default-keybindings)

;; C-c keybindings
(global-set-key (kbd "C-c b") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-c f") 'ace-jump-mode)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c J") 'join-line)
(global-set-key (kbd "C-c j") 'join-next-line)
(global-set-key (kbd "C-c r") 'revert-buffer)
