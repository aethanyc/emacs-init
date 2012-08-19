;;;-------------------------------------------------------------------
;;; Editing Settings

;; Delete the seleted text when typing.
(delete-selection-mode 1)

;; Paste at the cursor position.
(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              mouse-yank-at-point t
              major-mode 'text-mode)

;; Adjust the behavior of the hippie-expand
(setq-default hippie-expand-try-functions-list
              '(try-complete-file-name
                try-expand-all-abbrevs
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol
                try-complete-file-name-partially))
(global-set-key (kbd "M-/") 'hippie-expand)

;; Toggle from 3 cases: UPPER CASE, lower case, and Title Case.
(global-set-key (kbd "C-z") 'toggle-letter-case)

;; Whitespace settings
;(global-whitespace-mode 1)
(setq-default whitespace-style '(face trailing lines-tail tabs empty indentation))
(global-set-key (kbd "C-c n") 'whitespace-cleanup)

;; Save place: the cursor goes to the last place.
(require 'saveplace)
(setq-default save-place t
              save-place-file (concat user-emacs-directory "save-place"))


;;;-------------------------------------------------------------------
;;; Text Mode Settings

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook
          (lambda ()
            ;; Hard-wrap/un-hard-wrap paragraph
            (local-set-key (kbd "M-q") 'compact-uncompact-block)))
