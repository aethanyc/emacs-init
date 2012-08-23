;;;-------------------------------------------------------------------
;;; Lisp Settings

(defun my-lisp-common-hook ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)
  (local-set-key (kbd "C-c v") 'eval-buffer)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(add-hook 'lisp-mode-hook 'my-lisp-common-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-common-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-common-hook)

;;;---------------------------------------------------------------------------
;;; C/C++ Settings

(defun my-c-mode-common-hook ()
  (electric-pair-mode 1)
  (subword-mode 1)
  (c-set-style "stroustrup")
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'arglist-intro 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'member-init-intro 0)
  (c-set-offset 'inline-open 0)
  (define-key c-mode-map (kbd "RET") 'c-context-line-break)
  (define-key c++-mode-map (kbd "RET") 'c-context-line-break))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;-------------------------------------------------------------------
;;; References
;; To bytecompile mulitple files, mark them in dired, and then press B.
;; http://emacswiki.org/emacs/ParEdit
