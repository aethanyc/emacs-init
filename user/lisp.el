;;;-------------------------------------------------------------------
;;; Lisp Settings

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c v") 'eval-buffer)))
(add-hook 'emacs-lisp-mode-hook 'whitespace-cleanup)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;;;-------------------------------------------------------------------
;;; References
;; To bytecompile mulitple files, mark them in dired, and then press B.
