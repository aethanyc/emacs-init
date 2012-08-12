;;;-------------------------------------------------------------------
;;; Lisp Settings

;; for common-lisp functions
(require 'cl)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c v") 'eval-buffer)))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;;;-------------------------------------------------------------------
;;; References
;; To bytecompile mulitple files, mark them in dired, and then press B.
