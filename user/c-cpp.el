;;;---------------------------------------------------------------------------
;;; C/C++ Settings

;;; TODO: C/C++ Settings need to be revised.
(defun my-c-mode-common-hook ()
  (electric-pair-mode 1)
  (subword-mode 1)
;  (setq fill-column 80)
  (c-set-style "stroustrup")
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'arglist-intro 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'member-init-intro 0)
  (c-set-offset 'inline-open 0)
  (define-key c-mode-map (kbd "RET") 'c-context-line-break)
  (define-key c++-mode-map (kbd "RET") 'c-context-line-break)
  (add-hook 'before-save-hook
            (lambda ()
              (whitespace-cleanup)))
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
