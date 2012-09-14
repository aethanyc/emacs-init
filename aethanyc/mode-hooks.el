;;;-------------------------------------------------------------------
;;; Lisp Mode

(defun my-lisp-hook ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(defun my-lisp-edit-keybindings ()
  (local-set-key (kbd "C-c v") 'eval-buffer)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(defun my-lisp-edit-hook ()
  (my-lisp-hook)
  (my-lisp-edit-keybindings))

(add-hook 'lisp-mode-hook 'my-lisp-edit-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-edit-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-edit-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)

;;;-------------------------------------------------------------------
;;; C/C++ Mode

(defun my-c-mode-common-hook ()
  ;; Semantic mode
  ;; http://www.emacswiki.org/emacs/CEDET_Quickstart
  (semantic-mode 1)
  (semantic-decoration-mode 1)
  (semantic-highlight-func-mode 1)
  (semantic-idle-summary-mode 1)
  (semantic-show-unmatched-syntax-mode 1)
  (add-to-list 'ac-sources 'ac-source-semantic t)

  (electric-pair-mode 1)
  (subword-mode 1)
  (c-set-style "stroustrup")
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'arglist-intro 0)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'member-init-intro 0)
  (c-set-offset 'inline-open 0)
  (local-set-key (kbd "RET") 'c-context-line-break))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;-------------------------------------------------------------------
;;; Text Mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;-------------------------------------------------------------------
;;; Ibuffer Mode

(defun my-ibuffer-mode-hook ()
  (local-set-key (kbd "C-x C-f") 'ibuffer-ido-find-file))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

;;;-------------------------------------------------------------------
;;; Other

;; Clean up white spaces before saving a buffer.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'byte-compile-current-buffer)