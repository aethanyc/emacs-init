;;;-------------------------------------------------------------------
;;; Lisp Mode

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

;;;-------------------------------------------------------------------
;;; C/C++ Mode

(defun my-c-mode-common-hook ()
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

;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode) t)

;; Ace Jump Mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(ace-jump-mode-enable-mark-sync)

;; Undo Tree Mode
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Smex
(setq smex-save-file (concat aethanyc-save-file-directory "smex-items"))
(smex-initialize)

;; Load Ergoemacs functions
(load "functions")
