;;; aethanyc-editor.el --- Enhance editing experience

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file enhances editing experience.

;;; Code:

;; Unicode is good.
(prefer-coding-system 'utf-8)

;; Solution to the problem "server directory is unsafe on Windows."
;; http://stackoverflow.com/questions/5233041/emacs-and-the-server-unsafe-error
(eval-after-load 'server
  '(progn
     (setq server-auth-dir aethanyc-savefiles-dir)))

;; Delete the seleted text when typing.
(delete-selection-mode 1)

;; Paste at the cursor position.
(setq mouse-yank-at-point t)

;; Do not use tabs to indent code.
(setq-default indent-tabs-mode nil)

;; Set the default mode as text-mode.
(setq-default major-mode 'text-mode)

;; Adjust the behavior of the hippie-expand
(setq-default hippie-expand-try-functions-list
              '(try-complete-file-name
                try-expand-all-abbrevs
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol
                try-complete-file-name-partially
                try-expand-list
                try-expand-line))

;; Set whitespace mode styles
(setq-default whitespace-style '(face trailing lines-tail tabs empty indentation))

;; Save the last place of the cursor.
(setq-default save-place t)
(setq-default save-place-file (concat aethanyc-savefiles-dir "save-place"))
(require 'saveplace)

;; Stop creating those backup~ files
(setq-default make-backup-files nil)

;; Stop creating those #autosave# files
(setq-default auto-save-default nil
              auto-save-list-file-prefix nil)

;; Keep a list of recently opened files
(setq-default recentf-save-file (concat aethanyc-savefiles-dir "recentf"))
(recentf-mode 1)

;; Unique buffer name
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)

;; Remove the all the version control backends to increase file open speed.
;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
(setq vc-handled-backends nil)

;; Do Not kill the whole line including the line ending. When using
;; paredit-mode to kill the whole s-expr, this will kill the blank
;; line after the s-expr.
(setq kill-whole-line nil)

;;;-------------------------------------------------------------------
;;; Desktop Save Mode
;;; http://www.emacswiki.org/DeskTop#toc5

(setq desktop-path (list aethanyc-savefiles-dir))
(setq desktop-dirname aethanyc-savefiles-dir)
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-base-lock-name "emacs-desktop-lock")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
          '(lambda ()
             ;; desktop-remove clears desktop-dirname
             (setq desktop-dirname-tmp desktop-dirname)
             (desktop-remove)
             (setq desktop-dirname desktop-dirname-tmp)))

(defun desktop-session-saved-p ()
  (file-exists-p (concat desktop-dirname desktop-base-file-name)))

;; Use session-restore to restore the desktop manually.
(defun desktop-restore-session ()
  "Restore a saved emacs session."
  (interactive)
  (if (desktop-session-saved-p)
      (desktop-read)
    (message "No desktop found.")))

;; Use session-save to save the desktop manually.
(defun desktop-save-session ()
  "Save an emacs session."
  (interactive)
  (if (desktop-session-saved-p)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; Ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (if (desktop-session-saved-p)
;;                  (if (y-or-n-p "Restore desktop? ")
;;                      (desktop-restore-session)))))



(use-package ace-jump-mode
  :init (setq ace-jump-mode-gray-background nil
              ace-jump-mode-case-fold nil)
  :bind (("M-m" . ace-jump-word-mode)
         ("M-M" . ace-jump-char-mode))
  :ensure ace-jump-mode)


(use-package ack-and-a-half
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))
  :ensure ack-and-a-half)


(use-package auto-complete
  :init
  (progn
    (require 'auto-complete-config)
    (ac-config-default)            ; Enables global-auto-complete-mode
    (setq-default ac-sources
                  '(ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers
                    ac-source-words-in-all-buffer))    (ac-linum-workaround)
    (setq ac-use-menu-map t
          ac-auto-show-menu t)
    (setq ac-comphist-file
          (expand-file-name "ac-comphist.dat" aethanyc-savefiles-dir))
    (setq ac-modes (append ac-modes
                           '(git-commit-mode
                             html-mode
                             markdown-mode
                             nxml-mode
                             org-mode
                             sh-mode
                             text-mode))))
  :bind ("<C-tab>" . auto-complete)
  :ensure auto-complete)


;; Although back-button is available in melpa, it depends on too many
;; packages that are not strictly required. So I add it directly to
;; the repository.
;; https://raw.github.com/rolandwalker/back-button/master/back-button.el
(use-package back-button
  :init
  (progn
    (defalias 'push-mark 'back-button-push-mark-local-and-global
      "Replace push-mark to preserve current position before jumping around.")
    (bind-key* "C-M-b" 'back-button-global-backward)
    (bind-key* "C-M-f" 'back-button-global-forward)
    (back-button-mode 1)))


(use-package eshell
  :init (setq eshell-directory-name
              (expand-file-name "eshell/" aethanyc-savefiles-dir))
  :bind ("C-x m" . eshell))


(use-package expand-region
  :bind ("M-n" . er/expand-region)
  :ensure expand-region)


(use-package gitconfig-mode
  :defer t
  :ensure gitconfig-mode)


(use-package gitignore-mode
  :defer t
  :ensure gitignore-mode)


(use-package ido
  :init
  (progn
    (setq ido-create-new-buffer 'always
          ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t)
    (setq ido-save-directory-list-file
          (expand-file-name "ido-last" aethanyc-savefiles-dir))
    (ido-mode 1)
    (ido-everywhere 1)))


(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1)
  :ensure ido-ubiquitous)


(use-package flx-ido
  :init
  (progn
    ;; Disable ido faces to see flx highlights.
    (setq ido-use-faces nil)
    (flx-ido-mode 1))
  :ensure flx-ido)


(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (progn
    ;; https://github.com/purcell/emacs.d/blob/master/init-git.el
    (defadvice magit-status (around magit-fullscreen activate)
      "Show magit status in full frame."
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer."
      (interactive)
      (kill-buffer)
      (when (get-register :magit-fullscreen)
        (ignore-errors
          (jump-to-register :magit-fullscreen))))

    (bind-key "q" 'magit-quit-session magit-status-mode-map))
  :ensure magit)


(use-package markdown-mode
  :defer t
  :ensure markdown-mode)


(use-package paredit
  :bind (("M-B" . paredit-backward)
         ("M-F" . paredit-forward))
  :config (use-package paredit-menu
            :ensure paredit-menu)
  :ensure paredit)


(use-package projectile
  :init
  (progn
    (setq projectile-cache-file
          (expand-file-name "projectile.cache" aethanyc-savefiles-dir)
          projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" aethanyc-savefiles-dir))
    (projectile-global-mode 1))
  :ensure projectile)


(use-package smex
  :init (setq smex-save-file (concat aethanyc-savefiles-dir "smex-items"))
  :bind (("<menu>" . smex)
         ("<apps>" . smex) ; the key with a menu icon
         ("M-x" . smex)
         ("s-a" . smex)
         ("M-<menu>" . smex-major-mode-commands)
         ("M-<apps>" . smex-major-mode-commands)
         ("s-A" . smex-major-mode-commands))
  :ensure smex)


(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind ("C-=" . undo-tree-redo)
  :ensure undo-tree)


(provide 'aethanyc-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-editor.el ends here
