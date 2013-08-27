;;; aethanyc-editor.el --- Enhance editing experience

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file enhances editing experience.

;;; Code:

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

;; ido mode
(ido-mode 1)
(ido-ubiquitous-mode 1)
(setq-default ido-enable-flex-matching t)
(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-max-prospects 8)
(setq-default ido-save-directory-list-file (concat aethanyc-savefiles-dir "ido-last"))

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


;;;-------------------------------------------------------------------
;;; Eshell

(setq eshell-directory-name (concat aethanyc-savefiles-dir "eshell/"))

;; Generate Eshell alias from bash alias
;; http://www.emacswiki.org/emacs/EshellAlias#toc8
;; alias | sed -E "s/^alias ([^=]+)='(.*)'$/alias \1 \2 \$*/g; s/'\\\''/'/g;"
(setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))


(use-package ace-jump-mode
  :init (setq ace-jump-mode-gray-background nil
              ace-jump-mode-case-fold nil)
  :bind (("M-a" . ace-jump-word-mode)
         ("M-A" . ace-jump-char-mode))
  :ensure ace-jump-mode)


(use-package gitconfig-mode
  :ensure gitconfig-mode)


(use-package gitignore-mode
  :ensure gitignore-mode)


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
  :ensure markdown-mode)


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
