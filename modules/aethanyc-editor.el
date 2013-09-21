;;; aethanyc-editor.el --- Enhance editing experience

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file enhances editing experience.

;;; Code:

(eval-when-compile
  (require 'use-package))

;; Unicode is good.
(prefer-coding-system 'utf-8)

;; Delete the seleted text when typing.
(delete-selection-mode 1)

;; Paste at the cursor position.
(setq mouse-yank-at-point t)

;; Do not use tabs to indent code.
(setq-default indent-tabs-mode nil)

;; Set the default mode as text-mode.
(setq-default major-mode 'text-mode)

;; Adjust the behavior of the hippie-expand
(setq hippie-expand-try-functions-list
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

;; Stop creating those backup~ files
(setq make-backup-files nil)

;; Stop creating those #autosave# files
(setq auto-save-default nil
      auto-save-list-file-prefix nil)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)

;; Remove the all the version control backends to increase file open speed.
;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
(setq vc-handled-backends nil)


;; Built-in packages

;; Save the last place of the cursor.  This should be loaded as early
;; as possible.  I don't know why it fails to load save-place-file if
;; it is loaded too late.
(use-package saveplace
  :init
  (progn
    (setq save-place-file (expand-file-name "places" aethanyc-savefiles-dir))
    (setq-default save-place t)))


;; Keep a list of recently opened files
(use-package recentf
  :init
  (progn
    (setq recentf-save-file (expand-file-name "recentf" aethanyc-savefiles-dir))
    (recentf-mode 1)))


;; Save the history of minibuffer.
(use-package savehist
  :init
  (progn
    (setq savehist-file (expand-file-name "history" aethanyc-savefiles-dir))
    (savehist-mode 1)))


(use-package webjump
  :bind ("C-c j" . webjump)
  :config
  (progn
    (setq webjump-sites
          '(("FileBrowser" . "")         ; Dummy entry to open file browser
            ("DuckDuckGo" .
             [simple-query "duckduckgo.com"
                           "duckduckgo.com/?q=" ""])
            ("EmacsWiki" .
             [simple-query "www.emacswiki.org"
                           "duckduckgo.com/?q="
                           "+site:emacswiki.org"])
            ("Github" .
             [simple-query "github.com"
                           "github.com/search?q=" "&ref=cmdform"])
            ("Google" .
             [simple-query "www.google.com"
                           "www.google.com/search?q=" ""])
            ("OxfordDictionary" .
             [simple-query "oaadonline.oxfordlearnersdictionaries.com"
                           "oaadonline.oxfordlearnersdictionaries.com/dictionary/" ""])
            ("StackOverflow" .
             [simple-query "stackoverflow.com"
                           "stackoverflow.com/search?q=" ""])
            ("Wikipedia" .
             [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
            ("Youtube" .
             [simple-query "www.youtube.com"
                           "www.youtube.com/results?search_query=" ""])))))


;; Packages

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
    ;; Packages that enhance auto-complete.
    (use-package fuzzy
      :ensure fuzzy)
    (use-package pos-tip
      :ensure pos-tip)

    (require 'auto-complete-config)
    (ac-config-default)            ; Enables global-auto-complete-mode
    (setq-default ac-sources
                  '(ac-source-abbrev
                    ac-source-yasnippet
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers
                    ac-source-words-in-all-buffer))
    (setq ac-use-menu-map t
          ac-auto-show-menu t
          ac-use-fuzzy t
          ac-quick-help-delay 0.7)
    (setq ac-comphist-file
          (expand-file-name "ac-comphist.dat" aethanyc-savefiles-dir))
    (setq ac-modes (append ac-modes
                           '(git-commit-mode
                             html-mode
                             inferior-emacs-lisp-mode
                             markdown-mode
                             nxml-mode
                             org-mode
                             sh-mode
                             text-mode))))
  :bind ("<C-tab>" . auto-complete)
  :diminish ""
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
    (back-button-mode 1))
  :diminish "")


;;; http://www.emacswiki.org/DeskTop#toc5
(use-package desktop
  :init
  (progn
    (require 'desktop)

    (setq desktop-path (list aethanyc-savefiles-dir))
    (setq desktop-dirname aethanyc-savefiles-dir)
    (setq desktop-base-file-name "desktop")
    (setq desktop-base-lock-name "desktop.lock")

    ;; Specify modes that need not to be saved.
    (dolist (mode '(dired-mode fundamental-mode help-mode))
      (add-to-list 'desktop-modes-not-to-save mode))

    (defun aethanyc-desktop-after-read-hook ()
      "Remove the desktop saved file after it's been read."
      ;; desktop-remove clears the desktop-dirname. Let's restore it.
      (let ((desktop-dirname-old desktop-dirname))
        (desktop-remove)
        (setq desktop-dirname desktop-dirname-old)))

    (defun aethanyc-desktop-save ()
      "Save the desktop in directory `desktop-dirname'."
      (interactive)
      (if (file-exists-p (desktop-full-file-name))
          (if (yes-or-no-p "Overwrite existing desktop? ")
              (desktop-save desktop-dirname t)
            (message "Desktop not saved."))
        (desktop-save desktop-dirname t))
      (message "Desktop saved in %s" (abbreviate-file-name desktop-dirname)))

    (defun aethanyc-desktop-save-on-exit ()
      "Save desktop automatically on exit only when it has been loaded."
      (interactive)
      (if (file-exists-p (desktop-full-lock-name))
          (aethanyc-desktop-save)))

    (add-hook 'desktop-after-read-hook 'aethanyc-desktop-after-read-hook)
    (add-hook 'kill-emacs-hook 'aethanyc-desktop-save-on-exit))

  :bind (("<f12>" . desktop-revert)
         ("<M-f12>" . aethanyc-desktop-save)))


(use-package eshell
  :init (setq eshell-directory-name
              (expand-file-name "eshell/" aethanyc-savefiles-dir))
  :bind ("C-x m" . eshell))


(use-package expand-region
  :bind ("M-8" . er/expand-region)
  :ensure expand-region)


(use-package flycheck
  :init
  (progn
    (setq-default flycheck-flake8-maximum-line-length 85)
    (add-hook 'after-init-hook 'global-flycheck-mode))
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :ensure flycheck)


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
    (ido-everywhere 1)

    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode 1)
      :ensure ido-ubiquitous)

    (use-package flx-ido
      :init
      (progn
        ;; Disable ido faces to see flx highlights.
        (setq ido-use-faces nil)
        (flx-ido-mode 1))
      :ensure flx-ido)))


(use-package imenu-anywhere
  :bind ("C-c i" . imenu-anywhere)
  :ensure imenu-anywhere)


(use-package keyfreq
  :init
  (progn
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))
  :ensure keyfreq)


(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (progn
    ;; Do not allow magit overrides the window management keys.
    ;; Bind those show level keys to C-x 1, C-x 2, etc.
    (dolist (i '(1 2 3 4))
      (let ((key-unbind (format "M-%d" i))
            (key-bind (format "C-x %d" i))
            (func-bind (intern (format "magit-show-level-%d-all" i))))
        (unbind-key 'key-unbind magit-mode-map)
        (bind-key 'key-bind func-bind magit-mode-map)))

    ;; Allow magit to restore provious window configuration after quitting.
    (setq magit-restore-window-configuration t))
  :ensure magit)


;; Install git-wip to ~/bin
;; https://raw.github.com/bartman/git-wip/master/git-wip
;; $ git config --global --add magit.extension wip-save
(when (executable-find "git-wip")
  (use-package magit-wip
    :init
    (progn
      (magit-wip-mode 1)
      (global-magit-wip-save-mode 1))))


(use-package markdown-mode
  :defer t
  :ensure markdown-mode)


(use-package paredit
  :init
  (progn
    (aethanyc-hook-into-modes 'paredit-mode
                              '(lisp-mode-hook emacs-lisp-mode-hook ielm-mode-hook)))
  :config
  (progn
    (bind-key "M-B" 'paredit-backward paredit-mode-map)
    (bind-key "M-F" 'paredit-forward paredit-mode-map)
    (use-package paredit-menu
      :ensure paredit-menu))
  :diminish "Par"
  :ensure paredit)


(use-package projectile
  :init
  (progn
    (setq projectile-cache-file
          (expand-file-name "projectile.cache" aethanyc-savefiles-dir)
          projectile-known-projects-file
          (expand-file-name "projectile-bookmarks.eld" aethanyc-savefiles-dir))
    (setq projectile-indexing-method 'alien)
    (projectile-global-mode 1))
  :diminish ""
  :ensure projectile)


(use-package server
  :init (setq server-auth-dir aethanyc-savefiles-dir))


(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always)
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1)
    (aethanyc-hook-into-modes 'turn-off-smartparens-mode
                              '(lisp-mode-hook emacs-lisp-mode-hook ielm-mode-hook)))
  :ensure smartparens)


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


(use-package smooth-scrolling
  :ensure smooth-scrolling)


(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind ("C-=" . undo-tree-redo)
  :diminish ""
  :ensure undo-tree)

;; Unique buffer name
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))


(use-package yasnippet
  :init
  (progn
    (yas-reload-all)
    (aethanyc-hook-into-modes 'yas-minor-mode
                              '(c-mode-common-hook
                                python-mode-hook)))
  :ensure yasnippet)


(provide 'aethanyc-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-editor.el ends here
