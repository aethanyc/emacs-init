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
(require 'diminish)
(require 'bind-key)

(require 'aethanyc-core)

;; Unicode is good.
(prefer-coding-system 'utf-8)

;; Delete the seleted text when typing.
(delete-selection-mode 1)

;; Trigger reindentation when typing RET.
(electric-indent-mode 1)

;; Paste at the cursor position.
(setq mouse-yank-at-point t)

;; Do not use tabs to indent code.
(setq-default indent-tabs-mode nil)

;; Set the default mode as text-mode.
(setq-default major-mode 'text-mode)

;; Adjust the behavior of the hippie-expand
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list
        try-expand-line))

;; Stop creating those backup~ files
(setq make-backup-files nil)

;; Make auto-save files visible.
(setq auto-save-list-file-prefix (concat aethanyc-savefiles-dir "auto-save-"))

;; Auto save buffer for every 5 seconds.
(setq auto-save-timeout 5)

;; Create no interlocking files so I won't see them in git status.
(setq create-lockfiles nil)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)

;; Remove the all the version control backends to increase file open speed.
;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
(setq vc-handled-backends nil)

;; Let `fill-paragraph' leaves just one space at the end of the sentence.
(setq-default sentence-end-double-space nil)

;; Do not display message in scratch buffer.
(setq initial-scratch-message nil)

;; Set default fill column to 76.
(setq-default fill-column 76)

;; Add newline at the end automatically when the file is about to be saved.
(setq-default require-final-newline t)

;; Uniquify buffer name.
(setq uniquify-buffer-name-style 'forward)

;; Use text-mode for *scratch* buffer.
(setq initial-major-mode 'text-mode)

;; Variable-value paris that are considered safe in file local variables.
(setq safe-local-variable-values
      '((js-indent-level . 2)))


;; Built-in packages

;; Save the last place of the cursor.  This should be loaded as early
;; as possible.  I don't know why it fails to load save-place-file if
;; it is loaded too late.
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "places" aethanyc-savefiles-dir))
  (save-place-mode 1))


(use-package dired-x
  :bind ("C-c d" . dired-jump))


;; Check available dictionaries: hunspell -D. Name hunspell English
;; dictionary files as default.aff and default.dic, and put them under
;; ~/Library/Spelling on mac.
(use-package flyspell
  :config
  (cond ((executable-find "hunspell")
         (setq ispell-program-name "hunspell")
         (setq ispell-dictionary "default")
         (setq ispell-dictionary-alist
               '(("default" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'text-mode-hook #'flyspell-mode))


(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1)
  :diminish git-gutter-mode
  :ensure t)


(use-package org
  :defer t
  :config
  (setq org-ctrl-k-protect-subtree t
        org-special-ctrl-k t
        org-src-fontify-natively t
        org-startup-indented t
        org-startup-truncated nil
        org-special-ctrl-a/e t
        org-export-copy-to-kill-ring nil
        org-export-with-sub-superscripts (quote {}))

  ;; Highlight the source code in html exported.
  (use-package htmlize
    :ensure t)

  ;; Export org-mode content to reveal.js
  (use-package ox-reveal
    :init
    (defun aethanyc-org-reveal-save-then-export ()
      "Save buffer and then export to html."
      (interactive)
      (save-buffer)
      (org-reveal-export-to-html))
    :config
    (bind-key "<f5>" #'aethanyc-org-reveal-save-then-export org-mode-map)
    :ensure t))


;; Keep a list of recently opened files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" aethanyc-savefiles-dir)
        recentf-max-menu-items 20
        recentf-max-saved-items 500)
  (recentf-mode 1))


;; Save the history of minibuffer.
(use-package savehist
  :defer 5
  :init
  (setq savehist-file (expand-file-name "history" aethanyc-savefiles-dir))
  :config
  (savehist-mode 1))


(use-package sort
  :commands sort-lines
  :config
  (setq sort-fold-case t))


(use-package webjump
  :bind ("C-c j" . aethanyc-webjump-query-region)
  :config
  (defun aethanyc-webjump-query-region ()
    "Query webjump using the text in the active region."
    (interactive)
    (if (use-region-p)
        (let* ((completion-ignore-case t)
               (item (assoc-string
                      (completing-read "WebJump to site: " webjump-sites nil t)
                      webjump-sites t))
               (name (car item))
               (expr (cdr item)))
          (browse-url (webjump-url-fix
                       (if (not expr)
                           ""
                         (let ((nonquery-url (aref expr 1))
                               (query-prefix (aref expr 2))
                               (query-suffix (aref expr 3))
                               (query (buffer-substring (mark) (point))))
                           (concat query-prefix (webjump-url-encode query) query-suffix))))))
      (call-interactively #'webjump)))

  (setq webjump-sites
        '(("FileBrowser" . "")         ; Dummy entry to open file browser
          ("Bugzilla" .
           [simple-query "https://bugzilla.mozilla.org/"
                         "https://bugzilla.mozilla.org/buglist.cgi?quicksearch=" ""])
          ("DuckDuckGo" .
           [simple-query "duckduckgo.com"
                         "duckduckgo.com/?q=" ""])
          ("DXR" .
           [simple-query "dxr.mozilla.org"
                         "dxr.mozilla.org/mozilla-central/search?q=" ""])
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
          ("MozillaWiki" .
           [simple-query "wiki.mozilla.org"
                         "wiki.mozilla.org/Special:Search?search=" ""])
          ("MDN" .
           [simple-query "developer.mozilla.org/en-US"
                         "developer.mozilla.org/en-US/search?q=" ""])
          ("OxfordDictionary" .
           [simple-query "oaadonline.oxfordlearnersdictionaries.com"
                         "oaadonline.oxfordlearnersdictionaries.com/dictionary/" ""])
          ("Rust" .
           [simple-query "https://doc.rust-lang.org/std/"
                         "https://doc.rust-lang.org/std/?search=" ""])
          ("Searchfox" .
           [simple-query "http://searchfox.org"
                         "http://searchfox.org/mozilla-central/search?q=" ""])
          ("Servo" .
           [simple-query "https://doc.servo.org/servo/"
                         "https://doc.servo.org/servo/?search=" ""])
          ("StackOverflow" .
           [simple-query "stackoverflow.com"
                         "stackoverflow.com/search?q=" ""])
          ("Wikipedia" .
           [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
          ("Youtube" .
           [simple-query "www.youtube.com"
                         "www.youtube.com/results?search_query=" ""]))))


;; Packages

(use-package ace-link
  :config
  (ace-link-setup-default)
  :ensure t)


(use-package ace-window
  :bind* (("M-o" . ace-window))
  :config
  (setq aw-background nil)
  (setq aw-keys '(?h ?t ?n ?s ?u ?e ?o ?a))
  (setq aw-scope 'frame)
  :ensure t)


(use-package ag
  :defer t
  :ensure ag)


(use-package avy
  :bind (("M-m" . avy-goto-word-1)
         ("M-M" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line))
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-style 'at-full)
  :ensure t)


;; Although back-button is available in melpa, it depends on too many
;; packages that are not strictly required. So I add it directly as a
;; git submodule.
(use-package back-button
  :bind (("M-B" . backward-sexp)
         ("M-F" . forward-sexp))
  :bind* (("C-M-b" . back-button-global-backward)
          ("C-M-f" . back-button-global-forward))
  :config
  (defalias 'push-mark #'back-button-push-mark-local-and-global
    "Replace push-mark to preserve current position before jumping around.")
  (back-button-mode 1)
  :diminish back-button-mode)


(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  :ensure t)


(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major)
  :ensure t)


;;; http://www.emacswiki.org/DeskTop#toc5
(use-package desktop
  :init
  (setq desktop-path (list aethanyc-savefiles-dir))
  (setq desktop-dirname aethanyc-savefiles-dir)
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")

  :config
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

  (add-hook 'desktop-after-read-hook #'aethanyc-desktop-after-read-hook)
  (add-hook 'kill-emacs-hook #'aethanyc-desktop-save-on-exit)

  :bind (("<f9>" . desktop-revert)
         ("<M-f9>" . aethanyc-desktop-save)))


(use-package eshell
  :init (setq eshell-directory-name
              (expand-file-name "eshell/" aethanyc-savefiles-dir))
  :bind ("C-x m" . eshell))


(use-package expand-region
  :bind ("M-8" . er/expand-region)
  :ensure t)


(use-package flycheck
  :init
  ;; C-c ! ? to describe the syntax checker.
  (setq-default flycheck-clang-include-path '(".")
                flycheck-clang-language-standard "c++11"
                flycheck-emacs-lisp-initialize-packages t
                flycheck-emacs-lisp-load-path load-path
                flycheck-flake8-maximum-line-length 85)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  (use-package flycheck-rust
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    :ensure t)

  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :ensure t)


(use-package git-timemachine
  :defer t
  :ensure t)


(use-package gitconfig-mode
  :defer t
  :ensure t)


(use-package gitignore-mode
  :defer t
  :ensure t)


(use-package git-messenger
  :bind ("C-c m" . git-messenger:popup-message)
  :config (setq git-messenger:show-detail t)
  :ensure t)


(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-c !" "C-c p" "C-x r" "C-c C-c"))
  (guide-key-mode 1)
  :diminish guide-key-mode
  :ensure t)


(use-package ido
  :config
  (setq ido-create-new-buffer 'always
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t)
  (setq ido-save-directory-list-file
        (expand-file-name "ido-last" aethanyc-savefiles-dir))

  ;; It is easier to switch buffer on single key.
  (bind-key "<f2>" #'ido-switch-buffer)
  (bind-key "<f2>" #'ido-switch-buffer-other-window ctl-x-4-map)
  (bind-key "<f2>" #'ido-switch-buffer-other-frame ctl-x-5-map)

  (ido-mode 1)
  (ido-everywhere 1)

  (use-package ido-completing-read+
    :config (ido-ubiquitous-mode 1)
    :ensure t)

  (use-package flx-ido
    :config
    ;; Disable ido faces to see flx highlights.
    (setq ido-use-faces nil)
    (flx-ido-mode 1)
    :ensure t)

  (use-package ido-vertical-mode
    :config
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)
    (ido-vertical-mode 1)
    :ensure t))


(use-package imenu-anywhere
  :bind ("C-c i" . imenu-anywhere)
  :ensure t)


(use-package keyfreq
  :defer 5
  :init
  (setq keyfreq-file (expand-file-name "keyfreq" aethanyc-savefiles-dir)
        keyfreq-file-lock (expand-file-name "keyfreq.lock" aethanyc-savefiles-dir))
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :ensure t)


(use-package magit
  :bind (("<f4>" . magit-status)
         ("<f5>" . magit-status)
         ("<C-f4>" . magit-dispatch-popup))
  :config
  (setq magit-bury-buffer-function #'magit-mode-quit-window)
  (setq magit-completing-read-function #'magit-ido-completing-read)
  (setq magit-revision-show-gravatars nil)

  (magit-auto-revert-mode 1)

  ;; Do not allow magit overrides the window management keys.
  ;; Bind those show level keys to C-x 1, C-x 2, etc.
  (dolist (i '(1 2 3 4))
    (let ((key-to-unbind (format "M-%d" i))
          (key-to-bind (format "C-x %d" i))
          (func-to-bind (intern (format "magit-section-show-level-%d-all" i))))
      (unbind-key key-to-unbind magit-mode-map)
      (bind-key key-to-bind func-to-bind magit-mode-map)))

  (use-package git-commit
    :config
    (setq git-commit-finish-query-functions
          (delete #'git-commit-check-style-conventions
                  git-commit-finish-query-functions))
    (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill))
  :ensure t)


(use-package mark-tools
  :commands list-marks
  :ensure t)


(use-package markdown-mode
  :defer t
  :ensure t)


(use-package multiple-cursors
  :init (setq mc/list-file (expand-file-name "mc-lists.el" aethanyc-savefiles-dir))
  :bind (("M-9" . mc/mark-next-like-this)
         ("C-c M-9" . mc/mark-all-like-this))
  :ensure t)


(use-package paredit
  :defer t
  :init
  (aethanyc-hook-into-modes 'paredit-mode
    '(lisp-mode-hook emacs-lisp-mode-hook))
  :config
  (bind-key "M-B" #'paredit-backward paredit-mode-map)
  (bind-key "M-F" #'paredit-forward paredit-mode-map)
  (bind-key "RET" #'paredit-newline paredit-mode-map)
  (use-package paredit-menu
    :ensure t)
  :diminish "Par"
  :ensure t)


(use-package paredit-everywhere
  :defer t
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
  :config
  (unbind-key "M-DEL" paredit-everywhere-mode-map)
  (unbind-key "M-d" paredit-everywhere-mode-map)
  :ensure t)


(use-package projectile
  :init
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" aethanyc-savefiles-dir))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" aethanyc-savefiles-dir))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode 1)
  :diminish projectile-mode
  :ensure t)


(use-package server
  :defer 5
  :init
  (setq server-auth-dir aethanyc-savefiles-dir)
  :config
  (unless (server-running-p)
    (server-start)))


(use-package smex
  :init (setq smex-save-file (concat aethanyc-savefiles-dir "smex-items"))
  :bind (("<menu>" . smex)
         ("<apps>" . smex) ; the key with a menu icon
         ("<f12>" . smex)
         ("M-x" . smex)
         ("M-<menu>" . smex-major-mode-commands)
         ("M-<apps>" . smex-major-mode-commands)
         ("<M-f12>" . smex-major-mode-commands))
  :ensure t)


(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (disable-smooth-scroll-for-function scroll-up-command)
  (disable-smooth-scroll-for-function scroll-down-command)
  :ensure t)


(use-package toml-mode
  :defer t
  :ensure t)


(use-package undo-tree
  :config (global-undo-tree-mode 1)
  :bind ("C-=" . undo-tree-redo)
  :diminish undo-tree-mode
  :ensure t)


(use-package whitespace
  :init
  (setq-default whitespace-style '(face trailing tab-mark))
  ;; Turn on whitespace-mode only in file buffers.
  (add-hook 'find-file-hook #'whitespace-mode)
  :diminish whitespace-mode)


(use-package whitespace-cleanup-mode
  :defer 5
  :config
  (global-whitespace-cleanup-mode 1)
  :diminish whitespace-cleanup-mode
  :ensure t)


(provide 'aethanyc-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-editor.el ends here
