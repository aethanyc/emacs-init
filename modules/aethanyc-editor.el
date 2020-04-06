;;; aethanyc-editor.el --- Enhance editing experience

;; Copyright (C) 2013-2019 Ting-Yu Lin

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

;; `yank' with the mouse selection.
(setq select-enable-primary t)

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

;; Set the size of default fill column to 80.
(setq-default fill-column 80)

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


(use-package abbrev
  :diminish)


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
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))


(use-package org
  :defer t
  :config
  (setq org-ctrl-k-protect-subtree t
        org-special-ctrl-k t
        org-src-fontify-natively t
        org-startup-indented t
        org-startup-truncated nil
        org-special-ctrl-a/e t)

  (use-package ox
    :config
    (setq org-export-with-sub-superscripts (quote {})))

  ;; Highlight the source code in html exported.
  (use-package htmlize
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
  :config
  (setq savehist-file (expand-file-name "history" aethanyc-savefiles-dir))
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
          ("Wikipedia" .
           [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))


;; ELPA or MELPA Packages

(use-package ace-window
  :bind* (("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?t ?n ?s ?u ?e ?o ?a))
  (setq aw-scope 'frame)
  :ensure t)


(use-package ag
  :defer t
  :ensure ag)


(use-package amx
  :init
  (setq amx-save-file (concat aethanyc-savefiles-dir "amx-items"))
  :ensure t)


(use-package avy
  :bind (("M-m" . avy-goto-word-1)
         ("M-M" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line))
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-style 'at-full)
  :ensure t)


;; back-button is available in melpa, but it depends on packages that are not
;; strictly required. So I load it from a git submodule.
;;
;; Note: if keybindings for (backward-sexp) and (forward-sexp) are changed,
;; remember to change those in paredit, too.
(use-package back-button
  :load-path "site-lisp/back-button/"
  :bind (("M-B" . backward-sexp)
         ("M-F" . forward-sexp))
  :bind* (("C-M-b" . back-button-local-backward)
          ("C-M-f" . back-button-local-forward))
  :config
  ;; Hard code the Unicode char.
  (setq back-button-spacer-char #xb7    ; Middle Dot
        back-button-thumb-char #x2299)  ; Circled Dot Operator
  (back-button-mode 1)
  :diminish)


(use-package counsel
  :config
  (counsel-mode 1)
  :diminish
  :ensure t)


(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major)
  :ensure t)


(use-package eshell
  :init (setq eshell-directory-name
              (expand-file-name "eshell/" aethanyc-savefiles-dir))
  :bind ("C-x m" . eshell))


(use-package expand-region
  :bind (("M-8" . er/expand-region)
         ("M-*" . er/contract-region))
  :ensure t)


(use-package flx
  :ensure t)


(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1)
  :diminish git-gutter-mode
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


(use-package imenu-anywhere
  :bind ("C-c I" . ivy-imenu-anywhere)
  :ensure t)


(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :requires flx
  :diminish
  :ensure t)


(use-package magit
  :bind (("<f4>" . magit-status)
         :map magit-mode-map
         ("M-1" . nil)                  ; was magit-section-show-level-1-all
         ("M-2" . nil)                  ; was magit-section-show-level-2-all
         ("M-3" . nil)                  ; was magit-section-show-level-3-all
         ("M-4" . nil)                  ; was magit-section-show-level-4-all
         ("5" . magit-section-show-level-1-all)
         ("6" . magit-section-show-level-2-all)
         ("7" . magit-section-show-level-3-all)
         ("8" . magit-section-show-level-4-all))
  :config
  (setq magit-bury-buffer-function #'magit-mode-quit-window)

  (magit-auto-revert-mode 1)

  (use-package git-commit
    :config
    (setq git-commit-style-convention-checks nil)
    (defun my-git-commit-setup ()
      (setq fill-column 72))
    (add-hook 'git-commit-setup-hook #'my-git-commit-setup)
    (remove-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill))
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
  :bind (:map paredit-mode-map
              ("M-B" . paredit-backward)
              ("M-F" . paredit-forward)
              ("RET" . paredit-newline))
  :config
  (use-package paredit-menu
    :ensure t)
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :diminish
  :ensure t)


(use-package paredit-everywhere
  :hook (prog-mode . paredit-everywhere-mode)
  :diminish
  :ensure t)


(use-package projectile
  :init
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" aethanyc-savefiles-dir))
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" aethanyc-savefiles-dir))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :demand t
  :diminish
  :ensure t)


(use-package ripgrep
  :ensure t)


(use-package server
  :defer 5
  :init
  (setq server-auth-dir aethanyc-savefiles-dir)
  :config
  (unless (server-running-p)
    (server-start)))


(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (disable-smooth-scroll-for-function scroll-up-command)
  (disable-smooth-scroll-for-function scroll-down-command)
  :ensure t)


(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :ensure t)


(use-package toml-mode
  :defer t
  :ensure t)


(use-package undo-tree
  :config (global-undo-tree-mode 1)
  :bind ("C-=" . undo-tree-redo)
  :diminish
  :ensure t)


(use-package which-key
  :config
  (which-key-mode)
  :diminish
  :ensure t)


(use-package whitespace
  :init
  (setq-default whitespace-style '(face trailing tab-mark))
  ;; Turn on whitespace-mode only in file buffers.
  :hook (find-file . whitespace-mode)
  :diminish)


(use-package whitespace-cleanup-mode
  :defer 5
  :config
  (global-whitespace-cleanup-mode 1)
  :diminish
  :ensure t)


(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode
  :ensure t)


(provide 'aethanyc-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-editor.el ends here
