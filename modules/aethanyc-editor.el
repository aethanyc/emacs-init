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

;; Make auto-save files visible.
(setq auto-save-list-file-prefix (concat aethanyc-savefiles-dir "auto-save-"))

;; Auto save buffer for every 5 seconds.
(setq auto-save-timeout 5)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)

;; Remove the all the version control backends to increase file open speed.
;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
(setq vc-handled-backends nil)

;; Let `fill-paragraph' leaves just one space at the end of the sentence.
(setq-default sentence-end-double-space nil)

;; Do not display message in scratch buffer.
(setq initial-scratch-message nil)

;; Remove trailing whitespaces before saving a buffer.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Built-in packages

;; Save the last place of the cursor.  This should be loaded as early
;; as possible.  I don't know why it fails to load save-place-file if
;; it is loaded too late.
(use-package saveplace
  :init
  (progn
    (setq save-place-file (expand-file-name "places" aethanyc-savefiles-dir))
    (setq-default save-place t)))


;; For key C-x C-j: `dired-jump'
(use-package dired-x)


(use-package ispell
  :init
  (progn
    ;; Check available dictionaries: hunspell -D
    ;; Name hunspell English dictionary files as default.aff and default.dic
    (setq ispell-program-name
          (or (executable-find "hunspell")
              (executable-find "ispell")))

    (when ispell-program-name
      (use-package flyspell
        :init
        (progn
          (add-hook 'prog-mode-hook 'flyspell-prog-mode)
          (add-hook 'text-mode-hook 'flyspell-mode)))

      ;; Fix slow cursor movement on windows.
      (when (eq system-type 'windows-nt)
        (use-package flyspell-lazy
          :init
          (progn
            (add-hook 'prog-mode-hook 'flyspell-lazy-mode)
            (add-hook 'text-mode-hook 'flyspell-lazy-mode))
          :ensure flyspell-lazy)))))


(use-package org
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'visual-line-mode))
  :config
  (progn
    (setq org-completion-use-ido t
          org-ctrl-k-protect-subtree t
          org-special-ctrl-k t
          org-src-fontify-natively t
          org-startup-indented nil
          org-special-ctrl-a/e t
          org-M-RET-may-split-line '((default))
          org-export-copy-to-kill-ring nil
          org-export-with-sub-superscripts (quote {}))

    ;; Highlight the source code in html exported.
    (use-package htmlize
      :ensure htmlize)

    ;; Grab link in various app on Mac OS.
    (when (eq system-type 'darwin)
      (use-package org-mac-link
        :disabled t
        :init (progn (bind-key "C-c C-g" 'org-mac-grab-link org-mode-map))
        :ensure org-mac-link))

    ;; Export org-mode content to reveal.js
    (use-package ox-reveal
      :init
      (progn
        (defun org-reveal-save-then-export ()
          "Save buffer and then export to html."
          (interactive)
          (save-buffer)
          (org-reveal-export-to-html))

        (bind-key "<f5>" 'org-reveal-save-then-export org-mode-map))
      :ensure ox-reveal)))


;; Keep a list of recently opened files
(use-package recentf
  :init
  (progn
    (setq recentf-save-file (expand-file-name "recentf" aethanyc-savefiles-dir)
          recentf-max-menu-items 20
          recentf-max-saved-items 500)
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
  :bind (("M-m" . ace-jump-mode)
         ("M-M" . ace-jump-char-mode))
  :config (ace-jump-mode-enable-mark-sync)
  :ensure ace-jump-mode)


(use-package ack-and-a-half
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))
  :ensure ack-and-a-half)


(use-package ag
  :ensure ag)


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
    (ac-flyspell-workaround)
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
                             latex-mode
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


(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings))
  :ensure browse-kill-ring)


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

  :bind (("<f9>" . desktop-revert)
         ("<M-f9>" . aethanyc-desktop-save)))


(use-package eshell
  :init (setq eshell-directory-name
              (expand-file-name "eshell/" aethanyc-savefiles-dir))
  :bind ("C-x m" . eshell))


(use-package evil
  :defer t
  :ensure evil)


(use-package expand-region
  :bind ("M-8" . er/expand-region)
  :ensure expand-region)


(use-package flycheck
  :init
  (progn
    ;; C-c ! ? to describe the syntax checker.
    (setq-default flycheck-clang-include-path '(".")
                  flycheck-emacs-lisp-initialize-packages t
                  flycheck-emacs-lisp-load-path load-path
                  flycheck-flake8-maximum-line-length 85)

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

    ;; It is easier to switch buffer on single key.
    (bind-key "<f2>" 'ido-switch-buffer)
    (bind-key "<f2>" 'ido-switch-buffer-other-window ctl-x-4-map)
    (bind-key "<f2>" 'ido-switch-buffer-other-frame ctl-x-5-map)

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
      :ensure flx-ido)

    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1)
      :ensure ido-vertical-mode)))


(use-package imenu-anywhere
  :bind ("C-c i" . imenu-anywhere)
  :ensure imenu-anywhere)


(use-package keyfreq
  :init
  (progn
    (setq keyfreq-file (expand-file-name "keyfreq" aethanyc-savefiles-dir)
          keyfreq-file-lock (expand-file-name "keyfreq.lock" aethanyc-savefiles-dir))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))
  :ensure keyfreq)


(use-package magit
  :bind ("<f4>" . magit-status)
  :config
  (progn
    ;; Diminish magit auto revert mode lighter.
    (setq magit-auto-revert-mode-lighter "")

    ;; Do not allow magit overrides the window management keys.
    ;; Bind those show level keys to C-x 1, C-x 2, etc.
    (dolist (i '(1 2 3 4))
      (let ((key-unbind (format "M-%d" i))
            (key-bind (format "C-x %d" i))
            (func-bind (intern (format "magit-show-level-%d-all" i))))
        (unbind-key key-unbind magit-mode-map)
        (bind-key key-bind func-bind magit-mode-map))))
  :ensure magit)


;; Install git-wip to ~/bin
;; https://raw.github.com/bartman/git-wip/master/git-wip
;; $ git config --global --add magit.extension wip-save
(when (executable-find "git-wip")
  (use-package magit-wip
    :init
    (progn
      (global-magit-wip-save-mode 1))))


(use-package mark-tools
  :ensure mark-tools)


(use-package markdown-mode
  :defer t
  :ensure markdown-mode)


(use-package multiple-cursors
  :init (setq mc/list-file (expand-file-name "mc-lists.el" aethanyc-savefiles-dir))
  :bind (("M-9" . mc/mark-next-like-this)
         ("C-c M-9" . mc/mark-all-like-this))
  :ensure multiple-cursors)


(use-package paredit
  :init
  (progn
    (aethanyc-hook-into-modes 'paredit-mode
      '(lisp-mode-hook emacs-lisp-mode-hook)))
  :config
  (progn
    (bind-key "M-B" 'paredit-backward paredit-mode-map)
    (bind-key "M-F" 'paredit-forward paredit-mode-map)
    (bind-key "RET" 'paredit-newline paredit-mode-map)
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
    ;; The known project list is loaded when projectile is required.
    ;; It should be reloaded since the file path is changed.
    (projectile-load-known-projects)
    (projectile-global-mode 1))
  :diminish ""
  :ensure projectile)


(use-package server
  :init
  (progn
    (setq server-auth-dir aethanyc-savefiles-dir)
    (unless (server-running-p)
      (server-start))))


(use-package smartparens
  :init
  (progn
    ;; Configuration from smartparens-config.el
    (--each sp--html-modes
      (eval-after-load (symbol-name it) '(require 'smartparens-html)))
    (eval-after-load "lua-mode" '(require 'smartparens-lua))

    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always)
    (sp-use-paredit-bindings)

    ;; Do not turn on smartparens-mode in lisp family mode.
    (setq sp-ignore-modes-list
          (append sp-ignore-modes-list
                  '(lisp-mode lisp-interaction-mode emacs-lisp-mode
                              inferior-emacs-lisp-mode)))

    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1))
  :ensure smartparens)


(use-package smex
  :init (setq smex-save-file (concat aethanyc-savefiles-dir "smex-items"))
  :bind (("<menu>" . smex)
         ("<apps>" . smex) ; the key with a menu icon
         ("<f12>" . smex)
         ("M-<menu>" . smex-major-mode-commands)
         ("M-<apps>" . smex-major-mode-commands)
         ("<M-f12>" . smex-major-mode-commands))
  :config (global-set-key [remap execute-extended-command] 'smex)
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
      '(c-mode-common-hook python-mode-hook LaTeX-mode-hook)))
  :ensure yasnippet)


(provide 'aethanyc-editor)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-editor.el ends here
