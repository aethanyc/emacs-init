;;;-------------------------------------------------------------------
;;; Packages

(defvar aethanyc-packages '(
                            ace-jump-mode
                            ack-and-a-half
                            auto-complete
                            buffer-move
                            ergoemacs-keybindings
                            framemove
                            highlight-symbol
                            ido-ubiquitous
                            magit
                            markdown-mode
                            maxframe
                            paredit
                            projectile
                            smex
                            smooth-scrolling
                            undo-tree
                            zenburn-theme
                            )
  "A list of packages to ensure are installed at launch.")


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package aethanyc-packages)
  (when (not (package-installed-p package))
    (package-install package)))

;;;-------------------------------------------------------------------
;;; Packages initialization

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

;; Smex: an M-x enhancement
(setq smex-save-file (concat aethanyc-save-file-directory "smex-items"))
(smex-initialize)

;; Load Ergoemacs functions
(load "functions")

;; Projectile Mode
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Highlight-symbol Mode
(require 'highlight-symbol)
(setq highlight-symbol-on-navigation-p t)
(setq highlight-symbol-idle-delay 30)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; Auto Complete Mode
(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file
      (concat aethanyc-save-file-directory "ac-comphist.dat"))
(setq ac-use-menu-map t)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-all-buffer))
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(global-auto-complete-mode 1)
