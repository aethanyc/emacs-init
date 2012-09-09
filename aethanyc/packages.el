;;;-------------------------------------------------------------------
;;; Packages

(defvar aethanyc-packages '(
                            ace-jump-mode
                            ack-and-a-half
                            ergoemacs-keybindings
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
