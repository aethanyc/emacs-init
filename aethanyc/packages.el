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
