;;;-------------------------------------------------------------------
;;; Packages

(defvar aethanyc-packages '(
                            ace-jump-mode
                            ack-and-a-half
                            auto-complete
                            buffer-move
                            ergoemacs-keybindings
                            framemove
                            helm
                            helm-projectile
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

;; Helm mode
(require 'helm-config)
(require 'helm-projectile)

;; Projectile Mode
(projectile-global-mode 1)
(setq projectile-enable-caching t)
(setq projectile-ignored-file-extensions
      (append projectile-ignored-file-extensions
              '("bin" "bmp" "cod" "d" "dll" "exe" "fml" "gif" "gir" "html" "idb"
                "ilk" "img" "jpg" "lib" "log" "map" "ncb" "obj" "pdb" "pl" "png"
                "pyc" "qbk" "rgn" "sln" "suo" "svg" "ttf" "txt" "user" "vcproj"
                "zip")))

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
(setq ac-use-quick-help nil)
(setq ac-modes (append ac-modes '(text-mode
                                  magit-log-edit-mode
                                  inferior-emacs-lisp-mode)))
(add-to-list 'ac-sources 'ac-source-semantic t)
