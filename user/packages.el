;;;-------------------------------------------------------------------
;;; Packages

(defvar user-packages '(
                        ace-jump-mode
                        ac-js2
                        ack-and-a-half
                        auto-complete
                        back-button
                        buffer-move
                        ergoemacs-mode
                        expand-region
                        framemove
                        helm
                        helm-projectile
                        highlight-symbol
                        ido-ubiquitous
                        js2-mode
                        lua-mode
                        magit
                        markdown-mode
                        maxframe
                        multiple-cursors
                        paredit
                        projectile
                        scala-mode
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

(dolist (package user-packages)
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
(defadvice ace-jump-mode (before ace-jump-mode-advice)
  (back-button-push-mark-local-and-global))
(ad-activate 'ace-jump-mode)

;; Undo Tree Mode
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Smex: an M-x enhancement
(setq smex-save-file (concat user-save-file-directory "smex-items"))
(smex-initialize)

;; Load Ergoemacs functions
(load "ergoemacs-functions")

;; Helm mode
(require 'helm-config)
(require 'helm-projectile)
(helm-mode 1)

;; Projectile Mode
(projectile-global-mode 1)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-cache-file
      (expand-file-name "projectile.cache" user-save-file-directory))
(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" user-save-file-directory))

;; Highlight-symbol Mode
(require 'highlight-symbol)
(setq highlight-symbol-on-navigation-p t)
(setq highlight-symbol-idle-delay 30)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; Auto Complete Mode
(require 'auto-complete-config)
(ac-config-default)
(setq ac-comphist-file
      (concat user-save-file-directory "ac-comphist.dat"))
(setq ac-use-menu-map t)
(setq ac-auto-show-menu t)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer))
(setq ac-modes (append ac-modes '(html-mode
                                  inferior-emacs-lisp-mode
                                  log-edit-mode
                                  magit-log-edit-mode
                                  markdown-mode
                                  nxml-mode
                                  org-mode
                                  text-mode)))

;; multiple-cursors
(setq mc/list-file (concat user-save-file-directory "mc-list"))

;; back-button
(setq pcache-directory user-save-file-directory)
(require 'back-button)
(setq back-button-mode-lighter nil)
(back-button-mode 1)
