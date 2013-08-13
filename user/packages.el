;; -*- coding: utf-8-unix; -*-

;;;-------------------------------------------------------------------
;;; Packages

(defvar user-packages '(
                        ac-js2
                        ace-jump-mode
                        ack-and-a-half
                        auto-complete
                        back-button
                        buffer-move
                        ergoemacs-mode
                        exec-path-from-shell
                        expand-region
                        flymake-cursor
                        flymake-python-pyflakes
                        framemove
                        fuzzy
                        git-commit-mode
                        helm
                        helm-projectile
                        highlight-symbol
                        ido-ubiquitous
                        jedi
                        js2-mode
                        lua-mode
                        magit
                        markdown-mode
                        maxframe
                        multiple-cursors
                        paredit
                        projectile
                        smex
                        smooth-scrolling
                        undo-tree
                        yasnippet
                        zenburn-theme
                        )
  "A list of packages to ensure are installed at launch.")

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package user-packages)
  (when (not (package-installed-p package))
    (package-install package)))

;;;-------------------------------------------------------------------
;;; Packages initialization

;; Copy the value of paths from shell
(when (is-mac-p)
  (exec-path-from-shell-initialize))

;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode) t)

;; Ace Jump Mode
(eval-after-load 'ace-jump-mode
  '(progn
     (setq ace-jump-mode-gray-background nil
           ace-jump-mode-case-fold nil)))

;; Undo Tree Mode
(global-undo-tree-mode 1)

;; Smex: an M-x enhancement
(eval-after-load 'smex
  '(progn
     (setq smex-save-file (concat user-save-file-directory "smex-items"))))

;; Load Ergoemacs functions
(require 'ergoemacs-functions)

;; Helm mode
(require 'helm-config)
(require 'helm-projectile)
(setq helm-completing-read-handlers-alist
      '((describe-function . ido-completing-read)
        (describe-variable . ido-completing-read)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file . nil)
        (tmm-menubar . nil)))
(helm-mode 1)

;; Projectile Mode
(eval-after-load 'projectile
  '(progn
     (setq projectile-cache-file
           (concat user-save-file-directory "projectile.cache"))
     (setq projectile-known-projects-file
           (concat user-save-file-directory "projectile-bookmarks.eld"))))
(projectile-global-mode 1)

;; Highlight-symbol Mode
(eval-after-load 'highlight-symbol
  '(progn
     (setq highlight-symbol-on-navigation-p t)
     (set-face-attribute 'highlight-symbol-face nil :background "gray35")))

;; Auto Complete Mode
(require 'auto-complete-config)
(ac-config-default)
(ac-linum-workaround)
(setq ac-comphist-file
      (concat user-save-file-directory "ac-comphist.dat"))
(setq ac-use-menu-map t)
(setq ac-auto-show-menu t)
;; Override ac-sources defined in (ac-config-default)
(setq-default ac-sources '(ac-source-abbrev
                           ac-source-yasnippet
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer))
(setq ac-modes (append ac-modes '(eshell-mode
                                  git-commit-mode
                                  html-mode
                                  inferior-emacs-lisp-mode
                                  inferior-python-mode
                                  log-edit-mode
                                  magit-log-edit-mode
                                  markdown-mode
                                  nxml-mode
                                  org-mode
                                  text-mode)))

;; multiple-cursors
(eval-after-load 'multiple-cursors
  '(progn
     (setq mc/list-file (concat user-save-file-directory "mc-list"))))

;; back-button
(setq pcache-directory user-save-file-directory)
(back-button-mode 1)
(setq back-button-mode-lighter nil)
;; Replace push-mark so that the current position is always preserved
;; before jumping around.
(fset 'push-mark 'back-button-push-mark-local-and-global)

;; YASnippet
(yas-global-mode 1)

;; magit
(eval-after-load 'magit
  '(progn
     (defun magit-visit-item-other-window ()
       (interactive)
       (let ((current-prefix-arg '(4)))
         (call-interactively 'magit-visit-item)))
     (define-key magit-mode-map (kbd "RET") 'magit-visit-item-other-window)))

;; paredit
(eval-after-load 'paredit
  '(progn
     ;; Expose navigation forward and backward keybindings
     (define-key paredit-mode-map (kbd "C-M-b") nil)
     (define-key paredit-mode-map (kbd "C-M-f") nil)
     (define-key paredit-mode-map (kbd "M-B") 'paredit-backward)
     (define-key paredit-mode-map (kbd "M-F") 'paredit-forward)))
