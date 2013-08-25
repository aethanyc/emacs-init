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
                        ; ergoemacs-mode
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

;;;-------------------------------------------------------------------
;;; Packages initialization

;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode) t)

;; Load Ergoemacs functions
; (require 'ergoemacs-functions)

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
           (concat aethanyc-savefiles-dir "projectile.cache"))
     (setq projectile-known-projects-file
           (concat aethanyc-savefiles-dir "projectile-bookmarks.eld"))))
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
      (concat aethanyc-savefiles-dir "ac-comphist.dat"))
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
     (setq mc/list-file (concat aethanyc-savefiles-dir "mc-list"))))

;; back-button
(setq pcache-directory aethanyc-savefiles-dir)
(back-button-mode 1)
(setq back-button-mode-lighter nil)
;; Replace push-mark so that the current position is always preserved
;; before jumping around.
(fset 'push-mark 'back-button-push-mark-local-and-global)

;; YASnippet
(yas-global-mode 1)

;; paredit
(eval-after-load 'paredit
  '(progn
     ;; Expose navigation forward and backward keybindings
     (define-key paredit-mode-map (kbd "C-M-b") nil)
     (define-key paredit-mode-map (kbd "C-M-f") nil)
     (define-key paredit-mode-map (kbd "M-B") 'paredit-backward)
     (define-key paredit-mode-map (kbd "M-F") 'paredit-forward)))
