;;;-------------------------------------------------------------------
;;; Lisp Mode

(defun my-lisp-hook ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(defun my-lisp-edit-keybindings ()
  (local-set-key (kbd "C-c v") 'eval-buffer)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(defun my-lisp-edit-hook ()
  (my-lisp-hook)
  (my-lisp-edit-keybindings))

(add-hook 'lisp-mode-hook 'my-lisp-edit-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-edit-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-edit-hook)
(add-hook 'ielm-mode-hook 'my-lisp-hook)

;;;-------------------------------------------------------------------
;;; Semantic mode
;; http://www.emacswiki.org/emacs/CEDET_Quickstart
(require 'semantic)
(require 'semantic/analyze/refs)
(require 'semantic/mru-bookmark)
(require 'pulse)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-mru-bookmark-mode))

(setq pulse-flag 'never)
(setq semanticdb-default-save-directory
      (concat aethanyc-save-file-directory "semanticdb"))

(defun my-semantic-hook ()
  (local-set-key (kbd "M-g") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c t") 'semantic-analyze-proto-impl-toggle))

(add-hook 'semantic-init-hook 'my-semantic-hook)

;;;-------------------------------------------------------------------
;;; C/C++ Mode

(defun my-c-mode-common-hook ()
  (semantic-mode 1)
  (electric-pair-mode 1)
  (subword-mode 1)f
  ;; (c-set-style "stroustrup")
  ;; C-c C-o: Current indentation level
  ;; (c-set-offset 'cpp-macro 0)
  ;; (c-set-offset 'topmost-intro-cont 4)
  ;; (c-set-offset 'arglist-intro 0)
  ;; (c-set-offset 'arglist-close 0)
  ;; (c-set-offset 'member-init-intro 0)
  ;; (c-set-offset 'inline-open 0)
  (local-set-key (kbd "RET") 'c-context-line-break))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defconst my-c++-style
  '("stroustrup"
    (c-offsets-alist
     (topmost-intro-cont . +)
     (arglist-intro . 0)
     (arglist-close . 0)
     (member-init-intro . 0)
     (cpp-macro . 0))))

(defun my-c++-mode-hook ()
  (c-add-style "my-c++-style" my-c++-style t))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;;-------------------------------------------------------------------
;;; Text Mode

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;-------------------------------------------------------------------
;;; Ibuffer Mode

(defun my-ibuffer-mode-hook ()
  (local-set-key (kbd "C-x C-f") 'ibuffer-ido-find-file))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

;;;-------------------------------------------------------------------
;;; Scala Mode

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;;;-------------------------------------------------------------------
;;; Other

;; Clean up white spaces before saving a buffer.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'byte-compile-current-buffer)
