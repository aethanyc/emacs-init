;;;-------------------------------------------------------------------
;;; Prog Mode

(defun my-prog-mode-hook ()
  (electric-pair-mode 1)
  (highlight-symbol-mode 1)
  (linum-mode 1)
  (subword-mode 1))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;;-------------------------------------------------------------------
;;; Lisp Mode

(defun my-lisp-mode-hook ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(defun set-my-lisp-keybindings ()
  (local-set-key (kbd "C-c v") 'eval-buffer)
  (local-set-key (kbd "RET") 'paredit-newline))

(defun my-lisp-mode-keybindings-hook ()
  (my-lisp-mode-hook)
  (set-my-lisp-keybindings))

(add-hook 'lisp-mode-hook 'my-lisp-mode-keybindings-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-keybindings-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-keybindings-hook)
(add-hook 'ielm-mode-hook 'my-lisp-mode-hook)

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
      (concat user-save-file-directory "semanticdb"))

(defun my-semantic-hook ()
  (local-set-key (kbd "M-g") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c t") 'semantic-analyze-proto-impl-toggle))

(add-hook 'semantic-init-hook 'my-semantic-hook)

;;;-------------------------------------------------------------------
;;; C/C++ Mode

(defconst my-c/c++-style
  '("stroustrup"
    (c-offsets-alist
     (topmost-intro-cont . +)
     (arglist-intro . 0)
     (arglist-close . 0)
     (member-init-intro . 0)
     (cpp-macro . 0))))

(defun my-c-mode-common-hook ()
  (semantic-mode 1)
  (add-to-list 'ac-sources 'ac-source-semantic t)
  (c-add-style "my-c/c++-style" my-c/c++-style)
  (c-set-style "my-c/c++-style")
  (local-set-key (kbd "RET") 'c-context-line-break))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;-------------------------------------------------------------------
;;; Python Mode

(defun my-python-mode-hook ()
  ;; Ipython settings was copied from the document of python-mode.
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args ""
  ;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  ;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  ;;       python-shell-completion-setup-code
  ;;       "from IPython.core.completerlib import module_completion"
  ;;       python-shell-completion-module-string-code
  ;;       "';'.join(module_completion('''%s'''))\n"
  ;;       python-shell-completion-string-code
  ;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (setq python-shell-interpreter "python3"))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;;-------------------------------------------------------------------
;;; Ibuffer Mode

(defun my-ibuffer-mode-hook ()
  (local-set-key (kbd "C-x C-f") 'ibuffer-ido-find-file))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

;;;-------------------------------------------------------------------
;;; Javascript Mode (js-mode)

(defun my-js-mode-hook ()
  (js2-minor-mode 1)
  (skewer-mode 1)
  (ac-js2-mode 1))

(add-hook 'js-mode-hook 'my-js-mode-hook)

;;;-------------------------------------------------------------------
;;; Lua Mode

(defun my-lua-mode-hook ()
  (setq lua-indent-level 4)
  (define-key lua-mode-map "}" nil)
  (define-key lua-mode-map "]" nil)
  (define-key lua-mode-map ")" nil))

(add-hook 'lua-mode-hook 'my-lua-mode-hook)

;;;-------------------------------------------------------------------
;;; Other

;; Clean up white spaces before saving a buffer.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'byte-compile-current-buffer)
