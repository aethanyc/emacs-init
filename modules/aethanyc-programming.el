;;; aethanyc-programming.el --- Settings for programming

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file hosts settings for various programming modes.

;;; Code:


;;; Prog Mode

(defun aethanyc-prog-mode-hook ()
  "Enable utilities which are useful for all programming modes."
  (electric-pair-mode 1)
  (linum-mode 1)
  (subword-mode 1))

(add-hook 'prog-mode-hook 'aethanyc-prog-mode-hook)


;;; Lisp Mode

(defun my-lisp-mode-common-hook ()
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(defun my-emacs-lisp-mode-hook ()
  (my-lisp-mode-common-hook)
  (my-lisp-mode-keybindings-hook)
  (local-set-key (kbd "M-g") 'aethanyc-find-function-or-variable-at-point))

(defun my-lisp-keybindings ()
  (local-set-key (kbd "C-c v") 'eval-buffer)
  (local-set-key (kbd "RET") 'paredit-newline))

(defun my-lisp-mode-keybindings-hook ()
  (setq inferior-lisp-program "sbcl")
  (my-lisp-mode-common-hook)
  (my-lisp-keybindings))

(add-hook 'lisp-mode-hook 'my-lisp-mode-keybindings-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-keybindings-hook)
(add-hook 'ielm-mode-hook 'my-lisp-mode-common-hook)

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
      (concat aethanyc-savefiles-dir "semanticdb"))

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

;; External python packages to install:
;; $ sudo easy_install3 ipython
;; $ sudo pip3 install -r requirements.txt # Under jedi's install path
;; $ sudo pip3 install flake8

(setq python-shell-interpreter
      (cond ((executable-find "ipython3") "ipython3")
            ((executable-find "python3") "python3")
            (t "python")))

;; Jedi - Python auto-completion for Emacs
;; jedi:setup-keys and jedi:complete-on-dot must be set *before* jedi is loaded
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(eval-after-load 'jedi
  '(progn
     (setq jedi:server-command
           (list python-shell-interpreter jedi:server-script))
     ;; Push mark before goto definition
     (defadvice jedi:goto-definition (before jedi:goto-definition-advice)
       (push-mark))
     (ad-activate 'jedi:goto-definition)))

(defun my-python-mode-hook ()
  (cond ((executable-find "ipython3")
         ;; Ipython settings was copied from the document of python-mode.
         (setq python-shell-interpreter-args ""
               python-shell-prompt-regexp "In \\[[0-9]+\\]: "
               python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
               python-shell-completion-setup-code
               "from IPython.core.completerlib import module_completion"
               python-shell-completion-module-string-code
               "';'.join(module_completion('''%s'''))\n"
               python-shell-completion-string-code
               "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
  ;; flymake-python-pyflakes settings
  (when (executable-find "flake8")
    (setq flymake-python-pyflakes-executable "flake8")
    (flymake-python-pyflakes-load)
    (local-set-key (kbd "C-c C-n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c C-p") 'flymake-goto-prev-error))
  ;; Jedi settings
  (jedi:setup)
  (local-set-key (kbd "M-g") 'jedi:goto-definition))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'jedi:setup)

;;;-------------------------------------------------------------------
;;; Ibuffer Mode

(defun my-ibuffer-mode-hook ()
  (local-set-key (kbd "C-x C-f") 'aethanyc-ibuffer-ido-find-file))

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
(add-hook 'after-save-hook 'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
