;;; aethanyc-programming.el --- Settings for programming

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file hosts settings for various programming modes.

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'aethanyc-core)

;;; Prog Mode

(use-package subword
  :init (add-hook 'prog-mode-hook 'subword-mode)
  :diminish "")


;;; Lisp Mode

(use-package eldoc
  :init (aethanyc-hook-into-modes 'turn-on-eldoc-mode
          '(lisp-mode-hook emacs-lisp-mode-hook ielm-mode-hook))
  :diminish "")


(use-package elisp-slime-nav
  :init
  (progn
    (aethanyc-hook-into-modes 'elisp-slime-nav-mode
      '(emacs-lisp-mode-hook ielm-mode-hook)))
  :diminish ""
  :ensure elisp-slime-nav)


(use-package paren
  :init (aethanyc-hook-into-modes 'show-paren-mode
          '(lisp-mode-hook emacs-lisp-mode-hook ielm-mode-hook)))


(use-package lisp-mode
  :config
  (progn
    (bind-key "C-c v" 'eval-buffer emacs-lisp-mode-map)
    (bind-key "C-c v" 'eval-buffer lisp-interaction-mode-map)))


;;; C/C++ Mode

(when (executable-find "clang")
  (use-package auto-complete-clang
    :init
    (progn
      (defun ac-clang-setup ()
        (add-to-list 'ac-sources 'ac-source-clang))
      (add-hook 'c-mode-common-hook 'ac-clang-setup t))
    :ensure auto-complete-clang))


(when (executable-find "global")
  (use-package ggtags
    :init
    (progn
      (add-hook 'c-mode-common-hook 'ggtags-mode))
    :config
    (progn
      ;; Refine keys in `ggtags-mode-map'
      (bind-key "M-," 'pop-tag-mark ggtags-mode-map)
      (bind-key "M-*" 'ggtags-find-tag-resume ggtags-mode-map)

      ;; Refine keys in `ggtags-navigation-mode-map'
      (bind-key "M-," 'previous-error ggtags-navigation-mode-map)
      (bind-key "M-." 'next-error ggtags-navigation-mode-map)
      (bind-key "C-g" 'ggtags-navigation-mode-abort ggtags-navigation-mode-map)
      (unbind-key "M-o" ggtags-navigation-mode-map))
    :ensure ggtags))


(use-package cc-mode
  :init
  (progn
    (defun c-mode-common-setup ()
      (c-set-style "stroustrup"))
    (add-hook 'c-mode-common-hook 'c-mode-common-setup))
  :config
  (progn
    (bind-key "RET" 'c-context-line-break c-mode-base-map)))


;;; Python Mode

;; External python packages to install:
;; $ sudo pip3 ipython
;; $ sudo pip3 install flake8

(use-package python
  :init
  (progn
    ;; Set preferred python interpreter
    (setq python-shell-interpreter
          (cond ((executable-find "ipython3") "ipython3")
                ((executable-find "python3") "python3")
                (t "python")))

    ;; Ipython settings was copied from the document of python-mode.
    (when (executable-find "ipython3")
      (setq python-shell-interpreter-args ""
            python-shell-prompt-regexp "In \\[[0-9]+\\]: "
            python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
            python-shell-completion-setup-code
            "from IPython.core.completerlib import module_completion"
            python-shell-completion-module-string-code
            "';'.join(module_completion('''%s'''))\n"
            python-shell-completion-string-code
            "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))))


(use-package jedi
  :init
  (progn
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t)

    (aethanyc-hook-into-modes 'jedi:setup
      '(python-mode-hook inferior-python-mode-hook)))
  :ensure jedi)


;;; Lua Mode

(use-package lua-mode
  :init
  (progn
    (setq lua-indent-level 4))
  :config
  (progn
    (bind-key "}" nil lua-mode-map)
    (bind-key "]" nil lua-mode-map)
    (bind-key ")" nil lua-mode-map))
  :ensure lua-mode)


;;; Other

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
