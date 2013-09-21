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
                              '(emacs-lisp-mode-hook ielm-mode-hook))
    (bind-key "M-g" 'elisp-slime-nav-find-elisp-thing-at-point
              elisp-slime-nav-mode-map))
  :diminish ""
  :ensure elisp-slime-nav)


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
    :init (add-hook 'c-mode-common-hook 'ggtags-mode)
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
;; $ sudo pip3 install -r requirements.txt # Under jedi's install path
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
    (setq jedi:complete-on-dot t)
    (setq jedi:server-command
          (list python-shell-interpreter jedi:server-script))

    (defun aethanyc-jedi:goto-definition ()
      "Push mark before goto definition."
      (interactive)
      (push-mark)
      (call-interactively 'jedi:goto-definition))

    (aethanyc-hook-into-modes 'jedi:setup
                              '(python-mode-hook inferior-python-mode-hook)))
  :config (bind-key "M-g" 'aethanyc-jedi:goto-definition python-mode-map)
  :ensure jedi)


;;;-------------------------------------------------------------------
;;; Javascript Mode (js-mode)

(defun my-js-mode-hook ()
  (js2-minor-mode 1)
  (skewer-mode 1)
  (ac-js2-mode 1))

(add-hook 'js-mode-hook 'my-js-mode-hook)


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

;; Set whitespace mode styles
(setq-default whitespace-style '(face trailing lines-tail tabs empty indentation))

;; Clean up white spaces before saving a buffer.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
