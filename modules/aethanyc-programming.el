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
  :defer t
  :init
  (progn
    (aethanyc-hook-into-modes 'elisp-slime-nav-mode
      '(emacs-lisp-mode-hook ielm-mode-hook)))
  :config
  (progn
    ;; Call (push-mark) to jump back later by (back-button-global-backward)
    (defadvice elisp-slime-nav-find-elisp-thing-at-point
        (before elisp-slime-nav-find-elisp-thing-at-point-advice activate)
      (push-mark)))
  :diminish ""
  :ensure elisp-slime-nav)


(use-package lisp-mode
  :config
  (progn
    (bind-key "C-c v" 'eval-buffer emacs-lisp-mode-map)
    (bind-key "C-c v" 'eval-buffer lisp-interaction-mode-map)))


;;; C/C++ Mode

(use-package auto-complete-c-headers
  :config
  (progn
    (when (eq system-type 'darwin)
      (add-to-list 'achead:include-directories "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/c++/v1"))

    (defun ac-c-headers-setup ()
      (add-to-list 'ac-sources 'ac-source-c-headers))

    (aethanyc-hook-into-modes 'ac-c-headers-setup
      '(c-mode-hook c++-mode-hook)))
  :ensure auto-complete-c-headers)


(when (executable-find "clang")
  (use-package auto-complete-clang
    :init
    (progn
      (defun ac-clang-setup ()
        (add-to-list 'ac-sources 'ac-source-clang))
      (add-hook 'c-mode-common-hook 'ac-clang-setup t))
    :ensure auto-complete-clang))


;; To install global on Mac OS X
;; $ brew install global --with-exuberant-ctags
(when (executable-find "global")
  (use-package ggtags
    :defer t
    :init
    (progn
      (add-hook 'c-mode-common-hook 'ggtags-mode))
    :config
    (progn
      (setq ggtags-global-ignore-case t)
      (setq ggtags-completing-read-function
            (lambda (&rest args)
              (apply #'ido-completing-read
                     (car args)
                     (all-completions "" ggtags-completion-table)
                     (cddr args))))

      (bind-key "C-g" 'ggtags-navigation-mode-abort ggtags-navigation-map)
      (bind-key "C-M-o" 'ggtags-navigation-visible-mode ggtags-navigation-map)
      (unbind-key "M-o" ggtags-navigation-map)

      ;; Call (push-mark) to jump back later by (back-button-global-backward)
      (defadvice ggtags-find-tag-dwim (before ggtags-find-tag-dwim-advice activate)
        (push-mark)))
    :diminish ""
    :ensure ggtags))


(use-package cc-mode
  :defer t
  :init
  (progn
    (defun c-mode-common-setup ()
      (c-add-style "aethanyc"
                   '("stroustrup"
                     (c-offsets-alist
                      (inline-open . 0))))
      (c-set-style "aethanyc"))
    (add-hook 'c-mode-common-hook 'c-mode-common-setup))
  :config
  (progn
    (bind-key "RET" 'c-context-line-break c-mode-base-map)))


;;; LaTeX Mode

(use-package tex
  :defer t
  :config
  (progn
    (setq-default TeX-engine 'xetex
                  TeX-PDF-mode t)

    (add-to-list 'TeX-command-list
                 '("Latexmk" "latexmk -pvc -xelatex %(mode) %t"
                   TeX-run-TeX nil (plain-tex-mode latex-mode doctex-mode)
                   :help "Run Latexmk"))

    (when (eq system-type 'darwin)
      (add-to-list 'TeX-view-program-list
                   '("Open" "open %o"))
      (add-to-list 'TeX-view-program-selection
                   '(output-pdf "Open")))

    (use-package tex-buf
      :init (setq TeX-save-query nil))

    (use-package latex
      :init (setq LaTeX-indent-level 0)))
  :ensure auctex)


;;; Python Mode

;; External python packages to install:
;; $ pip3 install ipython
;; $ pip3 install virtualenv
;; $ pip3 install flake8

(use-package python
  :defer t
  :config
  (progn
    ;; Set preferred python interpreter
    (setq python-shell-interpreter
          (cond ((executable-find "ipython") "ipython")
                ((executable-find "python3") "python3")
                (t "python")))

    ;; Ipython settings was copied from the document of python-mode.
    (when (executable-find "ipython")
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
  :defer t
  :init
  (progn
    (aethanyc-hook-into-modes 'jedi:setup
      '(python-mode-hook inferior-python-mode-hook)))
  :config
  (progn
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t))
  :ensure jedi)


;;; Other

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook 'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
