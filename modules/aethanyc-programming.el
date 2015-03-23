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
(require 'diminish)
(require 'bind-key)

;;; Prog Mode

(use-package subword
  :defer t
  :init (add-hook 'prog-mode-hook #'subword-mode t)
  :diminish subword-mode)


(use-package elec-pair
  :defer t
  :init (add-hook 'prog-mode-hook #'electric-pair-mode))


(use-package nlinum
  :defer t
  :init (aethanyc-hook-into-modes #'nlinum-mode
          '(css-mode-hook prog-mode-hook))
  :ensure t)


(use-package which-func
  :defer t
  :init
  (setq mode-line-misc-info
        ;; Remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))

  (defun which-function-mode-setup ()
    (which-function-mode 1)

    ;; Show the current function name in the header line
    (setq header-line-format
          '((which-func-mode ("" which-func-format " ")))))

  (add-hook 'prog-mode-hook #'which-function-mode-setup))


(use-package company
  :defer 5
  :bind ("<C-tab>" . company-complete)
  :config
  (global-company-mode 1)
  (bind-key "C-n" #'company-select-next company-active-map)
  (bind-key "C-p" #'company-select-previous company-active-map)

  (use-package company-dabbrev-code
    :config (setq company-dabbrev-code-everywhere t))
  :diminish company-mode
  :ensure t)


;;; Lisp Mode

(use-package eldoc
  :defer t
  :init (aethanyc-hook-into-modes #'eldoc-mode
          '(lisp-mode-hook emacs-lisp-mode-hook ielm-mode-hook))
  :diminish eldoc-mode)


(use-package elisp-slime-nav
  :defer t
  :init
  (aethanyc-hook-into-modes #'elisp-slime-nav-mode
    '(emacs-lisp-mode-hook ielm-mode-hook))
  :config
  ;; Call (push-mark) to jump back later by (back-button-global-backward)
  (defadvice elisp-slime-nav-find-elisp-thing-at-point
      (before elisp-slime-nav-find-elisp-thing-at-point-advice activate)
    (push-mark))
  :diminish elisp-slime-nav-mode
  :ensure elisp-slime-nav)


(use-package lisp-mode
  :defer t
  :config
  (bind-key "C-c v" 'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-c v" 'eval-buffer lisp-interaction-mode-map))


;;; C/C++ Mode

;; To install global on Mac OS X
;; $ brew install global --with-exuberant-ctags
(use-package ggtags
  :if (executable-find "global")
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'ggtags-mode)
  :config
  (setq ggtags-global-ignore-case t)
  (setq ggtags-completing-read-function
        (lambda (&rest args)
          (apply #'ido-completing-read
                 (car args)
                 (all-completions "" ggtags-completion-table)
                 (cddr args))))

  (bind-key "C-g" #'ggtags-navigation-mode-abort ggtags-navigation-map)
  (bind-key "C-M-o" #'ggtags-navigation-visible-mode ggtags-navigation-map)
  (unbind-key "M-o" ggtags-navigation-map)

  ;; Call (push-mark) to jump back later by (back-button-global-backward)
  (defadvice ggtags-find-tag-dwim (before ggtags-find-tag-dwim-advice activate)
    (push-mark))
  :diminish ggtags-mode)


(use-package cc-mode
  :defer t
  :init
  (require 'cc-styles)
  (require 'cc-vars)
  (defun c-mode-common-setup ()
    (c-add-style "aethanyc"
                 '("stroustrup"
                   (c-basic-offset . 2)
                   (c-offsets-alist
                    (inline-open . 0)
                    (innamespace . 0))))
    (c-set-style "aethanyc")
    (setq company-backends '(company-dabbrev-code
                             company-capf
                             company-keywords
                             company-dabbrev)))
  (add-hook 'c-mode-common-hook #'c-mode-common-setup)
  :config
  (bind-key "C-c o" #'ff-find-other-file c-mode-base-map))


;;; LaTeX Mode

(use-package tex
  :defer t
  :config
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
    :init (setq LaTeX-indent-level 0))
  :ensure auctex)


;;; Python Mode

;; External python packages to install:
;; $ pip3 install ipython
;; $ pip3 install virtualenv
;; $ pip3 install flake8

(use-package python
  :defer t
  :config
  ;; Set preferred python interpreter
  (setq python-shell-interpreter
        (cond ((executable-find "ipython") "ipython")
              ((executable-find "python3") "python3")
              (t "python"))))


(use-package company-jedi
  :defer t
  :init
  (setq jedi:use-shortcuts t)
  (defun aethanyc-company-jedi-setup ()
    (jedi:setup)
    (setq company-backends '(company-jedi
                             company-dabbrev-code
                             company-keywords
                             company-dabbrev)))
  (aethanyc-hook-into-modes #'aethanyc-company-jedi-setup
    '(python-mode-hook inferior-python-mode-hook))
  :ensure t)


;;; Web development
(use-package css-mode
  :config
  (progn
    (setq css-indent-offset 2)))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (progn
    (setq-default js2-basic-offset 2))
  :ensure js2-mode)

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (progn
    (setq web-mode-script-padding 0
          web-mode-style-padding 0
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-sql-indent-offset 2)
    (use-package browse-url
      :init
      (progn
        (bind-key "C-c C-v" #'browse-url-of-buffer web-mode-map))))
  :ensure web-mode)


;;; Rust Mode
(use-package rust-mode
  :ensure rust-mode)


;;; Other

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook #'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
