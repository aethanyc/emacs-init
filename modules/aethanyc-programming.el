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
  :hook (prog-mode . subword-mode)
  :diminish)


(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))


(use-package display-line-numbers
  :hook ((css-mode prog-mode) . display-line-numbers-mode))


(use-package which-func
  :config
  (setq mode-line-misc-info
        ;; Remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-function-mode mode-line-misc-info))

  (defun which-function-mode-setup ()
    (which-function-mode 1)

    ;; Show the current function name in the header line
    (setq header-line-format
          '((which-func-mode ("" which-func-format " ")))))
  :hook (prog-mode . which-function-mode-setup))


(use-package company
  :defer 5
  :bind (("<C-tab>" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.2)
  (global-company-mode 1)

  (use-package company-dabbrev-code
    :config (setq company-dabbrev-code-everywhere t
                  company-dabbrev-downcase nil
                  company-dabbrev-ignore-case nil))
  :diminish
  :ensure t)


;;; Lisp Mode

(use-package eldoc
  :hook ((lisp-mode emacs-lisp-mode ielm-mode) . eldoc-mode)
  :diminish)


(use-package elisp-slime-nav
  :config
  ;; Call (push-mark) to jump back later by (back-button-global-backward)
  (defadvice elisp-slime-nav-find-elisp-thing-at-point
      (before elisp-slime-nav-find-elisp-thing-at-point-advice activate)
    (push-mark))
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :diminish
  :ensure t)


(use-package lisp-mode
  :bind (:map emacs-lisp-mode-map
         ("C-c v" . eval-buffer)
         :map lisp-interaction-mode-map
         ("C-c v" . eval-buffer)))


;;; C/C++ Mode

(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent))
  :ensure t)


(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c o" . ff-find-other-file)))


(use-package ggtags
  :bind (:map ggtags-navigation-map
              ("C-g" . ggtags-navigation-mode-abort)
              ("M-o" . nil))
  :hook (c-mode-common . ggtags-mode)
  :ensure-system-package global
  :diminish
  :ensure t)


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
  :config
  (setq jedi:use-shortcuts t)
  (defun company-jedi-setup ()
    (add-to-list 'company-backends 'company-jedi)
    (jedi:setup))
  :hook ((python-mode inferior-python-mode) . company-jedi-setup)
  :ensure t)


;;; Web development
(use-package css-mode
  :defer t
  :config
  (progn
    (setq css-indent-offset 2)))

(use-package js2-mode
  :mode "\\.jsm?\\'"
  :config
  (setq-default js2-basic-offset 2)
  :ensure t)

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-script-padding 0
        web-mode-style-padding 0
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-sql-indent-offset 2)
  :ensure t)


;;; Rust Mode
(use-package rust-mode
  :config
  (use-package flycheck-rust
    :hook (flycheck-mode . flycheck-rust-setup)
    :requires flycheck
    :ensure t)

  ;; cargo install racer
  (use-package racer
    :config
    ;; Install rust source code: rustup component add rust-src
    (setq racer-rust-src-path
          (if (eq system-type 'darwin)
              "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
            "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
    :diminish
    :ensure t)

  (use-package cargo
    :diminish cargo-minor-mode
    :ensure t)

  :hook ((rust-mode . cargo-minor-mode)
         (rust-mode . eldoc-mode)
         (rust-mode . racer-mode))

  :ensure t)


;;; Other
(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-auto-indent-on-semi nil
        graphviz-dot-indent-width 2)
  :ensure t)

;; `byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
(add-hook 'after-save-hook #'aethanyc-byte-compile-current-buffer)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
