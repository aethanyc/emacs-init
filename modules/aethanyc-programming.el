;;; aethanyc-programming.el --- Settings for programming

;; Copyright (C) 2013-2019 Ting-Yu Lin

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
  :hook ((css-mode prog-mode sgml-mode) . display-line-numbers-mode))


(use-package flycheck
  :init
  ;; C-c ! ? to describe the syntax checker.
  (setq-default flycheck-clang-include-path '(".")
                flycheck-clang-language-standard "c++17"
                flycheck-emacs-lisp-initialize-packages t
                flycheck-emacs-lisp-load-path load-path)
  :hook (prog-mode . flycheck-mode)
  :ensure t)


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-session-file
        (expand-file-name "lsp-session-v1" aethanyc-savefiles-dir))
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  :custom-face
  (lsp-face-highlight-textual ((t :weight bold)))
  (lsp-face-highlight-read ((t :underline t)))
  (lsp-face-highlight-write ((t :slant italic)))
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :ensure t)


(use-package lsp-ui
  :config
  ;; Turn off distractions.
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  :ensure t)


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


(use-package company-lsp
  ;; lsp-auto-configure defaults to t, so we don't need to tweak
  ;; company-backends.
  :ensure t)


;;; Lisp Mode

(use-package eldoc
  :hook ((lisp-mode emacs-lisp-mode ielm-mode) . eldoc-mode)
  :diminish)


(use-package elisp-slime-nav
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
  :diminish
  :ensure t)


(use-package elisp-mode
  :demand t
  :bind (:map emacs-lisp-mode-map
         ("C-c v" . eval-buffer)
         :map lisp-interaction-mode-map
         ("C-c v" . eval-buffer))
  :config
  (defun setup-byte-compile-buffer-after-save ()
    (add-hook 'after-save-hook #'aethanyc-byte-compile-current-buffer nil t))
  :hook (emacs-lisp-mode . setup-byte-compile-buffer-after-save))


;;; C/C++ Mode

(use-package google-c-style
  :hook (((c-mode c++-mode) . google-set-c-style)
         ((c-mode c++-mode) . google-make-newline-indent))
  :ensure t)


(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c o" . ff-find-other-file)))


;; https://github.com/MaskRay/ccls
(use-package ccls
  :preface
  (setq ccls-executable
        (or (executable-find "~/Projects/ccls/Release/ccls") "ccls"))
  :if ccls-executable
  :config
  (setq ccls-initialization-options '(:cache (:directory "obj-ccls-cache")))
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))
  :ensure t)


(use-package clang-format
  :config
  ;; Use clang-format installed by ./mach bootstrap
  (setq clang-format-executable
        (or (expand-file-name "~/.mozbuild/clang-tools/clang-tidy/bin/clang-format")
            (executable-find "clang-format")))
  (defun clang-format-before-save ()
    ;; Use clang-format-buffer only when finding .clang-format in the project
    ;; root.
    (when (locate-dominating-file "." ".clang-format")
      (clang-format-buffer)))
  (defun setup-clang-format-before-save ()
    (add-hook 'before-save-hook #'clang-format-before-save nil t))
  :hook ((c-mode c++-mode) . setup-clang-format-before-save)
  :ensure t)


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
              (t "python")))
  :hook (python-mode . lsp))


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
        web-mode-sql-indent-offset 2
        web-mode-enable-auto-indentation nil)
  :ensure t)


;;; Rust Mode

;; lsp requires rls.
;; Install rls by running "rustup component add rls rust-analysis rust-src".
;; Setup instructions: https://github.com/rust-lang/rls
(use-package rust-mode
  :hook (rust-mode . lsp)
  :ensure t)


(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup)
  :requires (flycheck rust-mode)
  :ensure t)


;;; Other
(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-auto-indent-on-semi nil
        graphviz-dot-indent-width 2)
  :ensure t)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
