;;; aethanyc-programming.el --- Settings for programming -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Ting-Yu Lin

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
  :hook ((prog-mode sgml-mode conf-mode) . display-line-numbers-mode))


(use-package bug-reference
  :config
  ;; This variable can also set in .dir-locals.el per project.
  (setq bug-reference-url-format "https://bugzilla.mozilla.org/show_bug.cgi?id=%s")
  :hook (prog-mode . bug-reference-prog-mode))


(use-package xref
  :config
  ;; If there's more than one definitions, use completing-read to select them in
  ;; minibuffer rather than in a new buffer.
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))


(use-package flycheck
  :init
  ;; C-c ! ? to describe the syntax checker.
  (setq-default flycheck-clang-include-path '(".")
                flycheck-emacs-lisp-initialize-packages t
                flycheck-emacs-lisp-load-path load-path)
  :hook (prog-mode . flycheck-mode)
  :ensure t)


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :ensure t)


(use-package lsp-ui
  :config
  ;; Turn off distractions.
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-enable nil)
  :ensure t)


(use-package company
  :config
  (global-company-mode 1)
  :diminish
  :ensure t)


(use-package company-dabbrev-code
  :config
  (setq company-dabbrev-code-everywhere t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  :requires company)


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
  :hook ((c-mode c++-mode) . google-set-c-style)
  :ensure t)


(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c o" . ff-find-other-file)))


;; lsp-mode for C/C++ requires clangd.
;; (Re-)install clangd: C-u M-x lsp-install-server, choose `clangd'.
(use-package lsp-clangd
  :config (setq lsp-clangd-binary-path "~/.mozbuild/clang/bin/clangd")
  :hook ((c-mode c++-mode) . lsp))


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

;; Install python-lsp-server: pip3 install 'python-lsp-server[all]'
;; https://github.com/python-lsp/python-lsp-server
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
  (setq css-indent-offset 2))


(use-package browse-url
  :config
  (setq browse-url-handlers
        '(("\\.\\(html?\\|xhtml\\|xht\\)\\'" . browse-url-default-browser))))


(use-package web-mode
  :mode "\\.\\(html?\\|xhtml\\|xht\\)\\'"
  :bind (:map web-mode-map
              ("C-c C-v" . browse-url-of-buffer))
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

;; lsp-mode requires rust-analyzer.
;; (Re-)install rust-analyzer: C-u M-x lsp-install-server, choose rust-analyzer.
(use-package rustic
  :config
  (setq rustic-format-trigger 'on-save)
  (defun setup-rustic-mode ()
    (setq fill-column 100))
  :hook (rustic-mode . setup-rustic-mode)
  :ensure t)


(use-package lsp-rust
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#inlay-hints
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)
  :requires lsp-mode)


(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup)
  :requires flycheck
  :ensure t)


;;; Other
(use-package graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 2)
  :ensure t)


(provide 'aethanyc-programming)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-programming.el ends here
