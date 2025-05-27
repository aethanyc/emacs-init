;;; early-init.el --- Settings loaded before init.el -*- lexical-binding:t -*-

;; Copyright (C) 2013-2025 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file loaded before the package system and GUI is initialized.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;;; Code:

;; Use plists for deserialization for lsp-mode.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")


;; Local Variables:
;; coding: utf-8
;; End:

;;; early-init.el ends here
