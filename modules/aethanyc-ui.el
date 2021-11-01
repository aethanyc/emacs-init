;;; aethanyc-ui.el --- UI tweaks -*- lexical-binding:t -*-

;; Copyright (C) 2013-2021 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file tweaks UI such as font, frame, theme, mode line, etc.

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'aethanyc-core)
(require 'diminish)
(require 'bind-key)

(defvar aethanyc-fonts-alist '(("PragmataPro" . ascii)
                               ("Droid Sans Mono" . ascii)
                               ("Consolas" . ascii)
                               ("Lantinghei TC" . unicode))
  "Alist of prefered font name with its charset.
The value should be an alist of elements (FONT . CHARSET).")

(defvar aethanyc-font-size (if (eq system-type 'darwin) 13 12))

;; C-u M-x list-fontsets: list the fonts contained in each fontset.
;; C-h r m Fontsets <RET>: see the info about fontset.
;; M-x describe-char or C-u C-x =: see the char info under cursor.
(create-fontset-from-fontset-spec standard-fontset-spec)
(dolist (font-charset (reverse aethanyc-fonts-alist))
  (let ((font-name-string (format "%s-%d" (car font-charset) aethanyc-font-size))
        (charset (cdr font-charset)))
    (set-fontset-font "fontset-standard" charset font-name-string nil 'prepend)))

;; Set the second parameter of `set-frame-font' to t so that the
;; initial frame would have proper maximized height and width. The
;; first font should be available or `set-frame-font' will fail.
(when (member (caar aethanyc-fonts-alist) (font-family-list))
  (set-frame-font "fontset-standard" t t))

;; Set this so that the subsequent frames could use the correct fonts.
(add-to-list 'default-frame-alist '(font . "fontset-standard"))

;; Make frames a bit wider.
(add-to-list 'default-frame-alist '(width . 90))

(setq frame-title-format '("%b" (buffer-file-name ": %f")))

(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(winner-mode 1)

(setq blink-matching-paren nil
      inhibit-startup-screen t
      visible-bell nil
      ring-bell-function #'ignore)

;; Install:
;; 1. See https://github.com/seagle0128/doom-modeline#use-package
;; 2. M-x all-the-icons-install-fonts
(use-package doom-modeline
  :config (doom-modeline-mode 1)
  :ensure t)


(use-package ediff-util
  :config
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))


(use-package highlight-symbol
  :bind (("C-<f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-S-<f3>" . highlight-symbol-query-replace)
         ("C-M-<f3>" . highlight-symbol-remove-all))
  :config
  (eval-when-compile
    (require 'zenburn-theme)
    (zenburn-with-color-variables
      (setq highlight-symbol-foreground-color `,zenburn-bg-1)
      (setq highlight-symbol-colors
            `(,zenburn-yellow ,zenburn-cyan ,zenburn-magenta ,zenburn-blue+1
                              ,zenburn-red+2 ,zenburn-green+4 ,zenburn-orange))))
  :diminish
  :ensure t)


(use-package paradox
  :config
  (setq paradox-github-token t)
  (use-package async
    :ensure t)
  :ensure t)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)


;; It is useful to visualize a string representing a color in html.
(use-package rainbow-mode
  :hook ((html-mode web-mode) . rainbow-mode)
  :ensure t)


(use-package zenburn-theme
  :config
  (setq zenburn-add-font-lock-keywords t)
  (load-theme 'zenburn t)
  :ensure t)


(provide 'aethanyc-ui)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-ui.el ends here
