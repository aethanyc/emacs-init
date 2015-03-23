;;; aethanyc-ui.el --- UI tweaks

;; Copyright (C) 2013 Ting-Yu Lin

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
      visible-bell t)

;; Prevent cursor going into minibuffer prompt. This is the same as:
;; M-x customize-variable <RET> minibuffer-prompt-properties <RET>
;; Select "Don't enter" option and save.
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))


;; Show number of matches in mode-line while searching
(use-package anzu
  :config (global-anzu-mode 1)
  :diminish anzu-mode
  :ensure t)


(use-package ediff-util
  :defer t
  :init
  (setq ediff-split-window-function #'split-window-horizontally)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))


(use-package highlight-symbol
  :init (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :bind (("<C-f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-S-f3>" . highlight-symbol-query-replace))
  :config
  (setq highlight-symbol-on-navigation-p t)
  (set-face-attribute 'highlight-symbol-face nil :background "gray35")
  :diminish highlight-symbol-mode
  :ensure t)


;; Add spaces between Chinese and English characters.
(use-package pangu-spacing
  :init
  (progn
    (setq pangu-spacing-real-insert-separtor t)
    (global-pangu-spacing-mode 1))
  :diminish ""
  :ensure pangu-spacing)


(use-package paradox
  :config
  (progn
    (setq paradox-github-token t)

    (use-package async
      :ensure t))
  :ensure paradox)


(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :ensure rainbow-delimiters)


;; Sometimes it is useful to color text that represent colors.
(use-package rainbow-mode
  :defer t
  :ensure rainbow-mode)


(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/name-width 36)
    (sml/setup)
    (sml/apply-theme 'respectful))
  :ensure smart-mode-line)


(use-package zenburn-theme
  :init
  (progn
    (load-theme 'zenburn t)
    (set-face-attribute 'region nil
                        :foreground (cdr (assoc "zenburn-fg"
                                                zenburn-colors-alist))
                        :background (cdr (assoc "zenburn-bg+2"
                                                zenburn-colors-alist))))
  :ensure zenburn-theme)


(provide 'aethanyc-ui)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-ui.el ends here
