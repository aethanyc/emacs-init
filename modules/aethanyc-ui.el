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

(setq frame-title-format '("%b" (buffer-file-name ": %f")))

(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

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
  :init (global-anzu-mode 1)
  :diminish ""
  :ensure anzu)


(use-package ediff-util
  :init
  (progn
    ;; Let ediff split window horizontally rather than vertically.
    (setq ediff-split-window-function 'split-window-horizontally)))


(use-package highlight-symbol
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :bind (("<C-f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-S-f3>" . highlight-symbol-query-replace))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p t)
    (set-face-attribute 'highlight-symbol-face nil :background "gray35"))
  :diminish ""
  :ensure highlight-symbol)


(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure rainbow-delimiters)


;; Sometimes it is useful to color text that represent colors.
(use-package rainbow-mode
  :defer t
  :ensure rainbow-mode)


(use-package whitespace
  :init
  (progn
    (setq-default whitespace-style '(face trailing tab-mark empty))
    ;; Clean up white spaces before saving a buffer.
    (add-hook 'before-save-hook 'whitespace-cleanup)

    ;; Highlight trailing whitespaces only in file buffer.
    (defun whitespace-setup ()
      (setq show-trailing-whitespace t))
    (add-hook 'find-file-hook 'whitespace-setup)))


(use-package zenburn-theme
  :init (load-theme 'zenburn t)
  :ensure zenburn-theme)


;; Frame operations

(defun aethanyc-toggle-frame-maximized ()
  "Toggle maximization state of the selected frame.

This function sends w32 command to toggle frame maximized on
Windows, and use `set-frame-parameter' on other systems"
  (interactive)
  (let ((status (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (eq status 'maximized) nil 'maximized))
    ;; Fix frame maximized does not work on Windows.
    ;; See the document of w32-send-sys-command for more system commands.
    (when (eq system-type 'windows-nt)
      (w32-send-sys-command (if (eq status 'maximized) #xf120 #xf030)))))

(bind-key "<M-f10>" 'aethanyc-toggle-frame-maximized)


;; Window operations

;; Prompt for a target window when there are more than 2.
(use-package switch-window
  :init
  (progn
    (setq switch-window-shortcut-style 'qwerty
          switch-window-qwerty-shortcuts '("a" "o" "e" "u" "h" "t"))
    (global-set-key [remap other-window] 'switch-window)
    (bind-key* "M-o" 'switch-window))
  :ensure switch-window)

;; Using S-arrow keys to move between windows.
(windmove-default-keybindings)

;; Using C-S-arrow keys to move buffer to a new place.
(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right))
  :ensure buffer-move)


(provide 'aethanyc-ui)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-ui.el ends here
