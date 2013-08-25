;;; aethanyc-ui.el --- UI tweaks

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file tweaks UI such as font, frame, theme, mode line, etc.

;;; Code:

(defvar my-fonts '("PragmataPro" "Droid Sans Mono" "Consolas"))
(defvar my-font-size (if (eq system-type 'darwin) 13 12))

(defun set-my-font (&optional frame)
  (set-frame-font (format "%s-%d" (first my-fonts) my-font-size) nil t))

;; Set font when initialzing
(when (display-graphic-p)
  (set-my-font))

;; When launch emacs daemon by emacs --daemon, the font cannot be set.
;; We have to set font again after the frame is created.
(add-hook 'after-make-frame-functions 'set-my-font)

(setq-default frame-title-format '("%b" (buffer-file-name ": %f")))
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)

;; Prevent cursor going into minibuffer prompt. This is the same as:
;; M-x customize-variable <RET> minibuffer-prompt-properties <RET>
;; Select "Don't enter" option and save.
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Use zenburn theme.
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; Turn on highlight matched parentheses.
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil :inverse-video t)
(set-face-attribute 'show-paren-mismatch nil :inverse-video t)

;; Turn on highlighting current line.
;; (global-hl-line-mode 1)

;; Display line and column number in the status bar.
(column-number-mode 1)

;; Let ediff split window horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

;; Put ediff control panel in single frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Clean mode line
;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;; There is a diminish mode. But it requires the minor mode to be active to be diminished.
(defvar mode-line-cleaner-alist
  '(;; Minor modes
    (abbrev-mode "")
    (auto-complete-mode "")
    (auto-fill-function " Fi")
    (eldoc-mode "")
    (hi-lock-mode "")
    (highlight-symbol-mode "")
    (paredit-mode " Par")
    (projectile-mode " Proj")
    (undo-tree-mode "")
    (yas-minor-mode "")
    ;; Major modes
    (emacs-lisp-mode "EL")
    (inferior-python-mode "Inf Py")
    (js-mode "JS")
    (lisp-interaction-mode "LI")
    (python-mode "Py"))
  "Alist for `clean-mode-line'.

Be sure to add the correct major/minor mode symbol, and the string you
want to use in the mode line. See minor-mode-alist for the original alist.")

(defun clean-mode-line ()
  (interactive)
  (dolist (cleaner mode-line-cleaner-alist)
    (let* ((mode (first cleaner))
           (mode-str (second cleaner))
           (old-mode (assq mode minor-mode-alist)))
      ;; Replace the minor mode string
      (when old-mode
        (setcar (cdr old-mode) mode-str))
      ;; Replace the major mode string
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'aethanyc-ui)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-ui.el ends here
