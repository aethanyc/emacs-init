;;; aethanyc-ui.el --- UI tweaks

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file tweaks UI such as font, frame, theme, mode line, etc.

;;; Code:

(defvar aethanyc-fonts '("PragmataPro" "Droid Sans Mono" "Consolas"))
(defvar aethanyc-font-size (if (eq system-type 'darwin) 13 12))

(when (display-graphic-p)
  (set-frame-font (font-spec :name (aethanyc-font-candidate aethanyc-fonts)
                             :size aethanyc-font-size) t t))

(setq frame-title-format '("%b" (buffer-file-name ": %f")))
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(column-number-mode 1)

;; Prevent cursor going into minibuffer prompt. This is the same as:
;; M-x customize-variable <RET> minibuffer-prompt-properties <RET>
;; Select "Don't enter" option and save.
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Let ediff split window horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

;; Put ediff control panel in single frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(use-package git-gutter-fringe
  :disabled t
  :ensure git-gutter-fringe)


(use-package highlight-symbol
  :bind (("<C-f3>" . highlight-symbol-at-point)
         ("<f3>" . highlight-symbol-next)
         ("<S-f3>" . highlight-symbol-prev)
         ("<M-S-f3>" . highlight-symbol-query-replace))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p t)
    (set-face-attribute 'highlight-symbol-face nil :background "gray35"))
  :ensure highlight-symbol)


(use-package rainbow-delimiters
  :init (global-rainbow-delimiters-mode)
  :ensure rainbow-delimiters)


;; Sometimes it is useful to color text that represent colors.
(use-package rainbow-mode
  :defer t
  :ensure rainbow-mode)


(use-package zenburn-theme
  :init (load-theme 'zenburn t)
  :ensure zenburn-theme)


;; Frame operations

(defun aethanyc-toggle-frame-fullscreen-state (parameter)
  "Helper function to set the fullscreen parameter of a frame."
  (set-frame-parameter nil 'fullscreen
                       (if (eq (frame-parameter nil 'fullscreen) parameter)
                           nil
                         parameter)))

(defun aethanyc-toggle-frame-maximized ()
  "Toggle the frame state between maximized and windowed.

A maximized frame still has window manager decorations."
  (interactive)
  (aethanyc-toggle-frame-fullscreen-state 'maximized))

(defun aethanyc-toggle-frame-fullscreen ()
  "Toggle the frame state between fullscreen and windowed."
  (interactive)
  (aethanyc-toggle-frame-fullscreen-state 'fullboth))

(bind-key "<f11>" 'aethanyc-toggle-frame-maximized)
(bind-key "<M-f11>" 'aethanyc-toggle-frame-fullscreen)


;; Window operations

;; Prompt for a target window when there are more than 2.
(use-package switch-window
  :init (setq switch-window-shortcut-style 'qwerty
              switch-window-qwerty-shortcuts '("a" "o" "e" "u" "h" "t"))
  :bind ("M-o" . switch-window)
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
