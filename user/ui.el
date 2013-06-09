;; -*- coding: utf-8-unix; -*-

;;;-------------------------------------------------------------------
;;; Display Settings

(defvar my-fonts '("Pragmata" "Droid Sans Mono" "Consolas"))
(defvar my-font-size 12)

(defun set-my-font (&optional frame)
  (set-frame-font (format "%s-%d" (first my-fonts) my-font-size) nil t))

;; Set font when initialzing
(when (display-graphic-p)
  (set-my-font))

;; When launch emacs daemon by emacs --daemon, the font cannot be set.
;; We have to set font again after the frame is created.
(add-hook 'after-make-frame-functions 'set-my-font)

;; Set frame and UI properties.
(setq-default frame-title-format '("%b" (buffer-file-name ": %f")))
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(setq-default inhibit-startup-screen t
              visible-bell t)

;;; maxframe
(require 'maxframe)
;; Pixel width and height of my main monitor.
(setq mf-max-width 1920
      mf-max-height 1080)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Make the default frame in two columns
(add-hook 'window-setup-hook 'split-window-horizontally t)

;; Use zenburn theme.
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
