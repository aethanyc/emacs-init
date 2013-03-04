;;;-------------------------------------------------------------------
;;; Display Settings

(defvar my-font-list '("Pragmata" "Droid Sans Mono" "Consolas"))
(defvar my-font-size 12)

(defun find-first-available-font (font-list)
  "Find the first avaliable font in the font-list."
  (while (and font-list
              (not (member (car font-list) (font-family-list))))
    (setq font-list (cdr font-list)))
  (car font-list))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)

  ;; Set font.
  (let ((font (find-first-available-font my-font-list)))
    (when font
      (set-frame-font (format "%s-%d" font my-font-size) nil t)))

  ;; Set frame title.
  (setq-default frame-title-format '("%b" (buffer-file-name ": %f")))

  ;; Set frame properties.
  ;; (add-to-list 'initial-frame-alist '(top . 0) t)
  ;; (add-to-list 'initial-frame-alist '(left . 0) t)
  ;; (add-to-list 'default-frame-alist '(width . 90) t)
  ;; (add-to-list 'default-frame-alist '(height . 45) t)
  )

(setq-default inhibit-startup-screen t
              visible-bell t)

;; Use zenburn theme.
(load-theme 'zenburn t)

;; Turn on highlight matched parentheses.
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil :inverse-video t)
(set-face-attribute 'show-paren-mismatch nil :inverse-video t)

;; Turn on highlighting current line.
;; (global-hl-line-mode 1)

;; Display line numbers in the left margin.
;; (global-linum-mode 1)
;; (setq-default linum-format "%5d|")

;; Display line and column number in the status bar.
(column-number-mode 1)

;; Let ediff split window horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

;; Put ediff control panel in single frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; maxframe
(require 'maxframe)
;; Pixel width of main monitor.
(setq mf-max-width 1920)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Clean mode line
;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;; There is a diminish mode. But it requires the minor mode to be active to be diminished.
(defvar mode-line-cleaner-alist
  '(;; Minor modes
    (abbrev-mode "")
    (auto-complete-mode " AC")
    (auto-fill-function " Fi")
    (eldoc-mode "")
    (hi-lock-mode "")
    (highlight-symbol-mode "")
    (paredit-mode " Par")
    (projectile-mode " Proj")
    (undo-tree-mode "")
    ;; Major modes
    (emacs-lisp-mode "EL")
    (js2-mode "JS")
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
