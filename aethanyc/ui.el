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
              visible-bell t
              indicate-empty-lines t)

;; Use zenburn theme.
(load-theme 'zenburn t)

;; Turn on highlight matched parentheses.
(show-paren-mode 1)

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