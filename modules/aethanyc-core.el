;;; aethanyc-core.el --- Core utility functions

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file hosts core utility functions.

;;; Code:

(defun aethanyc-join-next-line ()
  "Join the next line with current line."
  (interactive)
  (join-line 1))

;; http://ergoemacs.org/emacs/organize_your_dot_emacs.html
(defun aethanyc-byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defun aethanyc-find-user-init-file ()
  "Open user init file quickly."
  (interactive)
  (find-file user-init-file))

;; This is adapted from `delete-window' in:
;; http://www.emacswiki.org/emacs/frame-cmds.el
(defun aethanyc-delete-window-or-frame ()
  "Remove `select-window' from the display. If it is the only one
in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (if (one-window-p t) (delete-frame) (delete-window))))

;; This is adapted from `find-function-or-variable-at-point' in:
;; http://www.emacswiki.org/emacs/find-func-extension.el
(defun aethanyc-find-at-point (&optional other-window)
  "Find function or variable at point."
  (interactive "P")
  (let ((vsymb (variable-at-point))
        (fsymb (function-called-at-point)))
    ;; Push current position to the global mark ring.
    (push-mark)
    ;; (function-called-at-point) gets function name symbol even if
    ;; cursor is on a variable name, so we check vsymb first.
    (cond ((and vsymb (not (equal vsymb 0)))
           (funcall (if other-window 'find-variable-other-window 'find-variable)
                    vsymb))
          (fsymb
           (funcall (if other-window 'find-function-other-window 'find-function)
                    fsymb))
          (t (message "Neither function nor variable at point.")))))

(defun aethanyc-font-candidate (fonts)
  "Return the existing font which first matched."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(defun aethanyc-hook-into-modes (function mode-hooks)
  "Add FUNCTION to each hooks in MODE-HOOKS."
  (dolist (mode-hook mode-hooks)
    (add-hook mode-hook function)))

;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun aethanyc-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; http://whattheemacsd.com/key-bindings.el-01.html
(defun aethanyc-goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (if (and (boundp 'linum-mode)
           linum-mode)
      (call-interactively 'goto-line)
    (unwind-protect
        (progn
          (linum-mode 1)
          (call-interactively 'goto-line))
      (linum-mode -1))))


(provide 'aethanyc-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-core.el ends here
