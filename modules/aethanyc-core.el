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

;; http://emacswiki.org/emacs/InteractivelyDoThings#toc16
(defun aethanyc-ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

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
(defun aethanyc-find-function-or-variable-at-point (&optional other-window)
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

(provide 'aethanyc-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-core.el ends here
