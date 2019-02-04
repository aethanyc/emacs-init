;;; aethanyc-core.el --- Core utility functions

;; Copyright (C) 2013-2019 Ting-Yu Lin

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
  "`byte-compile' current buffer if it's `emacs-lisp-mode' and compiled file exists."
  (interactive)
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (byte-compile-file buffer-file-name)))


;; This is adapted from `delete-window' in:
;; http://www.emacswiki.org/emacs/frame-cmds.el
(defun aethanyc-delete-window-or-frame ()
  "Delete current window or frame.
Remove `select-window' from the display.  If it is the only one in
its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (if (one-window-p t) (delete-frame) (delete-window))))


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


;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun aethanyc-select-current-line ()
  "Select the current line.

Note that the mark goes to the end of the line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))


;; http://whattheemacsd.com/key-bindings.el-01.html
(defun aethanyc-goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (if display-line-numbers
      (call-interactively #'goto-line)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (call-interactively #'goto-line))
      (display-line-numbers-mode -1))))


;; http://whattheemacsd.com/editing-defuns.el-01.html
(defun aethanyc-open-line-below ()
  "Open a new line below the cursor."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))


(defun aethanyc-open-line-above ()
  "Open a new line above the cursor."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


;; http://www.masteringemacs.org/articles/2014/02/28/my-emacs-keybindings/
(defun aethanyc-kill-current-buffer ()
  "Kill current buffer without asking."
  (interactive)
  (kill-buffer (current-buffer)))


;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun aethanyc-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: 'all lower', 'Init Caps', 'ALL CAPS'."
  (interactive)
  (let (p1 p2 (deactivate-mark t) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (and p1 p2)
      (when (not (eq last-command this-command))
        (save-excursion
          (goto-char p1)
          (cond
           ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
           ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
           ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
           ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
           ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
           (t (put this-command 'state "all lower")))))

      (cond
       ((string= "all lower" (get this-command 'state))
        (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
       ((string= "init caps" (get this-command 'state))
        (upcase-region p1 p2) (put this-command 'state "all caps"))
       ((string= "all caps" (get this-command 'state))
        (downcase-region p1 p2) (put this-command 'state "all lower"))))))


;; http://www.emacswiki.org/emacs/SwitchingBuffers#toc5
(defun aethanyc-switch-to-recent-buffer ()
  "Switch between two recently used buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))


;; http://ergoemacs.org/emacs/emacs_copy_file_path.html
;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun aethanyc-copy-path-name-to-clipboard (&optional file-name-only-p)
  "Copy the current buffer's full path to `kill-ring'.
With `universal-argument' FILE-NAME-ONLY-P, copy only the file name."
  (interactive "P")
  (let ((full-path-name (buffer-file-name)))
    (if full-path-name
        (let ((name (if file-name-only-p
                        (file-name-nondirectory full-path-name)
                        full-path-name)))
          (kill-new name)
          (message "%s" name))
      (message "Buffer not associated with a file."))))


;; http://trey-jackson.blogspot.tw/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun aethanyc-almighty-quit ()
  "Exit recursive edit or minibuffer, and signal keyboard quit."
  (interactive)
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit))
  (keyboard-quit))


(defun aethanyc-split-window-below (&optional no-balance-window-p)
  "Call `split-window-below' and balance the size of the windows.

If NO-BALANCE-WINDOW-P is non-nil, windows will not be balanced
after splitting."
  (interactive "P")
  (split-window-below)
  (unless no-balance-window-p
    (balance-windows)))


(defun aethanyc-split-window-right (&optional no-balance-window-p)
  "Call `split-window-right', and balance the size of the windows.

If NO-BALANCE-WINDOW-P is non-nil, windows will not be balanced
after splitting."
  (interactive "P")
  (split-window-right)
  (unless no-balance-window-p
    (balance-windows)))


(provide 'aethanyc-core)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-core.el ends here
