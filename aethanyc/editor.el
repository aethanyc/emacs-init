;;;-------------------------------------------------------------------
;;; Editor

;; Start server if it is not running.
;; Solution to the problem "server directory is unsafe on Windows."
;; http://stackoverflow.com/questions/5233041/emacs-and-the-server-unsafe-error
(require 'server)
(setq server-auth-dir aethanyc-save-file-directory)
(when (not (server-running-p))
  (server-start))

;; Delete the seleted text when typing.
(delete-selection-mode 1)

;; Paste at the cursor position.
(setq mouse-yank-at-point t)

;; Do not use tabs to indent code.
(setq-default indent-tabs-mode nil)

;; Set the default mode as text-mode.
(setq-default major-mode 'text-mode)

;; Adjust the behavior of the hippie-expand
(setq-default hippie-expand-try-functions-list
              '(try-complete-file-name
                try-expand-all-abbrevs
                try-expand-dabbrev
                try-expand-dabbrev-all-buffers
                try-expand-dabbrev-from-kill
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol
                try-complete-file-name-partially
                try-expand-list
                try-expand-line))

;; Set whitespace mode styles
(setq-default whitespace-style '(face trailing lines-tail tabs empty indentation))

;; Save the last place of the cursor.
(setq-default save-place t)
(setq-default save-place-file (concat aethanyc-save-file-directory "save-place"))
(require 'saveplace)

;; Stop creating those backup~ files
(setq-default make-backup-files nil)

;; Stop creating those #autosave# files
(setq-default auto-save-default nil
              auto-save-list-file-prefix nil)

;; Keep a list of recently opened files
(setq-default recentf-save-file (concat aethanyc-save-file-directory "recentf"))
(recentf-mode 1)

;; ido mode
(ido-mode 1)
(ido-ubiquitous-mode 1)
(setq-default ido-enable-flex-matching t)
(setq-default ido-use-filename-at-point 'guess)
(setq-default ido-max-prospects 8)
(setq-default ido-save-directory-list-file (concat aethanyc-save-file-directory "ido-last"))

;; Enable "Finding Files and URLs at Point."
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#FFAP
;; (require 'ffap)

;; Unique buffer name
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)

;; Remove the all the version control backends to increase file open speed.
;; http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
(setq vc-handled-backends nil)

;; Do Not kill the whole line including the line ending. When using
;; paredit-mode to kill the whole s-expr, this will kill the blank
;; line after the s-expr.
(setq kill-whole-line nil)
