;;;-------------------------------------------------------------------
;;; Entry point of my customization files.

(defvar aethanyc-init-directory (concat user-emacs-directory "aethanyc/")
  "This folder contains all my customization files.")

(defvar aethanyc-save-file-directory (concat user-emacs-directory "save-file/")
  "This folder contains all the automatically generated files.")

(defvar aethanyc-init-files '("packages"
			      "elisp-functions"
			      "ui"
			      "editor"
			      "mode-hooks"
			      "alias"
			      "keybindings"
			      )
  "The names of my customization files.")

;; For Common Lisp functions.
(require 'cl)

;; Set coding system to utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Load all init files.
(dolist (file aethanyc-init-files)
  (load (concat aethanyc-init-directory file)))
