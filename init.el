;;;-------------------------------------------------------------------
;;; Entry point of my customization files.

(defvar aethanyc-init-directory (concat user-emacs-directory "aethanyc/")
  "This folder contains all my customization files.")

(defvar aethanyc-save-file-directory "~/.emacs-save-file/"
  "This folder contains all the automatically generated files.")

;; The directory should be created. Otherwise the auto-generated files
;; cannot be saved.
(unless (file-exists-p aethanyc-save-file-directory)
  (make-directory aethanyc-save-file-directory))

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
;; (set-selection-coding-system 'utf-8)

;; Add my init directory to `load-path' for convenience.
(add-to-list 'load-path aethanyc-init-directory)

;; Load all init files.
(dolist (file aethanyc-init-files)
  (load (concat aethanyc-init-directory file)))
