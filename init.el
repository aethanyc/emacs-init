;; -*- coding: utf-8-unix; -*-

;;;-------------------------------------------------------------------
;;; Entry point of my customization files.

;; See this document for the difference of a directory name and its
;; name as a file. In short, a directory name ends in a slash.
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html#Directory-Names

(defvar user-init-directory (file-name-as-directory (locate-user-emacs-file "user"))
  "This folder contains all my customization files.")

(defvar user-save-file-directory (file-name-as-directory "~/.emacs-save-file")
  "This folder contains all the automatically generated files.")

(defconst *is-a-mac* (eq system-type 'darwin))

;; The directory should be created. Otherwise the auto-generated files
;; cannot be saved.
(unless (file-exists-p user-save-file-directory)
  (make-directory user-save-file-directory))

(defvar user-init-files '(
                          "packages"
                          "elisp-functions"
                          "ui"
                          "editor"
                          "mode-hooks"
                          "alias"
                          "keybindings"
                          )
  "The names of my customization files.")

;; Set coding system to utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)

;; Add user init directory to `load-path' for convenience.
(add-to-list 'load-path user-init-directory)

;; Load all init files.
(dolist (file user-init-files)
  (load (concat user-init-directory file)))
