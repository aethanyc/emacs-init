;;;-------------------------------------------------------------------
;;; Entry point of my customization files.

(defvar my-init-directory (concat user-emacs-directory "user/"))

(defvar my-init-files '("packages"
                        "elisp-functions"
                        "display"
                        "file"
                        "edit"
                        "alias"
                        "keybindings"
                        "programming"
                        )
  "The names of my customization files.")

;; For Common Lisp functions.
(require 'cl)

;; Load all init files.
(dolist (file my-init-files)
  (load (concat my-init-directory file)))

;; Start server if it is not running.
;; Solution to the problem "server directory is unsafe on Windows."
;; http://stackoverflow.com/questions/5233041/emacs-and-the-server-unsafe-error
(require 'server)
(when (not (server-running-p))
  (server-start))
