;;;-------------------------------------------------------------------
;;; File Settings

;; Stop creating those backup~ files
(setq-default make-backup-files nil)

;; Stop creating those #autosave# files
(setq-default auto-save-default nil)

;; Keep a list of recently opened files
;; (recentf-mode 1)
;; (setq-default recentf-save-file (concat user-emacs-directory "recentf"))

;; http://emacswiki.org/emacs/InteractivelyDoThings
(ido-mode 1)
(ido-ubiquitous-mode 1)
(setq-default ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-max-prospects 8
              ido-save-directory-list-file (concat user-emacs-directory "ido.last"))

;; Enable "Finding Files and URLs at Point."
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#FFAP
;; (require 'ffap)

;; Unique buffer name
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; Reload the buffers automatically if they are changed outside.
(global-auto-revert-mode 1)
