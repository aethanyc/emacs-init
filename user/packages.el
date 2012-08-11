;;;-------------------------------------------------------------------
;;; Emacs 24 Package settings
;;; http://ergoemacs.org/emacs/emacs_package_system.html
;;; http://melpa.milkbox.net/
(defvar my-packages '(zenburn-theme
                      undo-tree
                      paredit
                      ace-jump-mode
                      ido-ubiquitous
                      smex
                      smooth-scrolling
                      magit
                      )
  "A list of packages to ensure are installed at launch.")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;;;-------------------------------------------------------------------
;;; Settings of other packages

;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;; http://www.dr-qubit.org/emacs.php#undo-tree
;; C-x u: undo-tree-visualize
(require 'undo-tree)
(global-undo-tree-mode 1)

;; http://www.emacswiki.org/emacs/AceJump
(require 'ace-jump-mode)
(setq-default ace-jump-mode-submode-list
              '(ace-jump-char-mode
                ace-jump-line-mode
                ace-jump-word-mode))
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Smex settings
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
