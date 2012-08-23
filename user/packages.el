;;;-------------------------------------------------------------------
;;; Emacs 24 Package settings
(defvar my-packages '(zenburn-theme
                      undo-tree
                      paredit
                      ace-jump-mode
                      ido-ubiquitous
                      smex
                      smooth-scrolling
                      magit
                      maxframe
                      ack-and-a-half
                      markdown-mode
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
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ace Jump Mode
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(ace-jump-mode-enable-mark-sync)
(global-set-key (kbd "C-c f") 'ace-jump-mode)
(global-set-key (kbd "C-c b") 'ace-jump-mode-pop-mark)

;; Smex settings
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Markdown Mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode) t)
