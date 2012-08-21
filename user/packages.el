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

;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;; http://www.dr-qubit.org/emacs.php#undo-tree
;; C-x u: undo-tree-visualize
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ace Jump Mode
;; https://github.com/winterTTr/ace-jump-mode
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
;; https://github.com/jhelwig/ack-and-a-half
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Markdown Mode
;; http://daringfireball.net/projects/markdown/syntax
;; http://jblevins.org/projects/markdown-mode/
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode) t)
