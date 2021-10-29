;;; aethanyc-keybindings.el --- Useful Keybindings

;; Copyright (C) 2013-2021 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets a lot of useful keybindings.

;;; Code:

(eval-when-compile
  (require 'bind-key))

(require 'aethanyc-core)

;; Basic keys
(bind-key [f2] 'switch-to-buffer)
(bind-key [f12] #'execute-extended-command)

(bind-key "<C-return>" #'aethanyc-open-line-below)
(bind-key "<C-S-return>" #'aethanyc-open-line-above)

;; Frame operations
(bind-key "M-`" #'other-frame)
(bind-key "M-0" #'aethanyc-delete-window-or-frame)
(bind-key "0" #'aethanyc-delete-window-or-frame ctl-x-map)
(bind-key "M-1" #'delete-other-windows)
(bind-key "M-2" #'aethanyc-split-window-below)
(bind-key "M-3" #'aethanyc-split-window-right)
(bind-key "M-4" #'ctl-x-4-prefix)
(bind-key "M-5" #'ctl-x-5-prefix)
(bind-key "M-7" #'aethanyc-select-current-line)

;; Apropos more info.
(bind-key "a" #'apropos help-map)

;; `hippie-expand' is more powerful than dabbrev-expand
(bind-key "M-/" #'hippie-expand)

;; M-^ is join-line
(bind-key "C-^" #'aethanyc-join-next-line)

;; Smarter move to beginning of the line
(bind-key [remap move-beginning-of-line] #'aethanyc-move-beginning-of-line)

;; Remap goto-line
(bind-key [remap goto-line] #'aethanyc-goto-line-with-feedback)

;; Toggle the letter case of current word or text selection between:
;; 'all lower', 'Init Caps', 'ALL CAPS'.
(bind-key "M-z" #'aethanyc-toggle-letter-case)

;; Switch between two recently used buffers.
(bind-key "C-x C-b" #'aethanyc-switch-to-recent-buffer)

;; Kill current buffer
(bind-key "C-x C-k" #'aethanyc-kill-current-buffer)

;; Open current buffer in a web browser.
(bind-key "C-c C-v" #'browse-url-of-buffer)

;; Open imenu to jump to definition in current buffer.
(bind-key "C-c i" #'imenu)

;; Quit minibuffer without focusing on it.
(bind-key [remap keyboard-quit] #'aethanyc-almighty-quit)


(provide 'aethanyc-keybindings)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-keybindings.el ends here
