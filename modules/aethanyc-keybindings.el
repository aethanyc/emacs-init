;;; aethanyc-keybindings.el --- Useful Keybindings

;; Copyright (C) 2013 Ting-Yu Lin

;; Author: Ting-Yu Lin <aethanyc@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/aethanyc/emacs-init

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file sets a lot of useful keybindings.

;;; Code:

(eval-when-compile
  (require 'bind-key))

;; Basic keys
(bind-key "<C-return>" 'aethanyc-open-line-below)
(bind-key "<C-S-return>" 'aethanyc-open-line-above)

;; Frame operations
(bind-key "M-`" 'other-frame)
(bind-key "M-0" 'aethanyc-delete-window-or-frame)
(bind-key "0" 'aethanyc-delete-window-or-frame ctl-x-map)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-vertically)
(bind-key "M-3" 'split-window-horizontally)
(bind-key "M-4" 'ctl-x-4-prefix)
(bind-key "M-5" 'ctl-x-5-prefix)
(bind-key "M-7" 'aethanyc-select-current-line)

;; Use regex searches by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-M-%" 'query-replace)

;; Apropos more info.
(bind-key "a" 'apropos help-map)

;; `hippie-expand' is more powerful than dabbrev-expand
(bind-key "M-/" 'hippie-expand)

;; C-c keybindings
(bind-key "C-c r" 'revert-buffer)

;; M-^ is join-line
(bind-key "C-^" 'aethanyc-join-next-line)

;; Smarter move to beginning of the line
(global-set-key [remap move-beginning-of-line] 'aethanyc-move-beginning-of-line)

;; Remap goto-line
(global-set-key [remap goto-line] 'aethanyc-goto-line-with-feedback)

;; Smarter copy/cut region
(global-set-key [remap kill-region] 'aethanyc-kill-line-or-region)
(global-set-key [remap kill-ring-save] 'aethanyc-save-line-or-region)

;; Toggle the letter case of current word or text selection between:
;; 'all lower', 'Init Caps', 'ALL CAPS'.
(bind-key "M-z" 'aethanyc-toggle-letter-case)

;; It is easier to switch buffer on single key.
(bind-key "<f2>" 'ido-switch-buffer)

;; Switch between two recently used buffers.
(bind-key "C-x C-b" 'aethanyc-switch-to-recent-buffer)

;; Kill current buffer
(bind-key "C-x C-k" 'aethanyc-kill-current-buffer)


(provide 'aethanyc-keybindings)

;; Local Variables:
;; coding: utf-8
;; End:

;;; aethanyc-keybindings.el ends here
