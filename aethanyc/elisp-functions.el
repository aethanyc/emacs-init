;;;-------------------------------------------------------------------
;;; Functions

(defun join-next-line ()
  "Join the next line with current line."
  (interactive)
  (join-line 1))

(defun first-available-font (font-list)
  "Return the first avaliable font in the font-list."
  (while (and font-list
              (not (member (car font-list) (font-family-list))))
    (setq font-list (cdr font-list)))
  (car font-list))

;; http://emacswiki.org/emacs/InteractivelyDoThings#toc16
(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))
