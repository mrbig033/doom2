;;; functions.el -*- lexical-binding: t; -*-

(defun my-search-settings ()
  (interactive)
  (counsel-ag nil "~/.doom.d/" "-f -G '.el'"))

(defun my-delete-frame ()
  (interactive)
  (delete-frame))

(defun my-save-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (evil-ex-nohighlight)
    (save-buffer)))

(defun my-just-save-buffer-quiet ()
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)))

(defun my-indent-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (evil-indent
     (point-min)
     (point-max))))

(defun my-backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun my-sel-to-end ()
  (interactive)
  (evil-visual-char)
  (evil-last-non-blank))

(defun my-yank-dirname-as-kill ()
  "Source: https://stackoverflow.com/a/53075288
   Copy the current directory into the kill ring."
  (interactive)
  (kill-new default-directory)
  (message default-directory))

(defun xah-clean-empty-lines ()
  "replace repeated blank lines to just 1."
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))
