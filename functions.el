;;; functions.el -*- lexical-binding: t; -*-

(defun my-comment-line ()
  (interactive)
  (save-excursion
    (comment-line 1)))

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

(defun my-show-package-commands ()
  (interactive)
  (counsel-M-x "^package-"))

(defun my-show-server-commands ()
  (interactive)
  (counsel-M-x "^server-"))

(defun my-force-normal-state ()
  (interactive)
  (evil-ex-nohighlight)
  (evil-force-normal-state))

;; (defun my-save-some-buffers ()
;;   (interactive)
;;   (let ((inhibit-message t))
;;     (evil-ex-nohighlight)
;;     (save-some-buffers t 0)))

;; (defun my-eval-buffer ()
;;   (interactive)
;;   (my-save-some-buffers)
;;   (+eval/buffer))

(defun my-eval-buffer ()
  (interactive)
  (my-save-buffer)
  (eval-buffer)
  (message " buffer evaluated"))

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

(defun my-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.
   Source: https://bit.ly/31X6KWk."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
