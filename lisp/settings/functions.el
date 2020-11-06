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

(defun my-show-info-commands ()
  (interactive)
  (counsel-M-x "^Info "))

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

;; Get focus even with focus stealing prevention
;; Source: https://bit.ly/37XClem
(defun my-new-frame-settings ()
  (select-frame-set-input-focus (selected-frame))
  (toggle-frame-maximized))

(defun my-sort-lines-by-length (reverse beg end)
  "sort lines by length."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; to make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))
        (reverse-region beg end)))))

(defun my-show-server-name ()
  (interactive)
  (helpful-variable 'server-name))

(defun my-show-major-mode ()
  (interactive)
  (helpful-variable 'major-mode))

;;;;; MODES ;;;;;
(define-derived-mode my-lisp-interaction-mode
  lisp-interaction-mode "my-lim")

(define-derived-mode scratch-mode
  text-mode "my-scratch")

(define-derived-mode my-markdown-mode
  markdown-mode "my-md")

(define-derived-mode my-fundamental-mode
  markdown-mode "my-fund")

(define-derived-mode my-text-mode
  markdown-mode "my-txt")

(define-derived-mode my-org-mode
  org-mode "my-org")

(defun my-bash-shebang ()
  (interactive)
  (erase-buffer)
  (insert "#!/usr/bin/env bash\n\n\n\n\n\n\n\n\n\n\n")
  (sh-mode)
  (sh-set-shell "bash")
  (xah-clean-empty-lines)
  (forward-to-indentation)
  (evil-insert-state)
  (my-just-save-buffer-quiet))

(defun my-python-shebang ()
  (interactive)
  (kill-region (point-min) (point-max))
  (insert "#!/usr/bin/env python3\n\n")
  ;; (insert "\"\"\" Docstring \"\"\"")
  ;; (insert "\n\n")
  (evil-insert-state))

(defun my-find-elisp-tmp ()
  (interactive)
  (find-file "~/doom-default/.doom.d/tmp/tmp.el"))

(defun my-find-elisp-tmp-other-window ()
  (interactive)
  (find-file-other-window "~/doom-default/.doom.d/tmp/tmp.el"))

(defun my-edit-hosts ()
  (interactive)
  (doom/sudo-find-file "/etc/hosts"))

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
      With prefix P, create local abbrev. Otherwise it will
      be global.
      If there's nothing wrong with the word at point, keep
      looking for a typo until the beginning of buffer. You can
      skip typos you don't want to fix with `SPC', and you can
      abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))
