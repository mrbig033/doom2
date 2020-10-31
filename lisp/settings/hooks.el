;;;; REMOVE HOOKS ;;;;;
(remove-hook! '(org-mode-hook
                text-mode-hook
                prog-mode-hook) hl-line-mode)

(remove-hook! 'evil-visual-state-exit-hook 'doom-enable-hl-line-maybe-h)
;;;; ADD HOOKS ;;;;;
(add-hook! '(prog-mode-hook
             text-mode-hook
             org-mode-hook
             helpful-mode-hook
             conf-mode-hook) #'olivetti-mode)
(add-hook! 'text-mode-hook
           #'electric-operator-mode
           #'abbrev-mode)
(add-hook! 'server-after-make-frame-hook #'my-new-frame-settings)
;;;; MAKE SCRIPTS EXECUTABLE ;;;;;
;; source: https://bit.ly/31ZDduV
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)
