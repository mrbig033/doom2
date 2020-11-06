;;; unbind-kbds.el -*- lexical-binding: t; -*-

(general-unbind '(scratch-mode-map my-org-mode-map)
  :with 'my-force-normal-state
  [remap my-save-buffer]
  [remap save-buffer])

(general-unbind 'normal lisp-interaction-mode-map
  :with 'ignore
  [remap my-save-buffer])

(general-unbind +doom-dashboard-mode-map
  :with 'forward-button
  [remap evil-better-visual-line-next-line])

(general-unbind +doom-dashboard-mode-map
  :with 'backward-button
  [remap evil-better-visual-line-previous-line])

(general-unbind +doom-dashboard-mode-map
  :with 'quit-window
  [remap evil-record-macro]
  [remap evil-force-normal-state])

(general-unbind +doom-dashboard-mode-map
  :with 'push-button
  [remap evil-forward-char])

(general-unbind 'lispyville-mode-map
  :with 'evil-switch-to-windows-last-buffer
  [remap lispy-splice])

(general-unbind 'org-capture-mode-map
  :with 'org-capture-finalize
  [remap my-indent-buffer])

(general-unbind 'org-src-mode-map
  :with 'org-edit-src-exit
  [remap lispy-mark-symbol])
