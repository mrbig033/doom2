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
  [remap evil-better-visual-line-next-line]
  :with 'backward-button
  [remap evil-better-visual-line-previous-line]
  :with 'quit-window
  [remap evil-record-macro]
  [remap evil-force-normal-state])
