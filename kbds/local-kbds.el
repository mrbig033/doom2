;;; local-kbds.el -*- lexical-binding: t; -*-

(map! :map (evil-org-mode-map org-mode-map)
        :i "C-l"         'pabbrev-expand-maybe
        :n "<backspace>" 'org-edit-special
        :n "zi"          'org-show-all
        :n "C-j"            'org-shiftleft
        :n "C-k"            'org-shiftright
        "C-k"            'org-shiftleft
        "C-c b"          'org-cycle-list-bullet
        "C-c C-s"        'org-emphasize)

(map! :map (my-org-mode-map my-lisp-interaction-mode-map)
      :n "<escape>" 'quit-window
      :n "q"        'quit-window)

(map! :map (flycheck-mode-map)
      :nvieg "C-c f"    'flycheck-first-error)

(map! :map (prog-mode-map)
      :n "<tab>" 'outline-toggle-children
      :n "C-c h" 'outline-hide-body
      :n "C-c s" 'outline-show-all)

(map! :map (prog-mode-map conf-mode-map)
      :nvieg "C-9" 'my-comment-line)

(map! :map (text-mode-map prog-mode-map conf-mode-map)
      :n "<escape>"    'my-save-buffer)

(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'my-force-normal-state)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "r" 'deer)

(map! :nv "f" 'avy-goto-char-2-below
      :nv "F" 'avy-goto-char-2-above)

(map! :map (ivy-minibuffer-map ivy-switch-buffer-map minibuffer-local-map read-expression-map)
      "C-,"      'ivy-previous-line
      "C-."      'ivy-next-line
      "C-k"      'kill-line
      "C-h"      'delete-backward-char)

(map! :map (+doom-dashboard-mode-map)
      :e "q"        'quit-window)

(map! :map (pabbrev-mode-map)
      :i "C-l" 'pabbrev-expand-maybe)
