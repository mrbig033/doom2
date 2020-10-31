;;; local-kbds.el -*- lexical-binding: t; -*-

;;;;; ORG ;;;;;
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
;;;;; PROG AND TEXT;;;;;
(map! :map (prog-mode-map)
      :n "<tab>" 'outline-toggle-children
      :ni "C-c h" 'outline-hide-body
      :ni "C-c s" 'outline-show-all
      :ni "C-c o" 'outline-hide-other)

(map! :map (prog-mode-map conf-mode-map)
      :nvieg "C-9" 'my-comment-line)

(map! :map (flycheck-mode-map)
      :nvieg "C-c f"    'flycheck-first-error)

(map! :map (text-mode-map
            prog-mode-map
            conf-mode-map)
      :n "<escape>"    'my-save-buffer)

(map! :map (pabbrev-mode-map)
      :i "C-l" 'pabbrev-expand-maybe)
;;;;; MISC ;;;;;
(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'my-force-normal-state)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "r" 'deer)

(map! :nv "f" 'avy-goto-char-2-below
      :nv "F" 'avy-goto-char-2-above)

(map! :map (ivy-minibuffer-map
            ivy-switch-buffer-map
            minibuffer-local-map
            read-expression-map)
      "C-,"      'ivy-previous-line
      "C-."      'ivy-next-line
      "C-k"      'kill-line
      "C-h"      'delete-backward-char)

(map! :map (Info-mode-map)
      :n "m"        'Info-menu
      :n "L" 'Info-history-forward
      :n "H" 'Info-history-back
      :n "ci" 'clone-indirect-buffer-other-window
      :n "<C-return>" 'my-evaluate-next-sexp
      :n "M-n" 'my-evaluate-next-sexp)

(defun my-evaluate-next-sexp ()
  (interactive)
  (lispy-forward 1)
  (eros-eval-last-sexp nil))
