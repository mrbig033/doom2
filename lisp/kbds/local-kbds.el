;;; local-kbds.el -*- lexical-binding: t; -*-

;;;;; ORG ;;;;;
(map! :map (evil-org-mode-map org-mode-map)
      :i "C-l"         'pabbrev-expand-maybe
      :n "<backspace>" 'org-edit-special
      :n "zi"          'org-show-all
      :n "C-j"         'org-shiftleft
      :n "C-k"         'org-shiftright
      :i "C-k"         'kill-line
      :desc "Goto Clock"      :localleader "cs" 'org-clock-display
      ;; :desc "Display Clocked" :localleader "cg" 'org-clock-goto
      "C-M-k"          'org-metaup
      "C-M-j"          'org-metadown
      "C-k"            'org-shiftleft
      "C-c b"          'org-cycle-list-bullet
      "C-c C-s"        'org-emphasize)

(map! :map (my-org-mode-map
            my-lisp-interaction-mode-map
            my-markdown-mode
            my-fundamental-mode
            my-text-mode
            my-org-mode)
      :n "<escape>" 'my-force-normal-state
      :n "q"        'quit-window)
;;;;; PROG AND TEXT;;;;;
(map! :map (prog-mode-map)
      :n "<tab>" 'outline-toggle-children
      :ni "C-c h" 'outline-hide-body
      :ni "C-c s" 'outline-show-all
      :ni "C-c o" 'outline-hide-other)

(map! :map (prog-mode-map conf-mode-map)
      :nvieg "C-9" 'my-comment-line)

(map! ;; :after (emacs-lisp-mode-map lisp-mode-map)
      :map (emacs-lisp-mode-map lisp-mode-map)
      :n "<return>" 'eros-eval-last-sexp
      :i "C-k"      'lispy-kill
      :localleader "0" 'evil-next-close-paren
      :localleader "9" 'evil-previous-open-paren)

(map! :map (flycheck-mode-map)
      :nvieg "C-c f"    'flycheck-first-error)

(map! :map (text-mode-map
            prog-mode-map
            conf-mode-map)
      :n "<escape>"    'my-save-buffer)

(map! :map (pabbrev-mode-map)
      :i "C-9" 'pabbrev-expand-maybe)
;;;;; MISC ;;;;;
(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'my-force-normal-state)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "r" 'deer)

(map! :map (ivy-minibuffer-map
            ivy-switch-buffer-map
            minibuffer-local-map
            read-expression-map)
      "C-,"      'ivy-previous-line
      "C-."      'ivy-next-line
      "C-k"      'kill-line
      "C-h"      'delete-backward-char)

(map! :map (Info-mode-map)
      :n "<escape>" 'my-force-normal-state
      :n "m"          'Info-menu
      :n "L"          'Info-history-forward
      ;; :n "<return>"          'Info-follow-nearest-node
      ;; :n "RET"          'Info-follow-nearest-node
      :n "C-n"          'Info-next
      :n "C-p"          'Info-prev
      :n "H"          'Info-history-back
      :n "ci"         'clone-indirect-buffer-other-window
      :n "<C-return>" 'eros-eval-last-sexp
      :n "M-n"        'forward-paragraph)

(defun my-evaluate-next-sexp ()
  (interactive)
  (lispy-forward 1)
  (eros-eval-last-sexp nil))
