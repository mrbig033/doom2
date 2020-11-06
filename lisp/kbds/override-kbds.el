;;; kbds/override-kbds.el -*- lexical-binding: t; -*-

(map! :map override
      :nv "f" 'avy-goto-char-2-below
      :nv "F" 'avy-goto-char-2-above
      :n "C-s"                            'counsel-grep-or-swiper
      :i "C-u"                            'my-backward-kill-line
      :n "gr"                             'my-sel-to-end
      :n "ge"                             'evil-end-of-visual-line
      :n "M-e"   'evil-forward-sentence-begin
      :n "M-a"   'evil-backward-sentence-begin
      :n "0"                              'evil-beginning-of-visual-line
      :n "g0"                             'evil-digit-argument-or-evil-beginning-of-line
      :n "!"                              'my-delete-frame
      :n "Q"                              'my-delete-frame
      :i "C-d"                            'delete-char
      :i "C-h"                            'delete-backward-char
      :i "C-n"                            'next-line
      :i "C-p"                            'previous-line
      :i "C-e"                            'move-end-of-line
      :i "C-a"                            'move-beginning-of-line
      :ni "<M-return>"                    'my-indent-buffer
      :nvieg "<f8>"                       'man
      :nvieg "C-S-j"                      'cool-moves/line-forward
      :nvieg "C-S-k"                      'cool-moves/line-backward
      :nvieg "M-y"                        'counsel-yank-pop
      :nvieg "M-9"                        'delete-window
      :nvieg "M-0"                        'quit-window
      :nvieg "C-0"                        'delete-other-windows
      :nvieg "M--"                        'winner-undo
      :nvieg "M-="                        'winner-redo
      :nvieg "M-k"                        'windmove-up
      :nvieg "M-j"                        'windmove-down
      :nvieg "M-h"                        'windmove-left
      :nvieg "M-l"                        'windmove-right
      :desc "Capture"             :n "çç" 'org-capture
      :desc "Capture Goto Last"   :n "çl" 'org-capture-goto-last-stored
      :desc "Capture Goto Target" :n "çt" 'org-capture-goto-target
      "C-c SPC"                           'caps-lock-mode
      "C-c q"                             'quick-calc
      "M-s"                               'evil-switch-to-windows-last-buffer
      "M-w"                               'eyebrowse-next-window-config
      "M-q"                               'eyebrowse-prev-window-config
      "C-c a"                             'align-regexp
      "C-/"                               'org-cycle-agenda-files
      :nvieg "M-," 'projectile-next-project-buffer
      :nvieg "M-." 'projectile-previous-project-buffer
      "<C-down>"                          'cool-moves/paragraph-forward
      "<C-up>"                            'cool-moves/paragraph-backward
      "C-S-j"                             'cool-moves/line-forward
      "C-S-k"                             'cool-moves/line-backward
      "C-S-n"                             'cool-moves/word-forward
      "C-S-p"                             'cool-moves/word-backwards)
