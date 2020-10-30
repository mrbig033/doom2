;;; kbds/override-kbds.el -*- lexical-binding: t; -*-

(map! :map override
      :n "C-s"       'counsel-grep-or-swiper
      :i "C-u"       'my-backward-kill-line
      :n "gr"        'my-sel-to-end
      :n "ge"        'evil-end-of-visual-line
      :n "M-e"       'forward-sentence
      :n "M-a"       'backward-sentence
      :n "0"         'evil-beginning-of-visual-line
      :n "g0"        'evil-digit-argument-or-evil-beginning-of-line
      :n "Q"         'my-delete-frame
      :i "C-d"       'delete-char
      :i "C-h"       'delete-backward-char
      :i "C-n"       'next-line
      :i "C-p"       'previous-line
      :i "C-e"       'move-end-of-line
      :i "C-a"       'move-beginning-of-line
      :i "C-k"       'kill-line
      :ni "M-RET"    'my-indent-buffer
      :nvieg "<f8>"  'man
      :nvieg "C-S-j" 'cool-moves/line-forward
      :nvieg "C-S-k" 'cool-moves/line-backward
      :nvieg "M-y"   'counsel-yank-pop
      :nvieg "M-0"   'quit-window
      :nvieg "C-0"   'delete-other-windows
      :nvieg "M--"   'winner-undo
      :nvieg "M-="   'winner-redo
      :nvieg "M-k"   'windmove-up
      :nvieg "M-j"   'windmove-down
      :nvieg "M-h"   'windmove-left
      :nvieg "M-l"   'windmove-right
      "C-c SPC"      'caps-lock-mode
      "C-c 0"        'quit-window
      "C-c q"        'quick-calc
      "M-s"          'evil-switch-to-windows-last-buffer
      "M-w"          'eyebrowse-next-window-config
      "M-q"          'eyebrowse-prev-window-config
      "C-c a"        'align-regexp
      "<C-down>"     'cool-moves/paragraph-forward
      "<C-up>"       'cool-moves/paragraph-backward
      "C-S-j"        'cool-moves/line-forward
      "C-S-k"        'cool-moves/line-backward
      "C-M-n"        'cool-moves/word-forward
      "C-M-p"        'cool-moves/word-backwards)
