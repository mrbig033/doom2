;;; kbds.el -*- lexical-binding: t; -*-

(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'quit-window
      "<escape>" 'quit-window)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "r" 'deer)

(map! :nv "f" 'avy-goto-char-2-below
      :nv "F" 'avy-goto-char-2-above)

(map! :map (ivy-minibuffer-map ivy-switch-buffer-map)
      "C-."      'ivy-next-line
      "C-k"      'kill-line
      "C-h"      'delete-backward-char)

(general-define-key
 :keymaps 'override
 "<C-down>" 'cool-moves/paragraph-forward
 "<C-up>" 'cool-moves/paragraph-backward
 "C-S-j" 'cool-moves/line-forward
 "C-S-k" 'cool-moves/line-backward
 "C-M-n" 'cool-moves/word-forward
 "C-M-p" 'cool-moves/word-backwards)

(map! :map (+doom-dashboard-mode-map)
      :e "q"         'quit-window)

(map! "M-p"            'backward-paragraph
      "M-n"            'forward-paragraph)

(map! :map (evil-org-mode-map org-mode-map)
      :n "<backspace>" 'org-edit-special
      :n "zi" 'org-show-all
      "C-c C-s" 'org-emphasize)

(map! :map override
      :n "M-RET"       'my-indent-buffer
      :n "C-s"         'counsel-grep-or-swiper
      :i "C-u"         'my-backward-kill-line
      :n "gr"          'my-sel-to-end
      :n "ge"          'evil-end-of-visual-line
      :n "M-e"         'forward-sentence
      :n "M-a"         'backward-sentence
      :n "0"           'evil-beginning-of-visual-line
      :n "g0"          'evil-digit-argument-or-evil-beginning-of-line
      :n "Q"         'my-delete-frame
      :i "C-d"         'delete-char
      :i "C-h"         'delete-backward-char
      :i "C-n"         'next-line
      :i "C-p"         'previous-line
      :i "C-e"         'move-end-of-line
      :i "C-a"         'move-beginning-of-line
      :i "C-k"         'kill-line
      :nvieg "M--"         'winner-undo
      :nvieg "M-="         'winner-redo
      :nvieg "C-,"         'helpful-at-point
      :nvieg "M-k"         'windmove-up
      :nvieg "M-j"         'windmove-down
      :nvieg "M-h"         'windmove-left
      :nvieg "M-l"         'windmove-right
      "C-c SPC"        'caps-lock-mode
      "C-c 0"          'quit-window
      "C-c q"          'quick-calc
      "M-s"            'evil-switch-to-windows-last-buffer)

(map! :nvieg "C-."         'doom/find-file-in-private-config
      :n "<escape>"    'my-save-buffer )

(map! :desc "Kill Buffer" :leader "k"   'kill-this-buffer
      :desc "Olivetti" :leader "to"   'olivetti-mode
      :desc "Change Dictionary" :leader "td"   'ispell-change-dictionary
      :desc "Quit Window" :leader "0"   'quit-window
      :desc "Make Frame" :leader "Q"   'make-frame
      :desc "Flyspell Buffer" :leader "tb"   'flyspell-buffer
      :desc "Flyspell Previous" :leader "tp"   'flyspell-correct-previous
      :desc "Goto Dashboard" :leader "gd"   '+doom-dashboard/open)
