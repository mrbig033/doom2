;;; kbds.el -*- lexical-binding: t; -*-

;; (define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "<pause>") (kbd "C-c"))
(define-key key-translation-map (kbd "<menu>") (kbd "C-x"))

(map! :n "'"         'evil-goto-mark
      :n "`"         'evil-goto-mark-line
      :n "g."        'evil-repeat
      :n "."         'ivy-switch-buffer
      :n "g4"         'evil-backward-word-end
      :i "M-/"       'hippie-expand
      :i "C-k"                            'kill-line
      :nvieg "C-."   'my-search-settings
      :nvieg "C-,"   'helpful-at-point
      :nvieg "C-c i" 'insert-char
      "C-h m"        'my-show-major-mode
      "M-p"          'backward-paragraph
      "M-n"          'forward-paragraph)
