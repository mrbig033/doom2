;;; kbds.el -*- lexical-binding: t; -*-

;; (define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "<pause>") (kbd "C-c"))
(define-key key-translation-map (kbd "<menu>") (kbd "C-x"))

(map! :n "'"         'evil-goto-mark
      :n "`"         'evil-goto-mark-line
      :nvieg "C-."   'my-search-settings
      :nvieg "C-,"   'helpful-at-point
      :nvieg "C-c i" 'insert-char
      "C-h m"         'my-show-major-mode
      )