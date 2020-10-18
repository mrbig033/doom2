;;; use-package.el -*- lexical-binding: t; -*-


(use-package! which-key
  :custom
  (which-key-idle-delay 0.6))

(use-package! evil
  :custom
  (evil-respect-visual-line-mode t))

(use-package! org
  :custom
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-ellipsis ".")
  (org-directory "~/org/"))

(use-package! ranger
  ;; :demand t
  :custom
  (ranger-deer-show-details nil))

(use-package! avy
  :custom
  (avy-single-candidate-jump t))

(use-package! ivy
  :custom
  (ivy-height 15)
  (ivy-extra-directories nil)
  (counsel-outline-display-style 'title)
  (counsel-find-file-at-point t)
  (counsel-bookmark-avoid-dired t)
  (counsel-grep-swiper-limit 10000)
  (ivy-ignore-buffers '("^#.*#$"
                        "^\\*.*\\*")))

(use-package! olivetti
  :custom
  (olivetti-body-width 120)
  :config
  (olivetti-mode +1))

(use-package cool-moves
  :load-path "/home/jones/.doom.d/lisp/cool-moves")
