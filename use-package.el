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
  (org-directory "~/org/")
  (org-capture-templates '(("t" "Todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %? %i %a" :prepend t)
                                ("n" "Note" entry (file+headline +org-capture-notes-file "Inbox") "* %u %? %i %a" :prepend t)
                                ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %? %i %a" :prepend t)
                                ("p" "Local Projects")
                                ("pt" "Todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %? %i %a" :prepend t)
                                ("pn" "Notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %? %i %a" :prepend t)
                                ("pc" "Changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %? %i %a" :prepend t)
                                ("o" "Centralized projects")
                                ("ot" "Todo" entry #'+org-capture-central-project-todo-file "* TODO %? %i %a" :heading "Tasks" :prepend nil)
                                ("on" "Notes" entry #'+org-capture-central-project-notes-file "* %U %? %i %a" :heading "Notes" :prepend t)
                                ("oc" "Changelog" entry #'+org-capture-central-project-changelog-file "* %U %? %i %a" :heading "Changelog" :prepend t))))

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
