;;; use-package.el -*- lexical-binding: t; -*-

(use-package! which-key
  :custom
  (which-key-idle-delay 0.5))

(use-package! evil
  :custom
  (evil-respect-visual-line-mode t))

(use-package! org
  :init
  (setq org-capture-templates '(("t" "Todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %? %i %a" :prepend t)
                                ("n" "Notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %? %i %a" :prepend t)
                                ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %? %i %a" :prepend t)))

  (map! :map (evil-org-mode-map org-mode-map)
        :i "C-l"         'pabbrev-expand-maybe
        :n "<backspace>" 'org-edit-special
        :n "zi"          'org-show-all
        :n "C-j"            'org-shiftleft
        :n "C-k"            'org-shiftright
        "C-k"            'org-shiftleft
        "C-c b"          'org-cycle-list-bullet
        "C-c C-s"        'org-emphasize)

  (remove-hook 'org-cycle-hook 'org-optimize-window-after-visibility-change)
  :custom
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-ellipsis ".")
  (org-directory "~/org/"))

;; https://github.com/hlissner/doom-emacs/issues/3159
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

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
  (olivetti-body-width 100))

(use-package cool-moves
  :load-path "/home/jones/.doom.d/lisp/cool-moves")

(use-package! super-save
  :custom
  (auto-save-default nil)
  (super-save-exclude '(".py"))
  (super-save-remote-files nil)
  (super-save-idle-duration 5)
  (super-save-auto-save-when-idle t)
  :config
  (add-to-list 'super-save-triggers 'evil-switch-to-windows-last-buffer 'delete-other-windows)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)

  (defun super-save-command ()
    (when (and buffer-file-name
               (buffer-modified-p (current-buffer))
               (file-writable-p buffer-file-name)
               (if (file-remote-p buffer-file-name) super-save-remote-files t)
               (super-save-include-p buffer-file-name))
      (my-just-save-buffer-quiet)))

  (super-save-mode +1))

(use-package! company
  :custom
  (company-show-numbers t)
  (company-idle-delay 0.2)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (company-dabbrev-other-buffers t)
  (company-selection-wrap-around t)
  (company-auto-commit nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-global-modes '(not erc-mode
                              text-mode
                              org-mode
                              markdown-mode
                              message-mode
                              help-mode
                              gud-mode
                              eshell-mode))

  :general
  (:keymaps '(company-active-map)
   ;; "<return>" nil
   ;; "TAB"      nil
   "C-h"    'backward-delete-char
   "M-q"    'company-complete-selection
   "C-d"    'counsel-company
   "M-y"    'my-company-yasnippet
   "M-p"    'my-company-comp-with-paren
   "M-."    'my-company-comp-with-dot
   "M-SPC"  'my-company-comp-space
   "C-u"    'my-backward-kill-line
   "M-0"    'company-complete-number
   "M-1"    'company-complete-number
   "M-2"    'company-complete-number
   "M-3"    'company-complete-number
   "M-4"    'company-complete-number
   "M-5"    'company-complete-number
   "M-6"    'company-complete-number
   "M-7"    'company-complete-number
   "M-8"    'company-complete-number
   "M-9"    'company-complete-number)

  :config

  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (yas-expand))

  (defun my-company-comp-with-paren ()
    (interactive)
    (company-complete-selection)
    (insert "()")
    (backward-char))

  (defun my-company-comp-with-dot ()
    (interactive)
    (company-complete-selection)
    (insert ".")
    (company-complete))

  (defun my-company-comp-space ()
    (interactive)
    (company-complete-selection)
    (insert " ")))

(use-package! eyebrowse
  :config
  (eyebrowse-mode +1))

(use-package! clipmon
  :config
  (clipmon-mode-start))
