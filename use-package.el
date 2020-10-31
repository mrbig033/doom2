;;; use-package.el -*- lexical-binding: t; -*-

;;;;; WHICH KEY ;;;;;
(use-package! which-key
  :custom
  (which-key-idle-delay 0.4))
;;;;; EVIL ;;;;;
(use-package! evil
  :custom
  (evil-respect-visual-line-mode t))
;;;;; EVIL BETTER VISUAL LINE ;;;;;
(use-package! evil-better-visual-line
  :config
  (evil-better-visual-line-on))
;;;;; ORG ;;;;;
(use-package! org
  :init
  (remove-hook!
    'org-cycle-hook 'org-optimize-window-after-visibility-change
    'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  :custom
  (org-ellipsis ".")
  (org-directory "~/org/")
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "|" "DONE(d)")))
  :config
  (setq! org-capture-templates '(("t" "Todo"
                                  entry (file+headline +org-capture-todo-file "Inbox [/]")
                                  "* TODO %? %i" :prepend t)

                                 ("n" "Notes"
                                  entry (file+headline +org-capture-notes-file "Inbox")
                                  "* %u %? %i" :prepend t)

                                 ("j" "Journal"
                                  entry (file+olp+datetree +org-capture-journal-file)
                                  "* %u %? %i" :prepend t))))

;;;;; EVIL ORG ;;;;;
;;;;; https://bit.ly/3kE3Pcl ;;;;
;; (after! evil-org
;;   )
;;;;; RANGER ;;;;;
(use-package! ranger
  ;; :demand t
  :custom
  (ranger-deer-show-details nil))
;;;;; AVY ;;;;;
(use-package! avy
  :custom
  (avy-single-candidate-jump t))
;;;;; IVY ;;;;;
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
(remove-hook 'Info-mode 'olivetti)
;;;;; OLIVETTI ;;;;;
(use-package! olivetti
  :hook (Info-mode . olivetti-mode)
  :custom
  (olivetti-body-width 100))
;;;;; COOL MOVES ;;;;;
(use-package cool-moves
  :load-path "/home/jones/.doom.d/lisp/cool-moves")
;;;;; SUPER SAVE ;;;;;
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
;;;;; COMPANY ;;;;;
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
;;;;; EYEBROWSE ;;;;;
(use-package! eyebrowse
  :config
  (eyebrowse-mode +1))
;;;;; CLIPMON ;;;;;
(use-package! clipmon
  :config
  (clipmon-mode-start))
;;;;; INFO ;;;;;
(use-package! info
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  (remove-hook 'Info-mode-hook 'doom-modeline-set-info-modeline))
;;;;; LISPYVILLE ;;;;;
(use-package! lispyville
  :custom
  (lispyville-no-alter-lispy-options t)
  :config
  (defalias 'lispyville-yank 'evil-yank))
