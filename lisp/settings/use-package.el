;;; use-package.el -*- lexical-binding: t; -*-

;;;;; WHICH KEY ;;;;;
(use-package! which-key
  :custom
  (which-key-idle-delay 0.3))
;;;;; EVIL ;;;;;
(use-package! evil
  :custom
  (evil-respect-visual-line-mode t))
;;;;; EVIL BETTER VISUAL LINE ;;;;;
(use-package! evil-better-visual-line
  :config
  (evil-better-visual-line-on))
;;;;; ORG ;;;;;

;; (use-package org-plus-contrib)

(use-package! org
  :init
  (add-hook 'org-mode-hook #'pabbrev-mode)
  (remove-hook 'org-cycle-hook 'org-optimize-window-after-visibility-change)
  (remove-hook 'org-mode-hook 'flyspell-mode)
  :custom
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-drawers (quote ("properties" "logbook")))   ;; Separate drawers for clocking and logs
  (org-archive-location ".%s::")
  (org-ellipsis ".")
  (org-directory "~/org/")
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "|" "DONE(d)")))
  :config
  (load-file "~/.doom.d/lisp/settings/org-capture-templates.el")
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;;;;; EVIL ORG ;;;;;
;;;;; https://bit.ly/3kE3Pcl ;;;;
(use-package! evil-org
  :config
  (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h))
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
  (super-save-idle-duration 10)
  (super-save-auto-save-when-idle t)
  :config

  (setq super-save-triggers '(switch-to-buffer
                              other-window
                              windmove-up
                              windmove-down
                              windmove-left
                              windmove-right
                              counsel-M-x
                              next-buffer
                              +eval/buffer
                              previous-buffer))

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
  (remove-hook 'Info-mode-hook 'doom-modeline-set-info-modeline))
;;;;; LISPYVILLE ;;;;;
(use-package! lispyville
  :config
  (defalias 'lispyville-yank 'evil-yank))
