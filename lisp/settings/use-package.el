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
  (org-ellipsis ".")
  (org-clock-persist t)
  (org-log-into-drawer t)
  (org-clock-in-resume t)
  (org-directory "~/org/")
  (org-clock-into-drawer t)
  (org-clock-update-period 240)
  (org-clock-history-length 10)
  (org-archive-location ".%s::")
  (org-clock-mode-line-total 'auto)
  (org-clock-persist-query-resume t)
  (org-clock-clocked-in-display nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-clock-report-include-clocking-task t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "|" "DONE(d)")))
  (org-drawers (quote ("properties" "logbook"))) ;; Separate drawers for clocking and logs
  :config
  (load-file "~/.doom.d/lisp/settings/org-capture-templates.el")
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;;;;; ORG POMODORO ;;;;
(use-package! org-pomodoro
  :after org
  :custom
  (org-pomodoro-offset 1)
  (org-pomodoro-audio-player "/usr/bin/paplay --volume=32768")
  (org-pomodoro-start-sound-args t)
  (org-pomodoro-length (* 8 org-pomodoro-offset))
  (org-pomodoro-short-break-length (/ org-pomodoro-length 5))
  (org-pomodoro-long-break-length (* org-pomodoro-length 0.8))
  (org-pomodoro-long-break-frequency 4)
  (org-pomodoro-ask-upon-killing nil)
  (org-pomodoro-manual-break t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  ;; (org-pomodoro-time-format "%.2m")
  (org-pomodoro-time-format "%.2m:%.2s")
  (org-pomodoro-short-break-format "SHORT: %s")
  (org-pomodoro-long-break-format "LONG: %s")
  (org-pomodoro-format "P: %s"))
;;;;; EVIL ORG ;;;;;
;;;;; https://bit.ly/3kE3Pcl ;;;;
(use-package! evil-org
  :config
  (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)
  (add-hook 'org-cycle-hook 'org-cycle-hide-drawers))
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
  (counsel-grep-swiper-limit 5000)
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
                              previous-buffer
                              eyebrowse-next-window-config
                              eyebrowse-last-window-config
                              eyebrowse-create-window-config
                              eyebrowse-close-window-config))

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
  :custom
  (eyebrowse-wrap-around t)
  (eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode +1))
;;;;; CLIPMON ;;;;;
(use-package! clipmon
  :config
  (clipmon-mode-start))
;;;;; INFO ;;;;;
(use-package! info
  :custom
  (info-lookup-other-window-flag nil)
  :init
  (remove-hook 'Info-mode-hook 'doom-modeline-set-info-modeline)
  :config
  (add-to-list 'display-buffer-alist
               '("*info*" display-buffer-same-window)))
;;;;; LISPYVILLE ;;;;;
(use-package! lispyville
  :config
  (defalias 'lispyville-yank 'evil-yank))
;;;; HL-SENTENCE ;;;;
(use-package! hl-sentence
  :custom-face
  (hl-sentence ((t (:inherit hl-line)))))
;;;;; ZOOM ;;;;;
(use-package! zoom
  :custom
  ;; default:
  ;; (zoom-size '(80 . 24))

  ;; golden ration:
  (zoom-size '(0.618 . 0.618))

  ;; custom ratio
  ;; (zoom-size '(0.5 . 0.5))
  )
