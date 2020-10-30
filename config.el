;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;; SETTINGS ;;;;;
(setq! doom-theme 'doom-one
       markdown-hide-urls t
       eldoc-idle-delay 100
       windmove-wrap-around t
       confirm-kill-emacs nil
       auto-revert-verbose nil
       user-full-name "Mr Big"
       doom-localleader-key "m"
       display-line-numbers-type nil
       pabbrev-idle-timer-verbose nil
       user-mail-address "mrbig033@protonmail.com"
       flycheck-global-modes '(not emacs-lisp-mode)
       header-line-format "  " ;; source: https://bit.ly/2TDNkkH
       doom-scratch-initial-major-mode 'my-lisp-interaction-mode
       doom-variable-pitch-font (font-spec :family "sans" :size 28)
       bitly-access-token "3026d7e8b1a0f89da10740c69fd77b4b3293151e"
       doom-font (font-spec :family "monospace" :size 27 :weight 'semi-light))
;;;;; HOOKS ;;;;;
(remove-hook! 'org-mode-hook 'flyspell-mode)
(remove-hook! '(org-mode-hook text-mode-hook prog-mode-hook) hl-line-mode)
(remove-hook! 'evil-visual-state-exit-hook 'doom-enable-hl-line-maybe-h)
(add-hook! '(prog-mode-hook
             text-mode-hook
             org-mode-hook
             helpful-mode-hook
             conf-mode-hook) #'olivetti-mode)
(add-hook! 'text-mode-hook
           #'electric-operator-mode
           #'abbrev-mode)
(add-hook! 'org-mode-hook #'olivetti-mode #'pabbrev-mode)
(add-hook! 'server-after-make-frame-hook #'my-new-frame-settings)
;;;;; MODES ;;;;;
(define-derived-mode my-lisp-interaction-mode
  lisp-interaction-mode "lim")

(define-derived-mode scratch-mode
  text-mode "scratch")

(define-derived-mode my-org-mode
  org-mode "my-org")

(mouse-avoidance-mode 'exile)
(global-auto-revert-mode t)
;;;;; LOAD FILES ;;;;;
(load-file "~/.doom.d/use-package.el")
(load-file "~/.doom.d/functions.el")
(mapc 'load (file-expand-wildcards "~/.doom.d/kbds/*.el"))
