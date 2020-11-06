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
       flycheck-global-modes '(not emacs-lisp-mode lisp-interaction-mode)
       header-line-format "  " ;; source: https://bit.ly/2TDNkkH
       ;; doom-scratch-initial-major-mode 'my-lisp-interaction-mode
       doom-variable-pitch-font (font-spec :family "sans" :size 28)
       bitly-access-token "3026d7e8b1a0f89da10740c69fd77b4b3293151e"
       doom-font (font-spec :family "monospace" :size 27 :weight 'semi-light))

(put 'customize-group 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(mouse-avoidance-mode 'banish)
(global-auto-revert-mode t)
;; (global-pabbrev-mode t)

;;;;; LOAD FILES ;;;;;
(mapc 'load (file-expand-wildcards "~/.doom.d/lisp/settings/*.el"))
(mapc 'load (file-expand-wildcards "~/.doom.d/lisp/kbds/*.el"))
