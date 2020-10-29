;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "monospace" :size 27 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 28))

;; https://old.reddit.com/r/emacs/comments/5ldmjh/adding_space_between_text_and_the_top_of_the/
(setq header-line-format "  "
      auto-revert-verbose nil)

(setq writeroom-global-effects '(writeroom-set-alpha
                                 writeroom-set-menu-bar-lines
                                 writeroom-set-tool-bar-lines
                                 writeroom-set-vertical-scroll-bars
                                 writeroom-set-bottom-divider-width
                                 writeroom-set-internal-border-width))

(setq writeroom-border-width 200)

(setq company-global-modes '(not erc-mode message-mode help-mode gud-mode text-mode org-mode))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(mouse-avoidance-mode 'exile)
(global-auto-revert-mode t)

(setq! flycheck-global-modes '(not emacs-lisp-mode)
       eldoc-idle-delay 3
       pabbrev-idle-timer-verbose nil
       markdown-hide-urls t)

(add-hook! 'text-mode-hook
           #'electric-operator-mode
           #'evil-better-visual-line-on
           #'abbrev-mode)

(add-hook! 'org-mode-hook
           #'olivetti-mode
           #'pabbrev-mode)

(add-hook! 'org-mode-hook (hl-line-mode -1))

(remove-hook! 'org-mode-hook 'flyspell-mode)
(remove-hook! 'evil-visual-state-exit-hook 'doom-enable-hl-line-maybe-h)

(load-file "~/.doom.d/use-package.el")
(load-file "~/.doom.d/functions.el")
(load-file "~/.doom.d/kbds.el")
