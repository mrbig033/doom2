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
(setq doom-font (font-spec :family "monospace" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 21))

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

(use-package! which-key
  :custom
  (which-key-idle-delay 0.6))

(use-package! avy
  :init
  (map! :n "f" 'avy-goto-word-1-below
        :n "F" 'avy-goto-word-1-above))

(use-package! olivetti
  :custom
  (olivetti-body-width 200)
  :config
  (olivetti-mode +1))

(map! :map (+doom-dashboard-mode-map)
      :e "q"         'quit-window)

(map! :n "<escape>"    'my-save-buffer
      :n "M-RET"       'my-indent-buffer
      :n "C-s"         'counsel-grep
      :i "C-u"         'my-backward-kill-line
      :n "gr"          'my-sel-to-end
      :n "ge"          'evil-end-of-visual-line
      :n "M-k"         'windmove-up
      :n "M-j"         'windmove-down
      :n "M-h"         'windmove-left
      :n "M-l"         'windmove-right
      "M-p"            'backward-paragraph
      "M-n"            'forward-paragraph
      "M-s"            'evil-switch-to-windows-last-buffer)

(map! :desc "Kill Buffer" :leader "k"   'kill-this-buffer
      :desc "Olivetti" :leader "to"   'olivetti-mode
      :desc "Goto Dashboard" :leader "gd"   '+doom-dashboard/open)

(setq flycheck-global-modes '(not emacs-lisp-mode))

(defun my-save-buffer ()
  (interactive)
  (evil-ex-nohighlight)
  (save-buffer))

(defun my-indent-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (evil-indent
     (point-min)
     (point-max))))

(defun my-backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun my-sel-to-end ()
  (interactive)
  (evil-visual-char)
  (evil-last-non-blank))

(defun xah-clean-empty-lines ()
  "replace repeated blank lines to just 1."
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))
