;;;;; SETTINGS ;;;;;
;;;;;

(setq! doom-theme 'doom-one
       dracula-enlarge-headings nil
       markdown-hide-urls t
       eldoc-idle-delay 100
       windmove-wrap-around t
       confirm-kill-emacs nil
       auto-revert-verbose nil
       user-full-name "mr big"
       doom-localleader-key "m"
       text-scale-mode-step 1.06
       display-line-numbers-type nil
       pabbrev-idle-timer-verbose nil
       user-mail-address "mrbig033@protonmail.com"
       bookmark-default-file "~/.doom.d/lisp/bookmarks"
       flycheck-global-modes '(not emacs-lisp-mode lisp-interaction-mode)
       header-line-format "  " ;; source: https://bit.ly/2tdnkkh
       ;; doom-scratch-initial-major-mode 'my-lisp-interaction-mode
       doom-variable-pitch-font (font-spec :family "sans" :size 28)
       bitly-access-token "3026d7e8b1a0f89da10740c69fd77b4b3293151e"
       doom-font (font-spec :family "monospace" :size 27 :weight 'semi-light))

(add-to-list 'display-buffer-alist
             '("*info*" display-buffer-same-window))

(put 'customize-group 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(load-file "~/.doom.d/lisp/cool-moves/cool-moves.el")
(mouse-avoidance-mode 'banish)
(global-auto-revert-mode t)
;; (global-pabbrev-mode t)

;;;; REMOVE HOOKS ;;;;;
(remove-hook! '(org-mode-hook
                text-mode-hook
                ;; prog-mode-hook
                ) hl-line-mode)

(remove-hook! 'evil-visual-state-exit-hook 'doom-enable-hl-line-maybe-h)
;;;; ADD HOOKS ;;;;;
(add-hook! '(prog-mode-hook
             text-mode-hook
             org-mode-hook
             helpful-mode-hook
             conf-mode-hook) #'olivetti-mode)
(add-hook! 'text-mode-hook
           #'electric-operator-mode
           #'abbrev-mode)

(add-hook! '(server-after-make-frame-hook
             ;; doom-first-buffer-hook
             ) #'my-new-frame-settings)


;;;; MAKE SCRIPTS EXECUTABLE ;;;;;
;; source: https://bit.ly/31ZDduV
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun my-deer-goto-tmp ()
  (interactive)
  (deer "~/.doom.d/tmp/"))

(defun my-comment-line ()
  (interactive)
  (save-excursion
    (comment-line 1)))

(defun my-indent-buffer-save-excursion ()
  (interactive)
  (save-excursion
    (let ((inhibit-message t))
      (evil-indent
       (point-min)
       (point-max)))))

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

(defun my-sort-lines-by-length (reverse beg end)
  "sort lines by length."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; to make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))
        (reverse-region beg end)))))

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
      With prefix P, create local abbrev. Otherwise it will
      be global.
      If there's nothing wrong with the word at point, keep
      looking for a typo until the beginning of buffer. You can
      skip typos you don't want to fix with `SPC', and you can
      abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(defun my-search-settings ()
  (interactive)
  (counsel-ag nil "~/.doom.d/" "-f -G 'config.org\|init.el\|packages.el'"))

;; org src buffer name
(defun my-org-edit-special ()
  (interactive)
  (org-edit-special)
  (my-recenter-window))

(defun my-evaluate-next-sexp ()
  (interactive)
  (lispy-forward 1)
  (eros-eval-last-sexp nil))

(defun my-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "~/.doom.d/tmp/sct.el*"))


(defun my-delete-frame ()
  (interactive)
  (delete-frame))

(defun my-save-buffer ()
  (interactive)
  (let ((inhibit-message t))
    (evil-ex-nohighlight)
    (save-buffer)))

(defun my-just-save-buffer-quiet ()
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)))
(defun my-show-package-commands ()
  (interactive)
  (counsel-M-x "^package-"))

(defun my-show-info-commands ()
  (interactive)
  (counsel-M-x "^Info "))

(defun my-show-server-commands ()
  (interactive)
  (counsel-M-x "^server-"))

(defun my-force-normal-state ()
  (interactive)
  (evil-ex-nohighlight)
  (evil-force-normal-state))

(defun my-eval-buffer ()
  (interactive)
  (my-save-buffer)
  (eval-buffer)
  (message " buffer evaluated"))

(defun my-sel-to-end ()
  (interactive)
  (evil-visual-char)
  (evil-last-non-blank))

(defun my-yank-dirname-as-kill ()
  "Source: https://stackoverflow.com/a/53075288
   Copy the current directory into the kill ring."
  (interactive)
  (kill-new default-directory)
  (message default-directory))

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

(defun my-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.
   Source: https://bit.ly/31X6KWk."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Get focus even with focus stealing prevention
;; Source: https://bit.ly/37XClem
(defun my-new-frame-settings ()
  (select-frame-set-input-focus (selected-frame))
  (toggle-frame-maximized))

(defun my-show-server-name ()
  (interactive)
  (helpful-variable 'server-name))

(defun my-show-major-mode ()
  (interactive)
  (helpful-variable 'major-mode))

;;;;; MODES ;;;;;
(define-derived-mode my-lisp-interaction-mode
  lisp-interaction-mode "my-lim")

(define-derived-mode scratch-mode
  text-mode "my-scratch")

(define-derived-mode my-markdown-mode
  markdown-mode "my-md")

(define-derived-mode my-fundamental-mode
  markdown-mode "my-fund")

(define-derived-mode my-text-mode
  markdown-mode "my-txt")

(define-derived-mode my-org-mode
  org-mode "my-org")

(defun my-bash-shebang ()
  (interactive)
  (erase-buffer)
  (insert "#!/usr/bin/env bash\n\n\n\n\n\n\n\n\n\n\n")
  (sh-mode)
  (sh-set-shell "bash")
  (xah-clean-empty-lines)
  (forward-to-indentation)
  (evil-insert-state)
  (my-just-save-buffer-quiet))

(defun my-python-shebang ()
  (interactive)
  (kill-region (point-min) (point-max))
  (insert "#!/usr/bin/env python3\n\n")
  ;; (insert "\"\"\" Docstring \"\"\"")
  ;; (insert "\n\n")
  (evil-insert-state))

(defun my-find-elisp-tmp ()
  (interactive)
  (find-file "~/.doom.d/tmp/tmp.el"))

(defun my-find-elisp-tmp-other-window ()
  (interactive)
  (find-file-other-window "~/.doom.d/tmp/tmp.el"))

(defun my-edit-hosts ()
  (interactive)
  (doom/sudo-find-file "/etc/hosts"))

(fset 'my-org-capture-todo-macro
      (kmacro-lambda-form [?\M-x ?c ?o ?u ?n ?s return ?T ?o ?d ?o return escape ?\M-k ?\M-j ?i ? ] 0 "%d"))

(defun my-tangle-py-config ()
  (interactive)
  (start-process-shell-command "tangle config.org"
                               nil
                               "~/dotfiles/maps/scripts/emacs-tangle-init")
  (message " init tangled"))

(defun my-tangle-py-config-quiet ()
  (interactive)
  (start-process-shell-command "tangle config.org"
                               nil
                               "~/dotfiles/maps/scripts/emacs-tangle-init"))
(defun my-recenter-window ()
  (interactive)
  (recenter-top-bottom
   `(4)))

(map! :desc "Yank Dirname"              :leader "fY"  'my-yank-dirname-as-kill
      :desc "My Rename"                 :leader "fR"  'my-rename-file-and-buffer
      :desc "Trash File"                :leader "fD"  'move-file-to-trash
      :desc "Goto Scratch"              :leader "fs"  'my-switch-to-scratch
      :desc "Find Config"               :leader "fc"  'my-find-config
      :desc "Edit Hosts"                :leader "fh"  'my-edit-hosts
      :desc "Goto Elisp"                :leader "fe"  'my-find-elisp-tmp
      :desc "Goto Elisp Other Window"   :leader "fE"  'my-find-elisp-tmp-other-window
      :desc "Tangle Config"             :leader "ft"  'my-tangle-py-config

      :desc "Lispy Interaction Mode"    :leader "ml"  'lisp-interaction-mode
      :desc "My Lispy Interaction Mode" :leader "mL"  'my-lisp-interaction-mode
      :desc "Markdown Mode"             :leader "mm"  'markdown-mode
      :desc "My Markdown Mode"          :leader "mM"  'my-markdown-mode
      :desc "Fundamental Mode"          :leader "mf"  'fundamental-mode
      :desc "My Fundamental Mode"       :leader "mF"  'my-fundamental-mode
      :desc "Text Mode"                 :leader "mt"  'text-mode
      :desc "Text Mode"                 :leader "mT"  'my-text-mode
      :desc "Typo Mode"                 :leader "my"  'typo-mode
      :desc "Org Mode"                  :leader "mo"  'org-mode
      :desc "My Org Mode"               :leader "mO"  'my-org-mode
      :desc "My Eval Buffer"            :leader "meb" '+eval/buffer

      :desc "Disable Theme"             :leader "hT"  'disable-theme
      :desc "Describe Keymaps"          :leader "hbb"  'describe-bindings
      :desc "Show Keymaps"              :leader "hbk"  'which-key-show-keymap
      :desc "Show Top Keymaps"          :leader "hbt"  'which-key-show-top-level
      :desc "Show Major Keymaps"        :leader "hbm"  'which-key-show-major-mode
      :desc "Describe Package"          :leader "hdpP"  'describe-package
      :desc "Show Full Keymaps"         :leader "hbf"  'which-key-show-full-keymap
      :desc "Show Minor Keymaps"        :leader "hbi"  'which-key-show-minor-mode-keymap

      :desc "Flyspell Mode"             :leader "tS"  'flyspell-mode
      :desc "Flyspell Buffer"           :leader "tb"  'flyspell-buffer
      :desc "Olivetti"                  :leader "to"  'olivetti-mode
      :desc "Xah Clean Empty Lines"     :leader "tD"  'xah-clean-empty-lines
      :desc "Visible Mode"              :leader "tv"  'visible-mode
      :desc "Change Dictionary"         :leader "td"  'ispell-change-dictionary
      :desc "Highlight Line"            :leader "th"  'hl-line-mode
      :desc "Hide Mode Line"            :leader "tH"  'hide-mode-line-mode
      :desc "Highlight Sentence"        :leader "ts"  'hl-sentence-mode

      :desc "New Snippet"               :leader "yn"  '+snippets/new
      :desc "Edit Snippet"              :leader "ye"  '+snippets/edit
      :desc "Find Snippet"              :leader "yf"  '+snippets/find
      :desc "Reload All"                :leader "yr"  'yas-reload-all
      :desc "Insert Snippet"            :leader "yi"  'yas-insert-snippet

      :desc "My Package Commands"       :leader "scp" 'my-show-package-commands
      :desc "My Server Commands"        :leader "scs" 'my-show-server-commands
      :desc "My Info Commands"          :leader "sci" 'my-show-info-commands

      :desc "Bash Shebang"              :leader "ib"   'my-bash-shebang
      :desc "Python Shebang"            :leader "ip"   'my-python-shebang

      :desc "Delete Window"             :leader "0"  'delete-window

      :desc "Open Scratch"              :leader "x"   'doom/open-scratch-buffer
      :desc "Switch to Scratch"         :leader "X"   'doom/switch-to-scratch-buffer

      :desc "Eyebrowse New"             :leader "v"   'eyebrowse-create-window-config
      :desc "Eyebrowse Close"           :leader "V"   'eyebrowse-close-window-config

      :desc "Kill Buffer"               :leader "k"   'kill-this-buffer
      :desc "My Eval Buffer"            :leader "e"   'my-eval-buffer
      :desc "Link Hint Open Link"       :leader "l"   'link-hint-open-link
      :desc "Flyspell Previous"         :leader "="   'flyspell-correct-wrapper

      :desc "Goto Dashboard"            :leader "gd"  '+doom-dashboard/open
      :desc "Clone Buffer"              :leader "wC"  'clone-indirect-buffer-other-window)

;;;;; ORG ;;;;;
(map! :map (evil-org-mode-map org-mode-map)
      :i "C-l"                                     'pabbrev-expand-maybe
      :n "<backspace>"                             'org-edit-special
      :n "zi"                                      'org-show-all
      :nvieg "M-m"                                 'my-org-edit-special
      :n "C-j"                                     'org-shiftleft
      :n "C-k"                                     'org-shiftright
      :i "C-k"                                     'kill-line
      "C-ç"                                        'counsel-outline
      "C-M-k"                                      'org-metaup
      "C-M-j"                                      'org-metadown
      "C-k"                                        'org-shiftleft
      "C-c b"                                      'org-cycle-list-bullet
      "C-c C-s"                                    'org-emphasize
      :desc "Goto Clock"      :localleader "cs"    'org-clock-display
      ;; :desc "Display Clocked" :localleader "cg" 'org-clock-goto
      )

(map! :map (my-org-mode-map
            my-lisp-interaction-mode-map
            my-markdown-mode
            my-fundamental-mode
            my-text-mode
            my-org-mode)
      :n "<escape>" 'my-force-normal-state
      :n "q"        'quit-window)

;;;;; PROG AND TEXT;;;;;
(map! :map (prog-mode-map)
      :n "<tab>" 'outline-toggle-children
      :ni "C-c h" 'outline-hide-body
      :ni "C-c s" 'outline-show-all
      :ni "C-c o" 'outline-hide-other)

(map! :map (prog-mode-map text-mode-map conf-mode-map)
      :nvieg "<C-backspace>" 'my-comment-line)

(map! :map (occur-mode-map)
      :n "q" 'quit-window)

(map! :map (emacs-lisp-mode-map lisp-mode-map)
      :n "<C-return>" 'eros-eval-last-sexp
      :n "C-m" 'eros-eval-last-sexp
      :i "C-k"      'lispy-kill
      :nvieg "M-," 'evil-previous-open-paren
      :nvieg "M-." 'evil-next-close-paren
      :localleader "0" 'evil-next-close-paren
      :localleader "9" 'evil-previous-open-paren)

(map! :map (flycheck-mode-map)
      :nvieg "C-c f"    'flycheck-first-error)

(map! :map (text-mode-map
            prog-mode-map
            conf-mode-map)
      :n "<escape>"    'my-save-buffer)

(map! :map (pabbrev-mode-map)
      :i "C-9" 'pabbrev-expand-maybe)
;;;;; MISC ;;;;;
(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'my-force-normal-state)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "r" 'deer)

(map! :map (ivy-minibuffer-map)
      "<C-return>" 'ivy-immediate-done)

(map! :map (ivy-minibuffer-map
            ivy-switch-buffer-map
            minibuffer-local-map
            read-expression-map)
      "C-,"      'ivy-previous-line
      "C-."      'ivy-next-line
      "C-k"      'kill-line
      "C-h"      'delete-backward-char)

(map! :map (Info-mode-map)
      :n "<escape>" 'my-force-normal-state
      :n "m"          'Info-menu
      :n "L"          'Info-history-forward
      :n "<right>"          'evil-forward-sentence-begin
      :n "<left>"          'evil-backward-sentence-begin
      ;; :n "<return>"          'Info-follow-nearest-node
      ;; :n "RET"          'Info-follow-nearest-node
      :n "q"          'ignore
      :n "C-n"          'Info-next
      :n "C-p"          'Info-prev
      :n "H"          'Info-history-back
      :n "ci"         'clone-indirect-buffer-other-window
      :n "<C-return>" 'eros-eval-last-sexp
      :n "M-n"        'forward-paragraph)

(map! :map override
      :nv "f"                                'avy-goto-char-2-below
      :nv "F"                                'avy-goto-char-2-above
      :n "C-s"                               '+default/search-buffer
      :i "C-u"                               'my-backward-kill-line
      :n "gr"                                'my-sel-to-end
      :n "ge"                                'evil-end-of-visual-line
      :n "M-e"                               'evil-forward-sentence-begin
      :n "M-a"                               'evil-backward-sentence-begin
      :n "0"                                 'evil-beginning-of-visual-line
      :n "g0"                                'evil-digit-argument-or-evil-beginning-of-line
      :n "!"                                 'my-delete-frame
      :n "Q"                                 'my-delete-frame
      :i "C-d"                               'delete-char
      :i "C-h"                               'delete-backward-char
      :i "C-n"                               'next-line
      :i "C-p"                               'previous-line
      :i "C-e"                               'move-end-of-line
      :i "C-a"                               'move-beginning-of-line
      :ni "<M-return>"                       'my-indent-buffer
      :nvieg "<f8>"                          'man
      :nvieg "C-c <f12>"                     'counsel-org-capture
      :nvieg "C-c m"                         'evil-record-macro
      :nvieg "C-S-j"                         'cool-moves/line-forward
      :nvieg "C-S-k"                         'cool-moves/line-backward
      :nvieg "M-y"                           'counsel-yank-pop
      :nvieg "M-9"                           'delete-window
      :nvieg "M-0"                           'quit-window
      :nvieg "C-0"                           'doom/window-maximize-buffer
      :nvieg "C-9"                           'doom/window-enlargen
      :nvieg "M--"                           'winner-undo
      :nvieg "M-="                           'winner-redo
      :nvieg "M-k"                           'windmove-up
      :nvieg "M-j"                           'windmove-down
      :nvieg "M-h"                           'windmove-left
      :nvieg "M-l"                           'windmove-right
      :nvieg "<M-up>"                        'windmove-up
      :nvieg "<M-down>"                      'windmove-down
      :nvieg "<M-left>"                      'windmove-left
      :nvieg "<M-right>"                     'windmove-right
      :desc "Capture"             :n "Ç"     'org-capture
      :desc "Capture Todo"        :n "ç" 'my-org-capture-todo-macro

      ;; :desc "Capture Goto Last"   :n "çl" 'org-capture-goto-last-stored
      ;; :desc "Capture Goto Target" :n "çt" 'org-capture-goto-target
      "C-c SPC"                              'caps-lock-mode
      "C-c q"                                'quick-calc
      "M-w"                                  'eyebrowse-next-window-config
      "M-q"                                  'eyebrowse-prev-window-config
      "C-c a"                                'align-regexp
      "C-'"                                  'org-cycle-agenda-files
      :nvieg "M-,"                           'projectile-next-project-buffer
      :nvieg "M-."                           'projectile-previous-project-buffer
      "<C-down>"                             'cool-moves/paragraph-forward
      "<C-up>"                               'cool-moves/paragraph-backward
      "C-S-j"                                'cool-moves/line-forward
      "C-S-k"                                'cool-moves/line-backward
      "C-S-n"                                'cool-moves/word-forward
      "C-S-p"                                'cool-moves/word-backwards)

(general-unbind '(scratch-mode-map my-org-mode-map)
  :with 'my-force-normal-state
  [remap my-save-buffer]
  [remap save-buffer])

(general-unbind 'normal lisp-interaction-mode-map
  :with 'ignore
  [remap my-save-buffer])

(general-unbind +doom-dashboard-mode-map
  :with 'forward-button
  [remap evil-better-visual-line-next-line])

(general-unbind +doom-dashboard-mode-map
  :with 'backward-button
  [remap evil-better-visual-line-previous-line])

(general-unbind +doom-dashboard-mode-map
  :with 'quit-window
  [remap evil-record-macro]
  [remap evil-force-normal-state])

(general-unbind +doom-dashboard-mode-map
  :with 'push-button
  [remap evil-forward-char])

(general-unbind 'lispyville-mode-map
  :with 'lispy-repeat
  [remap evil-repeat])

(general-unbind 'lispyville-mode-map
  :with 'evil-switch-to-windows-last-buffer
  [remap lispy-splice])

(general-unbind 'org-capture-mode-map
  :with 'org-capture-finalize
  [remap my-indent-buffer])

(general-unbind 'org-src-mode-map
  :with 'org-edit-src-exit
  [remap lispy-mark-symbol])

;; (define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "<pause>") (kbd "C-c"))
(define-key key-translation-map (kbd "<menu>") (kbd "C-x"))

(map! :n "'"         'evil-goto-mark
      :n "`"         'evil-goto-mark-line
      :n "g."        'evil-repeat
      :n ","         'ivy-switch-buffer
      :n "."         'counsel-find-file
      :n "g4"         'evil-backward-word-end
      :i "M-/"       'hippie-expand
      :n "go"         'cool-moves/open-line-below
      :n "gO"         'cool-moves/open-line-above
      "M-s"                               'evil-switch-to-windows-last-buffer
      :i "C-k"                            'kill-line
      :nvieg "C-."   'my-search-settings
      :nvieg "C-,"   'helpful-at-point
      :nvieg "C-c i" 'insert-char
      "C-c r"        '+popup/raise
      "C-h m"        'my-show-major-mode
      "M-p"          'backward-paragraph
      "M-n"          'forward-paragraph)

;; (use-package org-plus-contrib)
(use-package! org
  :init
  ;; (remove-hook 'org-mode-hook #'pabbrev-mode)
  (remove-hook 'org-cycle-hook 'org-optimize-window-after-visibility-change)
  (remove-hook 'org-mode-hook 'flyspell-mode)
  (add-hook! 'org-src-mode-hook 'my-indent-buffer)
  :custom
  (org-ellipsis ".")
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-log-into-drawer t)
  (org-directory "~/org/")
  (org-clock-into-drawer t)
  (org-clock-history-length 10)
  (org-clock-update-period 240)
  (org-startup-folded 'overview)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-odt-fontify-srcblocks t)
  (org-fontify-done-headline t)
  (org-clock-mode-line-total 'auto)
  (org-clock-clocked-in-display nil)
  (org-clock-persist-query-resume t)
  (org-fontify-whole-heading-line nil)
  (org-edit-src-auto-save-idle-delay 1)
  (org-archive-location ".%s::datetree/")
  (org-fontify-quote-and-verse-blocks nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-report-include-clocking-task t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "|" "DONE(d)")))
  (org-drawers (quote ("properties" "logbook"))) ;; Separate drawers for clocking and logs
  :config

  ;; source: https://bit.ly/38iBxkd
  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    (concat "[s] " org-buffer-name ""))

  (setq! system-time-locale "C"
         org-capture-templates
         '(("t" "Todo"
            entry
            (file+headline "~/org/agenda.org" "Todos")
            "* TODO %? %i" :prepend t)

           ("o" "Notes"
            entry
            (file+headline "~/org/agenda.org"  "Notes")
            "* %u %? %i" :prepend t)

           ("n" "Now"
            entry
            (file+headline "~/org/agenda.org"  "Now")
            "* %? %i" :prepend t)))
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(after! org
  (setq-default org-src-window-setup 'current-window))

(use-package! org-pomodoro
  :after org
  :custom
  (org-pomodoro-offset 1)
  (org-pomodoro-audio-player "/usr/bin/paplay --volume=50768")
  (org-pomodoro-start-sound-args t)
  (org-pomodoro-length (* 25 org-pomodoro-offset))
  (org-pomodoro-short-break-length (/ org-pomodoro-length 5))
  (org-pomodoro-long-break-length (* org-pomodoro-length 0.8))
  (org-pomodoro-long-break-frequency 4)
  (org-pomodoro-ask-upon-killing nil)
  (org-pomodoro-manual-break nil)
  (org-pomodoro-keep-killed-pomodoro-time t)
  ;; (org-pomodoro-time-format "%.2m")
  (org-pomodoro-time-format "%.2m:%.2s")
  (org-pomodoro-short-break-format "SHORT: %s")
  (org-pomodoro-long-break-format "LONG: %s")
  (org-pomodoro-format "P: %s"))

;;;;; source: https://bit.ly/3kE3Pcl ;;;;
(use-package! evil-org
  :config
  (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)
  (add-hook 'org-cycle-hook 'org-cycle-hide-drawers))

(use-package! company
  :custom
  (company-show-numbers t)
  (company-idle-delay 0.2)
  (company-tooltip-limit 5)
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

(use-package! super-save
  :custom
  (auto-save-default nil)
  (super-save-exclude '(".py"))
  (super-save-remote-files nil)
  (super-save-idle-duration 10)
  (super-save-auto-save-when-idle t)
  :config

  (setq super-save-triggers '(next-buffer
                              counsel-M-x
                              windmove-up
                              +eval/buffer
                              other-window
                              windmove-left
                              windmove-down
                              windmove-right
                              previous-buffer
                              org-edit-special
                              switch-to-buffer
                              org-edit-src-exit
                              eyebrowse-last-window-config
                              eyebrowse-next-window-config
                              eyebrowse-close-window-config
                              eyebrowse-create-window-config))

  (add-to-list 'super-save-hook-triggers 'find-file-hook)

  (defun super-save-command ()
    (when (and buffer-file-name
               (buffer-modified-p (current-buffer))
               (file-writable-p buffer-file-name)
               (if (file-remote-p buffer-file-name) super-save-remote-files t)
               (super-save-include-p buffer-file-name))
      (my-just-save-buffer-quiet)))

  (super-save-mode +1))


(use-package! ivy
  :custom
  ;; source:(https://bit.ly/32hmYcU)
  (swiper-use-visual-line nil)
  (ivy-height 15)
  (ivy-extra-directories nil)
  (counsel-outline-display-style 'title)
  (counsel-find-file-at-point t)
  (counsel-bookmark-avoid-dired t)
  (counsel-grep-swiper-limit 5000)
  (ivy-ignore-buffers '("^#.*#$"
                        "^\\*.*\\*"))
  :config
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(use-package! evil
  :custom
  (evil-jumps-cross-buffers nil)
  (evil-respect-visual-line-mode t))

(use-package! evil-better-visual-line
  :config
  (evil-better-visual-line-on))

(use-package! which-key
  :custom
  (which-key-idle-delay 1.0))

(use-package! ranger
  :init
  (add-hook! 'ranger-mode-hook '(ranger-toggle-details olivett-mode))
  :custom
  (ranger-deer-show-details nil))

(use-package! avy
  :custom
  (avy-single-candidate-jump t))

(use-package! olivetti
  :hook (Info-mode . olivetti-mode)
  :init
  (setq-default olivetti-body-width '100))

(use-package! eyebrowse
  :custom
  (eyebrowse-wrap-around t)
  (eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode +1))

(use-package! clipmon
  :config
  (clipmon-mode-start))

(use-package! info
  :init
  (remove-hook 'Info-mode 'olivetti)
  (remove-hook 'Info-mode-hook 'doom-modeline-set-info-modeline)
  :custom
  (info-lookup-other-window-flag nil))

(use-package! lispyville
  :config
  (defalias 'lispyville-yank 'evil-yank))

(use-package! hl-sentence
  :custom-face
  (hl-sentence ((t (:inherit hl-line)))))

(use-package! zoom
  :custom
  (zoom-size '(0.618 . 0.618)))
