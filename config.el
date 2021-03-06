;;;;; SETTINGS ;;;;;
(setq! doom-theme 'doom-dracula
       ;; dracula-enlarge-headings nil
       blink-matching-paren t
       markdown-hide-urls t
       eldoc-idle-delay 100
       windmove-wrap-around t
       confirm-kill-emacs nil
       ispell-dictionary "en"
       auto-revert-verbose nil
       user-full-name "mr big"
       doom-localleader-key "m"
       register-preview-delay 2
       text-scale-mode-step 1.06
       use-package-always-defer t
       kill-whole-line t
       large-file-warning-threshold (* (expt 10 6) ;; generate um megabyte
                                       20)         ;; make it 20 megabytes
       display-line-numbers-type nil
       abbrev-file-name "~/.doom.d/etc/abbrev_defs"
       ;; pabbrev-idle-timer-verbose nil
       user-mail-address "mrbig033@protonmail.com"
       bookmark-default-file "~/.doom.d/lisp/bookmarks"
       flycheck-global-modes '(not emacs-lisp-mode lisp-interaction-mode)
       header-line-format "  " ;; source: https://bit.ly/2tdnkkh
       ;; doom-scratch-initial-major-mode 'my-lisp-interaction-mode
       bitly-access-token "3026d7e8b1a0f89da10740c69fd77b4b3293151e"
       doom-variable-pitch-font (font-spec :family "sans" :size 28)
       persp-emacsclient-init-frame-behaviour-override nil
       doom-font (font-spec :family "monospace" :size 27 :weight 'semi-light))

(setq-default fill-column 79)

(add-to-list 'display-buffer-alist
             '("*info*" display-buffer-same-window))
(put 'customize-group 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(load-file "~/.doom.d/lisp/cool-moves/cool-moves.el")
(load-file "~/.doom.d/lisp/auto-capitalize/auto-capitalize.el")

(global-hl-line-mode -1)

(global-evil-visualstar-mode t)
(mouse-avoidance-mode 'banish)
(global-auto-revert-mode t)
(global-subword-mode t)
(global-eldoc-mode nil)
(size-indication-mode -1)
(line-number-mode -1)
(setq column-number-mode nil)
(setq-default column-number-mode nil)
(column-number-mode -1)
(setq-default line-number-mode nil)
(setq-default global-eldoc-mode nil)
(set-popup-rule! "tmp.el" :side 'bottom :modeline t :height 19 :quit 't)

(remove-hook 'server-after-make-frame-hook 'toggle-frame-maximized)

;; (doom-modeline-refresh-font-width-cache
;;  toggle-frame-fullscreen)

(defun my-deer-find-tmp ()
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
  (counsel-ag nil "~/.doom.d/" "-f -G 'config.org'"))

(defun my-save-quit-window ()
  (interactive)
  (my-just-save-buffer-quiet)
  (quit-window))

(defun my-show-racket-commands ()
  (interactive)
  (counsel-M-x "^racket-"))

(defun my-show-racket-repl-commands ()
  (interactive)
  (counsel-M-x "^racket-repl"))

(defun my-evil-substitute ()
  (interactive)
  (evil-ex "%s/"))

(defun my-find-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my-snippet-kill-buffers ()
  "Kill all `snippet-mode' buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'snippet-mode)
      (kill-buffer buffer)
      (message "snippet buffers killed"))))

(defun my-org-toggle-emphasis ()
  (interactive)
  (if org-hide-emphasis-markers
      (progn
        (setq org-hide-emphasis-markers nil)
        (message "emphasis not hidden")
        (org-mode)
        (org-hide-drawer-all))
    (setq org-hide-emphasis-markers t)
    (org-mode)
    (org-hide-drawer-all)
    (message "emphasis hidden")))

(defun my-org-start-pomodoro ()
  (interactive)
  (org-todo "STRT")
  (org-pomodoro))

(defun my-move-file-to-trash ()
  "Go to config.org"
  (interactive)
  (move-file-to-trash (buffer-file-name))
  (kill-this-buffer))

(defun my-find-config.org ()
  "Go to config.org"
  (interactive)
  (find-file "~/.doom.d/config.org"))

(defun my-find-config.el ()
  "Go to config.el"
  (interactive)
  (find-file "~/.doom.d/config.el"))

(defun my-org-edit-special ()
  (interactive)
  (my-tangle-py-config-quiet)
  (org-edit-special)
  (my-recenter-window))

(defun my-org-edit-src-exit ()
  (interactive)
  (my-eval-buffer-quiet)
  (org-edit-src-exit)
  (my-tangle-py-config-quiet)
  (my-recenter-window))

(defun my-org-edit-src-exit-no-eval ()
  (interactive)
  (org-edit-src-exit)
  (my-tangle-py-config-quiet)
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

(defun my-force-normal-state ()
  (interactive)
  (evil-ex-nohighlight)
  (evil-force-normal-state))

(defun my-eval-buffer ()
  (interactive)
  (my-save-buffer)
  (eval-buffer)
  (message " buffer evaluated"))

(defun my-eval-buffer-quiet ()
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)
    (eval-buffer)))

(defun my-sel-to-end ()
  (interactive)
  (evil-visual-char)
  (evil-last-non-blank))

(defun my-yank-dirname-as-kill ()
  " based on <+default/yank-buffer-filename>"
  (interactive)
  (message (kill-new (abbreviate-file-name default-directory))))

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

(define-derived-mode my-emacs-lisp-mode
  emacs-lisp-mode "my-el")

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
  (evil-insert-state))

(defun my-python-shebang ()
  (interactive)
  (kill-region (point-min) (point-max))
  (insert "#!/usr/bin/env python3\n\n")
  ;; (insert "\"\"\" Docstring \"\"\"")
  ;; (insert "\n\n")
  (evil-insert-state))

(defun my-find-elisp-eintr ()
  (interactive)
  (find-file-other-window "~/Documents/study/eintr.el"))

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

(defun my-emacs-init-commands ()
  (interactive)
  (start-process-shell-command "init commands" nil "~/dotfiles/maps/scripts/k")
  (toggle-frame-maximized))

(defun my-tangle-py-config ()
  (interactive)
  (my-just-save-buffer-quiet)
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

(define-key key-translation-map (kbd "<pause>") (kbd "C-x"))
(define-key key-translation-map (kbd "<menu>") (kbd "C-c"))

(map! :desc "Yank Dirname"             :leader "fY"    'my-yank-dirname-as-kill
      :desc "My Rename"                :leader "fR"    'my-rename-file-and-buffer
      :desc "Trash File"               :leader "fD"    'my-move-file-to-trash
      :desc "Goto Scratch"             :leader "fs"    'my-switch-to-scratch
      :desc "Find Config.org"          :leader "fc"    'my-find-config.org
      :desc "Find Config.el"           :leader "fC"    'my-find-config.el
      :desc "Edit Hosts"               :leader "fh"    'my-edit-hosts
      :desc "Goto Elisp"               :leader "fe"    'my-find-elisp-tmp
      :desc "Goto Elisp Eintr"         :leader "fE"    'my-find-elisp-eintr
      :desc "Tangle Config"            :leader "ft"    'my-tangle-py-config
      :desc "Goto Tmp Files"           :leader "fm"    'my-deer-find-tmp

      :desc "Disable Theme"            :leader "hT"    'disable-theme
      :desc "Describe Keymaps"         :leader "hbb"   'describe-bindings
      :desc "Show Keymaps"             :leader "hbk"   'which-key-show-keymap
      :desc "Show Top Keymaps"         :leader "hbt"   'which-key-show-top-level
      :desc "Show Major Keymaps"       :leader "hbm"   'which-key-show-major-mode
      :desc "Describe Package"         :leader "hdpP"  'describe-package
      :desc "Show Full Keymaps"        :leader "hbf"   'which-key-show-full-keymap
      :desc "Show Minor Keymaps"       :leader "hbi"   'which-key-show-minor-mode-keymap

      :desc "Flyspell Mode"            :leader "tS"    'flyspell-mode
      :desc "Typo Mode"                :leader "ty"    'typo-mode
      :desc "Flyspell Buffer"          :leader "tb"    'flyspell-buffer
      :desc "Olivetti"              :leader "to"    'olivetti-mode
      :desc "Xah Clean Empty Lines" :leader "tD"    'xah-clean-empty-lines
      :desc "Visible Mode"          :leader "tv"    'visible-mode
      :desc "Change Dictionary"     :leader "td"    'ispell-change-dictionary
      :desc "Highlight Line"        :leader "th"    'hl-line-mode
      :desc "Hide Mode Line"        :leader "tH"    'hide-mode-line-mode
      :desc "Highlight Sentence"    :leader "ts"    'hl-sentence-mode
      :desc "Aggressive Fill Par"   :leader "tA"    'aggressive-fill-paragraph-mode
      :desc "Auto Fill"             :leader "ta"    'auto-fill-mode
      :desc "Sort by Length"        :leader "tL"    'my-sort-lines-by-length
      ;; :desc "My Org Pomodoro"    :leader "tp"    'my-org-start-pomodoro
      :desc "Org Pomodoro"          :leader "tt"    'org-pomodoro
      :desc "Truncate Lines"        :leader "tu"    'toggle-truncate-lines
      :desc "Column Number Mode"    :leader "tC"    'column-number-mode
      :desc "Centered Cursor Mode"    :leader "tc"    'centered-cursor-mode
      :desc "Ispell English"        :leader "te"    'company-ispell-english
      :desc "Ispell Portugues"      :leader "tp"    'company-ispell-brasileiro

      :desc "New Snippet"              :leader "yn"    'yas-new-snippet
      :desc "Visit Snippet"            :leader "yv"    'yas-visit-snippet-file
      :desc "Reload All"               :leader "yr"    'yas-reload-all
      :desc "Reload Dir"               :leader "yd"    'my-reload-snippets
      :desc "Insert Snippet"           :leader "yi"    'yas-insert-snippet
      :desc "Insert Snippet"           :leader "yy"    'yas-insert-snippet
      :desc "Kill Snippet Buffers"     :leader "yk"    'my-snippet-kill-buffers

      :desc "My Package Commands"      :leader "scp" 'my-show-package-commands
      :desc "My Server Commands"       :leader "scs" 'my-show-server-commands
      :desc "My Info Commands"         :leader "sci" 'my-show-info-commands
      :desc "Engine"                   :leader "se"  'hydra-engine/body
      :desc "Evil Marks"               :leader "sm"  'counsel-evil-marks
      :desc "Use-packages"             :leader "sp"  'my-search-use-packages
      :desc "Functions"                :leader "sf"  'my-search-functions
      :desc "Swiper at Point"          :leader "ss"  'swiper-thing-at-point
      :desc "Evil Substitute"          :leader "su"  'my-evil-substitute

      :desc "Window to Register"       :leader "r"     'window-configuration-to-register
      :desc "Jump to Register"         :leader "j"     'jump-to-register
      :desc "Raise Popup"              :leader "wr"    '+popup/raise
      :desc "Close Popups"             :leader "wc"    '+popup/close-all
      :desc "Clone Buffer"             :leader "wi"    'clone-indirect-buffer-other-window
      :desc "Move Window Very Top"     :leader "wK"    'evil-window-move-very-top
      :desc "Move Window Very Bottom"  :leader "wJ"    'evil-window-move-very-bottom
      :desc "Move Window Far Left"     :leader "wH"    'evil-window-move-far-left
      :desc "Move Window Far Right"    :leader "wL"    'evil-window-move-far-right

      :desc "Bash Shebang"             :leader "ib"    'my-bash-shebang
      :desc "Python Shebang"           :leader "ip"    'my-python-shebang

      :desc "Delete Window"            :leader "0"     'delete-window

      :desc "Open Scratch"             :leader "x"     'my-find-elisp-tmp
      :desc "Ivy Switch Buffer"        :leader ","     '+ivy/switch-buffer

      :desc "Switch Project"           :leader "P"     'projectile-switch-project
      :desc "Add Project"              :leader "A"     'projectile-add-known-project

      :desc "Kill This Buffer"         :leader "k"    'kill-this-buffer
      :desc "Kill Buffer & Window"     :leader "bw"    'kill-buffer-and-window
      :desc "My Eval Buffer"           :leader "e"     'my-eval-buffer
      :desc "My Eval Block"            :leader "E"     'org-babel-execute-src-block
      :desc "Link Hint Open Link"      :leader "l"     'link-hint-open-link
      :desc "Flyspell Previous"        :leader "="     'flyspell-correct-wrapper
      :desc "Ispell Endless"           :leader "-"     'endless/ispell-word-then-abbrev
      :desc "Capture"                  :leader "ç"     'org-capture-goto-last-stored
      :desc "Git Timemachine"          :leader "bg"    'git-timemachine
      :desc "Save Buffer as Root"      :leader "U"     'undo-fu-only-redo-all

      :desc "Agenda"                   :leader "a" 'my-org-agenda
      :desc "Org Timer"                :leader "ot" 'my-show-org-timer-cmds

      :desc "Count Words"              :leader "cw" 'count-words

      :desc "Workspace New"            :leader "v"     'eyebrowse-create-window-config
      :desc "Workspace Close"          :leader "V"     'eyebrowse-close-window-config
      :desc "Workspace Swap Left"      :leader "TAB j" 'eyebrowse-next-window-config
      :desc "Workspace Swap Right"     :leader "TAB k" 'eyebrowse-prev-window-config

      :desc "Remove Buffer"            :leader "TAB 6" 'nil
      :desc "Remove Buffer"            :leader "TAB 7" 'nil
      :desc "Remove Buffer"            :leader "TAB 8" 'nil
      :desc "Remove Buffer"            :leader "TAB 9" 'nil

      :desc "Remove Buffer"            :leader "TAB a" 'persp-add-buffer

      ;; :desc "Goto Dashboard"           :leader "gd"    '+doom-dashboard/open
      :desc "Unbind Switch Buffer"     :leader ","    nil
      :desc "Unbind Counsel Find File" :leader "."    nil
      :desc "Unbind pp eval sexp"      :leader ";"    nil
      )

;;;;; RANGER ;;;;
(map! :after ranger
      :map (ranger-mode-map)
      ("tp"         'move-file-to-trash)
      ("C-n"        'ranger-next-file)
      ("C-p"        'ranger-prev-file)
      ("C-l"        'ranger-find-links-dir)
      ("<insert>"   'dired-create-empty-file)
      ("D"          'dired-do-flagged-delete)
      ("x"          'diredp-delete-this-file)
      ("<C-return>" 'dired-do-find-marked-files))

;; (map! :map (org-journal-mode-map)
;;       :n "<escape>" 'my-save-quit-window)

(map! :map (snippet-mode-map)
      :n "<escape>" 'ignore)

(map! :map (org-mode-map)
      :prefix "<pause>"
      :desc "Roam Toggle"  "<pause>"  'org-roam
      :desc "Roam Add Tag"     "t"    'org-roam-tag-add
      :desc "Roam Delete Tag"  "T"    'org-roam-tag-delete
      :desc "Roam Find File"   "f"    'org-roam-find-file
      :desc "Roam Graph"       "g"    'org-roam-graph
      :desc "Roam Insert Now"  "I"    'org-roam-insert-immediate
      :desc "Roam Insert"      "i"    'org-roam-insert
      :desc "Roam Goto Buffer" "b"    'org-roam-switch-to-buffer
      :desc "Roam Goto Index"  "x"    'org-roam-jump-to-index

      :desc "Dailies Today"     "dt"  'org-roam-dailies-find-today
      :desc "Dailies Capture"   "dc"  'org-roam-dailies-capture-today
      :desc "Dailies Previous"  "dp"  'org-roam-dailies-find-previous-note
      :desc "Dailies Next"      "dn"  'org-roam-dailies-find-next-note
      :desc "Dailies Directory" "dt"  'org-roam-dailies-find-today
      :desc "Dailies Date"      "dd"  'org-roam-dailies-find-date
      :desc "Dailies Date"      "di"  'org-roam-dailies-find-directory)

(map! :after git-timemachine
      :map (git-timemachine-mode-map)
      :n "i" 'ignore
      :n "<escape>" 'git-timemachine-quit
      :n "gtr" 'git-timemachine-show-current-revision)

(map! :map (Man-mode-map)
      :n "<escape>" 'quit-window
      :n "q" 'quit-window)

(map! :after image-mode
      :map (image-mode-map)
      :n "q" 'image-kill-buffer
      :n "<escape>" 'image-kill-buffer)

(map! :map (my-org-mode-map
            my-lisp-interaction-mode-map
            my-markdown-mode
            my-fundamental-mode
            my-emacs-lisp-mode-map
            my-text-mode
            my-org-mode)
      :n "<escape>" 'my-force-normal-state
      :n "<escape>" 'my-force-normal-state
      :n "q"        'quit-window)

(map! :map (lispyville-mode-map)
      :i "M-i" 'tab-to-tab-stop
      :n "C-k" nil
      :i "M-[" 'lispy-brackets
      )

;; (advice-add #'lispy-kill :after #'evil-insert)

(map! :map (my-emacs-lisp-mode-map)
      :n "<escape>" 'my-save-buffer
      :n "q"        'quit-window)

;;;;; PROG AND TEXT;;;;;
(map! :map (prog-mode-map)
      :n "<backspace>" 'my-org-edit-src-exit-no-eval
      :n "<tab>" 'outline-toggle-children
      :ni "C-c h" 'outline-hide-body
      ;; :ni "C-c s" 'outline-show-all
      :ni "C-c o" 'outline-hide-other)

(map! :map (prog-mode-map text-mode-map conf-mode-map)
      :nvieg "<C-backspace>" 'my-comment-line)

(map! :map (occur-mode-map)
      :n "q" 'quit-window)

(map! :map (emacs-lisp-mode-map lisp-mode-map)
      :n "<C-return>" 'eros-eval-last-sexp
      :i "C-k"      'lispy-kill
      ;; :nvieg "M-," 'evil-previous-open-paren
      :n "<backspace>" 'my-org-edit-src-exit-no-eval
      ;; :nvieg "M-." 'evil-next-close-paren
      ;; :nvieg "M-;" 'lispy-eval-expression
      :localleader "0" 'evil-next-close-paren
      :localleader "9" 'evil-previous-open-paren)

(map! :map (flycheck-mode-map)
      :nvieg "C-c f"    'flycheck-first-error)

(map! :map (text-mode-map
            prog-mode-map
            conf-mode-map)
      ;; :n "C-k" 'evil-change-line
      :n "<escape>"    'my-save-buffer)

;; (map! :map (pabbrev-mode-map)
;;       :i "C-9" 'pabbrev-expand-maybe)
;;;;; MISC ;;;;;
(map! :map (help-mode-map helpful-mode-map)
      :n "<escape>"    'my-force-normal-state)

(map! :map ranger-mode-map
      "q" 'ranger-close
      "<escape>" 'ranger-close
      :desc "Deer" :leader "d" 'deer)

(map! :map override
      "C-c 9"                              'org-cycle-agenda-files
      "<C-down>"                           'cool-moves/paragraph-forward
      "<C-up>"                             'cool-moves/paragraph-backward
      "C-S-j"                              'cool-moves/line-forward
      "C-S-k"                              'cool-moves/line-backward
      "C-S-n"                              'cool-moves/word-forward
      "C-S-p"                              'cool-moves/word-backwards
      "C-c SPC"                            'caps-lock-mode
      "C-c a"                              'align-regexp
      "C-c q"                              'quick-calc
      "M-q"                                'eyebrowse-prev-window-config
      "M-w"                                'eyebrowse-next-window-config
      "C-c ;"           '+vterm/toggle
      ;; "M-,"                             '+ivy/switch-workspace-buffer
      :i "C-k"                             'kill-line
      :i "C-a"                             'move-beginning-of-line
      :i "C-d"                             'delete-char
      :i "C-e"                             'move-end-of-line
      :i "C-h"                             'delete-backward-char
      :i "C-n"                             'next-line
      :i "C-p"                             'previous-line
      :i "C-u"                             'my-backward-kill-line
      :n "!"                               'my-delete-frame
      :n "0"                               'evil-beginning-of-visual-line
      :nvieg "C-s"                             '+default/search-buffer
      :n "M-a"                             'evil-backward-sentence-begin
      :n "M-e"                             'evil-forward-sentence-begin
      :n "M-i"                             'evil-jump-forward
      :n "M-o"                             'evil-jump-backward
      ;; :n "Q"                               'my-delete-frame
      :n "g0"                              'evil-digit-argument-or-evil-beginning-of-line
      ;; :n "ge"                              'evil-end-of-visual-line
      :n "gr"                              'my-sel-to-end
      :desc "What Cursor Position" :n "gA" 'what-cursor-position

      :desc "Copy Line" :n "gacl"                                'avy-copy-line
      :desc "Move Line" :n "gaml"                                'avy-move-line

      :v "gr"                                'eval-region
      :v "gW"                                'fill-region
      :ni "<M-return>"                       'my-indent-buffer
      :nv "F"                                'avy-goto-char-2-above
      :nv "f"                                'avy-goto-char-2-below
      ;; :nvieg "M-;"                           'lispy-eval-expression
      :nvieg "<M-down>"                      'windmove-down
      :nvieg "<M-left>"                      'windmove-left
      :nvieg "<M-right>"                     'windmove-right
      :nvieg "<M-up>"                        'windmove-up
      :nvieg "<f10>"                       'man
      :nvieg "C-0"                           'doom/window-maximize-buffer
      :nvieg "C-S-j"                         'cool-moves/line-forward
      :nvieg "C-S-k"                         'cool-moves/line-backward
      :nvieg "C-c m"                         'evil-record-macro
      :nvieg "M--"                           'winner-undo
      :nvieg "M-0"                           'quit-window
      :nvieg "M-9"                           'delete-window
      :nvieg "M-="                           'winner-redo
      :nvieg "M-h"                           'windmove-left
      :nvieg "M-j"                           'windmove-down
      :nvieg "M-k"                           'windmove-up
      :nvieg "M-l"                           'windmove-right
      :nvieg "M-y"                           'counsel-yank-pop

      ;; :desc "Next User Buffer" :nvieg "<f8>" 'projectile-next-project-buffer
      ;; :desc "Prev User Buffer" :nvieg "<f9>" 'projectile-previous-project-buffer
      :desc "Ace Window"       :n "M-ç"      'ace-window
      :desc "Goto Capture"  :n                 "ç"  'org-capture

      )

(general-unbind
  "C-;"
  "C-x m")

(general-unbind '(ivy-minibuffer-map)
  :with 'ivy-next-line
  [remap ivy-switch-buffer])

(general-unbind '(scratch-mode-map my-org-mode-map)
  :with 'my-force-normal-state
  [remap my-save-buffer]
  [remap save-buffer])

(general-unbind 'normal lisp-interaction-mode-map
  :with 'ignore
  [remap my-save-buffer])

(general-unbind 'lispyville-mode-map
  :with 'lispy-repeat
  [remap evil-repeat])

(general-unbind 'lispyville-mode-map
  :with 'evil-switch-to-windows-last-buffer
  [remap lispy-splice])

(general-unbind 'org-capture-mode-map
  :with 'org-capture-finalize
  [remap my-indent-buffer])

(general-unbind 'org-capture-mode-map
  :with 'org-capture-kill
  [remap my-save-buffer])

(general-unbind 'org-src-mode-map
  :with 'my-org-edit-src-exit
  [remap lispy-mark-symbol])

(define-key! +doom-dashboard-mode-map
  [left-margin mouse-1]             #'ignore
  [remap forward-button]            #'+doom-dashboard/forward-button
  [remap backward-button]           #'+doom-dashboard/backward-button
  "n"                               #'forward-button
  "p"                               #'backward-button
  "C-n"                             #'forward-button
  "C-p"                             #'backward-button
  [down]                            #'forward-button
  [up]                              #'backward-button
  [tab]                             #'forward-button
  [backtab]                         #'backward-button

  ;; Evil remaps
  [remap evil-next-line]            #'forward-button
  [remap evil-previous-line]        #'backward-button
  [remap evil-paste-pop-next]       #'forward-button
  [remap evil-paste-pop]            #'backward-button
  [remap evil-backward-word-begin]  #'counsel-bookmark
  [remap evil-paste-before]         #'doom/open-private-config
  [remap evil-backward-char]        #'doom/help
  [remap evil-delete]               #'doom/help
  [remap evil-append]               #'org-agenda
  [remap evil-replace]              #'counsel-recentf
  [remap evil-paste-after]          #'counsel-projectile-switch-project
  [remap evil-record-macro]         #'quit-window
  [remap evil-force-normal-state]   #'quit-window
  [remap evil-forward-char]         #'push-button
  [remap evil-replace-state]        #'ignore
  [remap evil-change]               #'ignore
  [remap evil-change-line]          #'ignore
  [remap evil-visual-char]          #'ignore
  [remap evil-visual-line]          #'ignore
  [remap evil-delete]               #'ignore
  [remap evil-delete-line]          #'ignore
  [remap evil-insert]               #'ignore)

(general-unbind '+doom-dashboard-mode-map
  :with 'forward-button
  [remap evil-better-visual-line-next-line])

(general-unbind '+doom-dashboard-mode-map
  :with 'backward-button
  [remap evil-better-visual-line-previous-line])

(map! "<f8>"          'deft
      "C-h m"         'my-show-major-mode
      "M-n"           'forward-paragraph
      "M-p"           'backward-paragraph
      "M-s"           'evil-switch-to-windows-last-buffer
      :e "<escape>"   'evil-exit-emacs-state
      :i "C-l"        'recenter-top-bottom
      :i "M-z"        'hippie-expand
      :n "'"         'evil-goto-mark
      :n ","          'counsel-M-x
      :n "."          '+ivy/switch-buffer
      :n ";"          'counsel-find-file
      :n "M-SPC"      'cycle-spacing
      :n "`"          'evil-goto-mark-line
      :n "g#"         'evil-backward-WORD-end
      :n "g."         'evil-repeat
      :n "g3"         'evil-backward-word-end
      :n "gM"         'evil-middle-of-visual-line
      :n "gO"         'cool-moves/open-line-above
      :n "gm"         'evil-set-marker
      :n "go"         'cool-moves/open-line-below
      :nvieg "C-,"    'helpful-at-point
      :nvieg "C-."    'my-search-settings
      :nvieg "C-c i"  'insert-char)

(use-package! org-journal
  :init
  (add-hook! 'org-journal-mode-hook
             #'abbrev-mode
             #'electric-operator-mode)
  (add-hook! 'org-journal-after-entry-create-hook
             #'evil-insert-state
             #'delete-other-windows
             #'olivetti-mode)
  :custom
  (org-journal-date-format "%A, %d, %Y")
  (org-journal-file-format "%Y-%m-%d")
  (org-journal-time-format "%R\n")
  ;; :config
  ;; (set-popup-rule! "jrnl" :side 'bottom :modeline nil :height 10 :quit 't)
  )

(use-package! racket
  :init
  (add-hook 'racket-repl-mode-hook 'lispyville-mode)

  (map! :map (racket-mode-map)
        "C-;" 'my-racket-switch-to-repl
        "C-c s" 'my-show-racket-commands
        :ni "<C-return>" 'racket-run-and-switch-to-repl)

  (map! :map (racket-repl-mode-map)
        "C-;" 'racket-repl-switch-to-edit
        "C-c s" 'my-show-racket-repl-commands
        "C-l" 'comint-clear-buffer)

  :config

  (set-company-backend! 'racket-repl-mode
    'company-capf 'company-yasnippet)

  (set-popup-rule! "*Racket REPL**" :side 'bottom :modeline nil :height 19 :quit 't)

  (defun my-racket-switch-to-repl ()
    (interactive)
    (display-buffer racket-repl-buffer-name)
    (select-window (get-buffer-window racket-repl-buffer-name t))
    (evil-insert-state)))

(use-package! recentf
  :ensure nil
  :config
  ;; (add-to-list 'recentf-exclude "\\.el")
  (add-to-list 'recentf-exclude "\\.doom\\.d")
  (add-to-list 'recentf-exclude "\\.tex")
  (add-to-list 'recentf-exclude "tmp")
  (add-to-list 'recentf-exclude "\\.emacs\\.d")
  (add-to-list 'recentf-exclude "emacs-files")
  (add-to-list 'recentf-exclude "roam")
  (add-to-list 'recentf-exclude "trash"))

(use-package! typo
  :config
  (map! :map typo-mode-map
        :i "'" "‘"
        :i "\"" "“")
  (defun typo-insert-cycle (cycle)
    "Insert the strings in CYCLE"
    (let ((i 0)
          (repeat-key last-input-event)
          repeat-key-str)
      (insert (nth i cycle))
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil))
      (while repeat-key
        (message "(inserted %s)"
                 (typo-char-name (nth i cycle))
                 repeat-key-str)
        (if (equal repeat-key (read-event))
            (progn
              (clear-this-command-keys t)
              (delete-char (- (length (nth i cycle))))
              (setq i (% (+ i 1)
                         (length cycle)))
              (insert (nth i cycle))
              (setq last-input-event nil))
          (setq repeat-key nil)))
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event)))))

  (define-typo-cycle typo-cycle-dashes
    "Cycle through various dashes."
    ("— " ; EM DASH
     "-" ; HYPHEN-MINUS
     )))

(use-package! deft
  :init
  (add-hook! 'deft-mode-hook
             #'olivetti-mode
             #'evil-emacs-state)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/roam")
  :config
  (map! :map (deft-mode-map)
        :e "<f8>"     'quit-window
        :e "C-r"      'deft-refresh
        :e "<escape>" 'kill-this-buffer
        :e "q"        'kill-this-buffer
        :e "C-h"      'deft-filter-decrement
        :e "C-u"      'deft-filter-clear
        :e "C-w"      'deft-filter-decrement-word
        :e "C-c k"    'kill-this-buffer))

(use-package! delight
  :after-call after-find-file
  :config
  (delight '((org-mode "[o]" "Org")
             (emacs-lisp-mode "[el]" "Elisp")
             (racket-mode "[rkt]" "Racket")
             (org-journal-mode "[j]" "Journal")

             (fundamental-mode "[fund]" "Fundamental")
             (markdown-mode "[md]" "Markdown"))))

;; (use-package! targets
;;   :init
;;   (setq targets-user-text-objects '((pipe "|" nil separator)
;;                                     (paren "(" ")" pair :more-keys "b")
;;                                     (bracket "[" "]" pair :more-keys "r")
;;                                     (curly "{" "}" pair :more-keys "c")))
;;   :config
;;   (targets-setup t
;;                  :inside-key nil
;;                  :around-key nil
;;                  :remote-key nil))

(use-package! vterm
  :init
  (map! :map vterm-mode-map
        :n "<escape>" '+vterm/toggle))

(use-package! hydra
  :config
  (defhydra hydra-engine (:hint nil :color blue :exit nil :foreign-keys nil)
    "

        Dictionaries    ^^^Others
        -----------------------------
        _f_: free         _g_: google
        _i_: informal     _p_: wiki pt
        _m_: michaelis    _e_: wiki en
        _u_: urban "

    ("<escape>" nil nil)

    ("F" my-engine-free-dictionary)
    ("I" my-engine-search-dic-informal)
    ("M" my-engine-search-michaealis)
    ("U" my-engine-urban-dict)

    ("G" my-engine-google)
    ("P" my-engine-wiki-pt)
    ("E" engine/search-wiki-en)

    ("f" engine/search-the-free-dictionary :hint nil)
    ("i" engine/search-dic-informal :hint nil)
    ("m" engine/search-michaelis :hint nil)
    ("u" engine/search-urban-dictionary :hint nil)

    ("g" engine/search-google :hint nil)
    ("p" engine/search-wiki-pt :hint nil)
    ("e" engine/search-wiki-en :hint nil))

  (defhydra hydra-roam (:hint nil :color blue :exit nil :foreign-keys nil)
    "
        Org Roam
        ^^^--------------------------------
        _r_: roam       _I_: insert quick
        _f_: find file  _m_: roam
        _g_: graph      _t_: add tag
        _i_: insert     _T_: delete tag"

    ("<escape>" nil nil)

    ("r" org-roam)

    ("b" org-roam-switch-to-buffer)
    ("f" org-roam-find-file)
    ("g" org-roam-graph)
    ("i" org-roam-insert)

    ("I" org-roam-insert-immediate)
    ("m" org-roam)
    ("t" org-roam-tag-add)
    ("T" org-roam-tag-delete))
  )

(use-package! git-auto-commit-mode
  :custom
  (gac-debounce-interval (* 60 10))
  (gac-silent-message-p t))

(use-package! super-save
  :after-call after-find-file
  :custom
  (auto-save-default nil)
  (super-save-exclude '(".py"))
  (super-save-remote-files nil)
  (super-save-idle-duration 10)
  (super-save-auto-save-when-idle nil)
  :config

  (setq super-save-triggers '(windmove-up
                              ;; counsel-M-x
                              next-buffer
                              other-window
                              +eval/buffer
                              windmove-down
                              windmove-left
                              windmove-right
                              previous-buffer
                              switch-to-buffer
                              org-edit-special
                              org-edit-src-exit
                              my-find-config.el
                              my-find-config.org
                              my-search-settings
                              my-org-edit-special
                              my-org-edit-src-exit
                              kill-buffer-and-window
                              eyebrowse-next-window-config
                              eyebrowse-last-window-config
                              eyebrowse-close-window-config
                              eyebrowse-create-window-config
                              my-search-settings-from-src-buffer))

  (add-to-list 'super-save-hook-triggers 'find-file-hook)

  (defun super-save-command ()
    (when (and buffer-file-name
               (buffer-modified-p (current-buffer))
               (file-writable-p buffer-file-name)
               (if (file-remote-p buffer-file-name) super-save-remote-files t)
               (super-save-include-p buffer-file-name))
      (my-just-save-buffer-quiet)))

  (super-save-mode +1))

(use-package! ranger
  :demand t
  :init
  (add-hook 'ranger-mode-hook 'olivetti-mode))

(use-package! lispyville
  :after-call after-find-file
  :config
  (advice-add #'lispy-kill :after '(lambda () (evil-insert 1)))
  (defalias 'lispyville-yank 'evil-yank))

(use-package! company
  :after-call after-find-file
  :init
  (add-hook 'company-mode-hook 'company-prescient-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-show-numbers t)
  (company-dabbrev-other-buffers t)
  (company-selection-wrap-around t)
  (company-auto-commit nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-global-modes '(not erc-mode
                              ;; text-mode
                              ;; org-mode
                              ;; markdown-mode
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
   "M-w"    'company-complete
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
  (setq! company-ispell-available t)

  (defun company-ispell-brasileiro ()
    (interactive)
    (setq-local company-ispell-dictionary (file-truename "~/.doom.d/etc/brasileiro.txt"))
    (ispell-change-dictionary "pt_BR")
    (message "company pt"))

  (defun company-ispell-english ()
    (interactive)
    (setq-local company-ispell-dictionary nil)
    (ispell-change-dictionary "pt_BR")
    (message "company en"))

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

(use-package! engine-mode
  :config
  (defengine Google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defun my-engine-google ()
    (interactive)
    (engine/search-google (current-word)))

  (defengine dic-informal
    "https://www.dicionarioinformal.com.br/sinonimos/%s")
  (defun my-engine-search-dic-informal ()
    (interactive)
    (engine/search-dic-informal (current-word)))

  (defengine michaelis
    "https://michaelis.uol.com.br/moderno-portugues/busca/portugues-brasileiro/%s")
  (defun my-engine-search-michaealis ()
    (interactive)
    (engine/search-michaelis (current-word)))

  (defengine urban-dictionary
    "https://www.urbandictionary.com/define.php?term=%s")
  (defun my-engine-urban-dict ()
    (interactive)
    (engine/search-urban-dictionary (current-word)))

  (defengine wiki-pt
    "https://pt.wikipedia.org/wiki/%s")
  (defun my-engine-wiki-pt ()
    (interactive)
    (engine/search-wiki-pt (current-word)))

  (defengine wiki-en
    "https://en.wikipedia.org/wiki/%s")
  (defun my-engine-wiki-en ()
    (interactive)
    (engine/search-wiki-en (current-word)))

  (defengine the-free-dictionary
    "https://www.thefreedictionary.com/%s")
  (defun my-engine-free-dictionary ()
    (interactive)
    (engine/search-the-free-dictionary (current-word)))

  (engine-mode t))

(use-package! google-translate
  :custom
  (google-translate-pop-up-buffer-set-focus t)
  (google-translate-default-source-language "pt")
  (google-translate-default-target-language "en")
  (google-translate-translation-directions-alist '(("pt" . "en") ("en" . "pt"))))

(use-package! olivetti
  :after-call after-find-file
  :hook (Info-mode . olivetti-mode)
  :init
  (setq-default olivetti-body-width '90))

(use-package! eyebrowse
  :after-call after-find-file
  :custom
  (eyebrowse-wrap-around t)
  (eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode +1))

(use-package! ace-window
  :after-call after-find-file
  :custom
  (aw-ignore-current t)
  (aw-scope 'frame)
  (aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f)))

(use-package! ivy
  :custom
  ;; source:(https://bit.ly/32hmYcU)
  (swiper-use-visual-line nil)
  (ivy-extra-directories nil)
  (counsel-outline-display-style 'title)
  (counsel-find-file-at-point t)
  (counsel-bookmark-avoid-dired t)
  (counsel-grep-swiper-limit 5000)
  (ivy-ignore-buffers '("^#.*#$"
                        "^\\*.*\\*"))
  :config

  (defun my-search-functions ()
    (interactive)
    (my-find-config.org)
    (swiper "(defun my"))

  (defun my-search-use-packages ()
    (interactive)
    (my-find-config.org)
    (swiper "(use-package! "))

  (map! :map (ivy-minibuffer-map)
        "<C-return>" 'ivy-immediate-done
        "<insert>" 'yank
        :map (ivy-minibuffer-map
              ivy-switch-buffer-map
              minibuffer-local-map
              read-expression-map)
        "C-,"      'ivy-previous-line
        "C-."      'ivy-next-line
        "C-k"      'kill-line
        "C-h"      'delete-backward-char)

  (setq! ivy-height 12
         swiper-use-visual-line-p (lambda (a) nil)))

(use-package ivy-prescient
  :custom
  (ivy-prescient-sort-commands '(:not
                                 swiper
                                 swiper-isearch
                                 ivy-switch-buffer
                                 lsp-ivy-workspace-symbol
                                 ivy--restore-session
                                 counsel-grep
                                 counsel-git-grep
                                 counsel-rg
                                 counsel-ag
                                 counsel-ack
                                 counsel-fzf
                                 counsel-pt
                                 counsel-imenu)))

(use-package! avy
  :after-call after-find-file
  :custom
  (avy-highlight-first t)
  (avy-single-candidate-jump t))

(use-package! hl-sentence
  :after-call after-find-file
  :custom-face
  (hl-sentence ((t (:inherit hl-line)))))

;; (use-package org-plus-contrib)
(use-package! org
  :after-call after-find-file
  :init
  (add-hook 'org-mode-hook 'abbrev-mode)
  (add-hook 'org-timer-done-hook 'my-find-scratch)
  (remove-hook 'org-cycle-hook 'org-optimize-window-after-visibility-change)
  (remove-hook 'org-mode-hook 'flyspell-mode)
  (add-hook! 'org-src-mode-hook 'my-indent-buffer)
  (add-hook! 'org-agenda-mode-hook 'hl-line-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (flycheck-mode -1)))
  :custom
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-show-all-dates nil)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-show-outline-path nil)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-archived-trees nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-unavailable-files t)
  (org-archive-location ".%s::datetree/")
  (org-attach-auto-tag "at")
  (org-catch-invisible-edits 'smart)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (org-clock-clocked-in-display nil)
  (org-clock-display-default-range 'thisyear)
  (org-clock-history-length 10)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-mode-line-total 'auto)
  (org-clock-persist t)
  (org-clock-persist-query-resume t)
  (org-clock-report-include-clocking-task t)
  (org-clock-update-period 240)
  (org-directory "~/org/")
  (org-drawers (quote ("properties" "logbook"))) ;; Separate drawers for clocking and logs
  (org-edit-src-auto-save-idle-delay 1)
  (org-edit-src-persistent-message nil)
  (org-ellipsis ".")
  (org-enforce-todo-checkbox-dependencies t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-footnote-auto-label t)
  (org-hide-emphasis-markers nil)
  (org-log-into-drawer t)
  (org-odt-fontify-srcblocks t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'overview)

  ;; (org-agenda-tags-column -80)
  :config

  (set-company-backend! '(org-mode org-journal-mode)
    'company-ispell 'company-dabbrev 'company-capf)

  (add-to-list 'org-link-abbrev-alist '("at" . org-attach-expand-link))
  (set-popup-rule! "*Org Agenda*" :side 'bottom :modeline t :height 19 :quit 't)

  (setq org-file-apps '((auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . default))
        org-todo-keywords '((sequence "T(t)" "S(s!)" "|" "D(d!)")))

  (general-unbind 'evil-org-agenda-mode-map
    :with 'org-agenda-next-item
    [remap org-agenda-next-line])

  (general-unbind 'evil-org-agenda-mode-map
    :with 'org-agenda-previous-item
    [remap org-agenda-previous-line])

  (map! :map (evil-org-mode-map org-mode-map)
        "C-M-j"                                   'org-metadown
        "C-M-k"                                   'org-metaup
        "C-c C-s"                                 'org-emphasize
        "C-c b"                                   'org-cycle-list-bullet
        "C-ç"                                     'counsel-outline
        ;; "<pause>"                                 'hydra-roam/body
        ;;
        :nvig "<insert>" 'org-insert-link
        :n "C-k" 'evil-change-line
        :i "C-l"                                    'recenter-top-bottom
        :n "<backspace>"                          'org-edit-special
        :n "zi"                                   'org-show-all
        :n "H" 'org-shiftleft
        :n "L" 'org-shiftright
        :nvieg "M-;"                              nil
        :nvieg "M-m"                              'my-org-edit-special
        :desc "Goto Clock"                         :localleader "cs" 'org-clock-display
        :desc "Web Insert Link"              :localleader "wi" 'org-web-tools-insert-link-for-url
        :desc "Web Insert as Entry"          :localleader "wI" 'org-web-tools-insert-web-page-as-entry
        :desc "Web Archive Attach"           :localleader "wa" 'org-web-tools-archive-attach
        :desc "Web Archive View"             :localleader "wA" 'org-web-tools-archive-view
        :desc "Web Read In Org"              :localleader "wr" 'org-web-tools-read-url-as-org
        :desc "Web Convert to Entries" :localleader "wc" 'org-web-tools-convert-links-to-page-entries)

  ;; source: https://bit.ly/38iBxkd
  (defun org-src--construct-edit-buffer-name (org-buffer-name lang)
    (concat "[s] " org-buffer-name ""))

  (setq! system-time-locale "C"
         org-capture-templates
         '(("ç" "Quick Todos"
            entry
            (file+headline "~/org/agenda.org" "Todos")
            "* TODO %^{title}%i%?" :prepend nil :immediate-finish t)

           ("t" "Full Todos"
            entry
            (file+headline "~/org/agenda.org" "Todos")
            "* TODO %^{title}%i\nSCHEDULED: %^t\n%?" :prepend nil)

           ("o" "Notes"
            entry
            (file+headline "~/org/agenda.org"  "Notes")
            "* %^{title}%i\n%u\n%?" :prepend t)))

  (defun my-org-agenda ()
    (interactive)
    (org-agenda t "a"))

  (defun my-org-todos-agenda ()
    (interactive)
    (org-agenda t "T"))

  (defun my-org-today-agenda ()
    (interactive)
    (let ((current-prefix-arg 1)
          (org-deadline-warning-days 0))
      (org-agenda t "a")))

  (defun my-org-7-days-agenda ()
    (interactive)
    (let ((current-prefix-arg 7)
          (org-deadline-warning-days 0))
      (org-agenda t "a")))

  (defun my-org-30-days-agenda ()
    (interactive)
    (let ((current-prefix-arg 30)
          (org-deadline-warning-days 0))
      (org-agenda t "a")))

  (defun my-show-org-timer-cmds ()
    (interactive)
    (counsel-M-x "^org-timer-"))

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
  (org-pomodoro-manual-break t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-time-format "%.2m:%.2s")
  (org-pomodoro-short-break-format "SHORT: %s")
  (org-pomodoro-long-break-format "LONG: %s")
  (org-pomodoro-format "P: %s"))

;;;;; source: https://bit.ly/3kE3Pcl ;;;;
(use-package! evil-org
  :config
  (remove-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)
  (add-hook 'org-cycle-hook 'org-cycle-hide-drawers))

(use-package! evil
  :init
  (add-hook 'better-jumper-post-jump-hook 'my-recenter-window)
  :custom
  (evil-ex-substitute-global t)
  (evil-jumps-cross-buffers nil)
  (evil-escape-unordered-key-sequence '("jk"))
  (evil-respect-visual-line-mode t)
  :config
  (evil-better-visual-line-on))

(after! evil
  (set-evil-initial-state! 'vterm-mode 'insert)
  (set-evil-initial-state! 'deft-mode 'emacs))

(use-package! evil-better-visual-line
  :config
  (evil-better-visual-line-on))

(use-package! doom-modeline
  :custom
  (doom-modeline-env-version nil)
  (doom-modeline-env-enable-go nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-env-enable-ruby nil)
  (doom-modeline-env-enable-perl nil)
  (doom-modeline-env-enable-rust nil)
  (doom-modeline-env-enable-python nil)
  (doom-modeline-env-enable-elixir nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-env-load-string ".")
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  :config
  (add-to-list 'doom-modeline-continuous-word-count-modes 'org-journal-mode))

(use-package! time
  :custom
  (display-time-format "| %a | %H:%M |")
  (display-time-interval (* 60 5))
  (display-time-default-load-average nil)
  :config
  (display-time-mode +1))

(use-package! which-key
  :custom
  (which-key-idle-delay 1.0))

(use-package! info
  :init
  (remove-hook 'Info-mode-hook 'olivetti)
  (remove-hook 'Info-mode-hook 'doom-modeline-set-info-modeline)
  :custom
  (info-lookup-other-window-flag nil)
  :config
  (map! :map (Info-mode-map)
        :n "<escape>"    'my-force-normal-state
        :n "m"           'Info-menu
        :n "L"           'Info-history-forward
        :n "<right>"     'evil-forward-sentence-begin
        :n "<left>"      'evil-backward-sentence-begin
        :n "q"           'evil-switch-to-windows-last-buffer
        :n "C-n"         'Info-next
        :n "C-p"         'Info-prev
        :n "H"           'Info-history-back
        :n "ci"          'clone-indirect-buffer-other-window
        :n "<C-return>"  'eros-eval-last-sexp
        :n "M-n"         'forward-paragraph))
