;;; leader-kbd.el -*- lexical-binding: t; -*-

(map! :desc "Kill Buffer"               :leader "k"   'kill-this-buffer
      :desc "Yank Dirname"              :leader "fY"  'my-yank-dirname-as-kill
      :desc "Olivetti"                  :leader "to"  'olivetti-mode
      :desc "Xah Clean Empty Lines"     :leader "tD"  'xah-clean-empty-lines
      :desc "Visible Mode"              :leader "tv"  'visible-mode
      :desc "Change Dictionary"         :leader "td"  'ispell-change-dictionary
      :desc "Delete Window"             :leader "0"   'delete-window
      :desc "Quit Window"               :leader "Q"   'quit-window
      :desc "Switch to Scratch"         :leader "X"   'doom/switch-to-scratch-buffer
      :desc "Open Scratch"              :leader "x"   'doom/open-scratch-buffer
      :desc "Eyebrowse New"             :leader "v"   'eyebrowse-create-window-config
      :desc "Eyebrowse Close"           :leader "V"   'eyebrowse-close-window-config
      :desc "Goto Dashboard"            :leader "gd"  '+doom-dashboard/open
      :desc "My Package Commands"       :leader "scp" 'my-show-package-commands
      :desc "My Server Commands"        :leader "scs" 'my-show-server-commands
      :desc "My Info Commands"          :leader "sci" 'my-show-info-commands
      :desc "Clone Buffer"              :leader "wC"  'clone-indirect-buffer-other-window
      :desc "Insert Snippet"            :leader "yi"  'yas-insert-snippet
      :desc "My Rename"                 :leader "fR"  'my-rename-file-and-buffer
      :desc "My Eval Buffer"            :leader "meb" '+eval/buffer
      :desc "My Eval Buffer"            :leader "e"   'my-eval-buffer
      :desc "Trash File"                :leader "fD"  'move-file-to-trash
      :desc "Highlight Line"            :leader "th"  'hl-line-mode
      :desc "Highlight Sentence"        :leader "ts"  'hl-sentence-mode
      :desc "Disable Theme"             :leader "hT"  'disable-theme
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
      :desc "Link Hint Open Link"       :leader "l"   'link-hint-open-link
      :desc "Bash Shebang"              :leader "ib"   'my-bash-shebang
      :desc "Python Shebang"            :leader "ip"   'my-python-shebang
      :desc "Flyspell Mode"             :leader "tS"  'flyspell-mode
      :desc "Flyspell Buffer"           :leader "tb"  'flyspell-buffer
      :desc "Flyspell Previous"         :leader "="   'flyspell-correct-wrapper
      :desc "Edit Hosts"                :leader "fh"  'my-edit-hosts
      :desc "Goto Elisp"                :leader "fe"  'my-find-elisp-tmp
      :desc "Goto Elisp Other Window"   :leader "fE"  'my-find-elisp-tmp-other-window
      :desc "New Snippet"               :leader "yn"  '+snippets/new
      :desc "Edit Snippet"              :leader "ye"  '+snippets/edit
      :desc "Find Snippet"              :leader "yf"  '+snippets/find
      :desc "Reload All"                :leader "yr"  'yas-reload-all
      :desc "Describe Keymaps"          :leader "hbb"  'describe-bindings
      :desc "Show Keymaps"              :leader "hbk"  'which-key-show-keymap
      :desc "Show Top Keymaps"          :leader "hbt"  'which-key-show-top-level
      :desc "Show Major Keymaps"        :leader "hbm"  'which-key-show-major-mode
      :desc "Describe Package"          :leader "hdpP"  'describe-package
      :desc "Show Full Keymaps"         :leader "hbf"  'which-key-show-full-keymap
      :desc "Show Minor Keymaps"        :leader "hbi"  'which-key-show-minor-mode-keymap)
