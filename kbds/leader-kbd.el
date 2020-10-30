;;; leader-kbd.el -*- lexical-binding: t; -*-

(map! :desc "Kill Buffer"             :leader "k"   'kill-this-buffer
      :desc "Yank Dirname"            :leader "fY"  'my-yank-dirname-as-kill
      :desc "Olivetti"                :leader "to"  'olivetti-mode
      :desc "Xah Clean Empty Lines"   :leader "tD"  'xah-clean-empty-lines
      :desc "Visible Mode"            :leader "tv"  'visible-mode
      :desc "Change Dictionary"       :leader "td"  'ispell-change-dictionary
      :desc "Delete Window"           :leader "0"   'delete-window
      :desc "Quit Window"             :leader "Q"   'quit-window
      :desc "Flyspell Buffer"         :leader "tb"  'flyspell-buffer
      :desc "Flyspell Previous"       :leader "tp"  'flyspell-correct-previous
      :desc "Org Capture Goto Target" :leader "nn"  'org-capture-goto-target
      :desc "Org Capture"             :leader "nN"  'org-capture
      :desc "Org Capture Goto Last"   :leader "nL"  'org-capture-goto-last-stored
      :desc "Switch to Scratch"       :leader "bx"  'doom/switch-to-scratch-buffer
      :desc "Switch to Scratch"       :leader "x"   'doom/switch-to-scratch-buffer
      :desc "Open Scratch"            :leader "bX"  'doom/open-scratch-buffer
      :desc "Eyebrowse New"           :leader "v"   'eyebrowse-create-window-config
      :desc "Eyebrowse Close"         :leader "V"   'eyebrowse-close-window-config
      :desc "Goto Dashboard"          :leader "gd"  '+doom-dashboard/open
      :desc "My Package Commands"     :leader "scp" 'my-show-package-commands
      :desc "My Server Commands"      :leader "scs" 'my-show-server-commands
      :desc "New Snippet"             :leader "yn"  '+snippets/new
      :desc "Edit Snippet"            :leader "ye"  '+snippets/edit
      :desc "Find Snippet"            :leader "yf"  '+snippets/find
      :desc "Insert Snippet"          :leader "yi"  'yas-insert-snippet
      :desc "My Rename"               :leader "fR"  'my-rename-file-and-buffer
      :desc "My Eval Buffer"          :leader "meb"  'my-eval-buffer)
