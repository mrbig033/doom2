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
