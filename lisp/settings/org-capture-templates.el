(setq! system-time-locale "C"
         org-capture-templates
         '(("t" "Todo"
            entry
            (file+headline +org-capture-todo-file "Todos")
            "* TODO %? %i" :prepend t)

           ("o" "Notes"
            entry
            (file+headline +org-capture-notes-file "Notes")
            "* %u %? %i" :prepend t)

           ("j" "Journal"
            entry
            (file+olp+datetree +org-capture-journal-file)
            "* %u %? %i" :prepend t)

           ("n" "Now"
            entry
            (file+headline "now.org" "Now")
            "* %? %i" :prepend t)))
