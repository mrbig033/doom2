**Describe the feature**

This is a very simple suggestion. Right now `doom` comes with the command `doom/delete-this-file` mapped to `SPC f D`, which is fine. It's not super accessible, which is good for a destructive command. It is, nevertheless, a rather dangerous command. As an alternative, the command `move-file-to-trash` is working fine on Kubuntu, sending the file to the trash insted of deleting it. Maybe Doom should avoid such destructive commands?

**System information**

**Operating System**
Distributor ID: Ubuntu
Description:    Ubuntu 20.04.1 LTS (Kubuntu)
Release:        20.04
Codename:       focal


**Output of "doom info"**
<details><pre>
((system
  (type . gnu/linux)
  (config . "x86_64-pc-linux-gnu")
  (shell . "/bin/bash")
  (uname . "Linux 5.4.0-52-generic #57-Ubuntu SMP Thu Oct 15 10:57:00 UTC 2020 x86_64")
  (path "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/snap/bin" "~/maps/scripts" "~/.fzf/bin" "/usr/lib/emacs/27.1/x86_64-linux-gnu"))
 (emacs
  (dir . "~/.emacs.d/")
  (version . "27.1")
  (build . "set 19, 2020")
  (buildopts . "--build=x86_64-linux-gnu --prefix=/usr '--includedir=${prefix}/include' '--mandir=${prefix}/share/man' '--infodir=${prefix}/share/info' --sysconfdir=/etc --localstatedir=/var --disable-silent-rules '--libdir=${prefix}/lib/x86_64-linux-gnu' '--libexecdir=${prefix}/lib/x86_64-linux-gnu' --disable-maintainer-mode --disable-dependency-tracking --prefix=/usr --sharedstatedir=/var/lib --libexecdir=/usr/lib --localstatedir=/var/lib --infodir=/usr/share/info --mandir=/usr/share/man --enable-locallisppath=/etc/emacs:/usr/local/share/emacs/27.1/site-lisp:/usr/local/share/emacs/site-lisp:/usr/share/emacs/27.1/site-lisp:/usr/share/emacs/site-lisp --program-suffix=27 --with-modules --with-file-notification=inotify --with-mailutils --with-harfbuzz --with-json --with-x=yes --with-x-toolkit=gtk3 --with-lcms2 --with-cairo --with-xpm=yes --with-gif=yes --with-gnutls=yes --with-jpeg=yes --with-png=yes --with-tiff=yes --with-xwidgets 'CFLAGS=-g -O2 -fdebug-prefix-map=/build/emacs27-bifpWT/emacs27-27.1~1.git86d8d76aa3=. -fstack-protector-strong -Wformat -Werror=format-security -no-pie' 'CPPFLAGS=-Wdate-time -D_FORTIFY_SOURCE=2' 'LDFLAGS=-Wl,-Bsymbolic-functions -Wl,-z,relro -no-pie'")
  (features . "XPM JPEG TIFF GIF PNG RSVG CAIRO SOUND GPM DBUS GSETTINGS GLIB NOTIFY INOTIFY ACL LIBSELINUX GNUTLS LIBXML2 FREETYPE HARFBUZZ M17N_FLT LIBOTF ZLIB TOOLKIT_SCROLL_BARS GTK3 X11 XDBE XIM MODULES THREADS XWIDGETS LIBSYSTEMD JSON PDUMPER LCMS2 GMP")
  (traits batch server-running envvar-file))
 (doom
  (dir . "~/.doom.d/")
  (version . "2.0.9")
  (build . "HEAD -> develop b25fdf0 2020-10-18 17:10:33 -0300")
  (elc-files . 0)
  (modules :completion company (ivy +prescient) :ui doom doom-dashboard modeline ophints (popup +defaults) zen :editor (evil +everywhere) file-templates fold snippets word-wrap :emacs (dired +ranger) electric undo vc :checkers syntax (spell +flyspell +aspell) :tools (eval +overlay) (lookup +dictionary +offline) pdf :lang emacs-lisp markdown org sh :config (default +bindings +smartparens))
  (packages (olivetti) (electric-operator) (fountain-mode) (evil-better-visual-line) (caps-lock) (pabbrev) (url-shortener) (super-save) (eyebrowse) (clipmon) (evil-snipe :disable t))
  (unpin "n/a")
  (elpa "n/a")))
</pre></details>
