(use-modules
 ((gnu home services desktop) #:select (home-dbus-service-type))
 ((gnu home services dict) #:select (home-dicod-service-type))
 ((gnu home services fontutils) #:select (home-fontconfig-service-type))
 ((gnu home services gnupg) #:select (home-gpg-agent-service-type
                                      home-gpg-agent-configuration))
 ((gnu home services shells) #:select (home-bash-service-type
                                       home-shell-profile-service-type))
 ((gnu home services shepherd) #:select (home-shepherd-service-type))
 ((gnu home services syncthing) #:select (home-syncthing-service-type))
 ((gnu home services xdg) #:select (home-xdg-mime-applications-configuration
                                    home-xdg-mime-applications-service-type
                                    home-xdg-user-directories-configuration
                                    home-xdg-user-directories-service-type xdg-desktop-entry))
 ((gnu home services) #:select (home-environment-variables-service-type
                                home-files-service-type
                                home-run-on-first-login-service-type))
 ((gnu packages fonts) #:select (font-openmoji font-wqy-zenhei))
 ((gnu packages freedesktop) #:select (xdg-desktop-portal
                                       xdg-desktop-portal-wlr
                                       xdg-desktop-portal-gtk))
 ((gnu packages gnupg) #:select (pinentry-emacs))
 ((gnu packages linux) #:select (alsa-utils))
 ((gnu packages qt) #:select (qtwayland qtbase))
 ((gnu packages wm) #:select (sway swaylock swayidle mako))
 ((gnu packages xdisorg) #:select (bemenu))
 ((gnu services) #:select (service simple-service))
 ((guix gexp) #:select (file-append plain-file mixed-text-file)))

(home-environment
 (packages (list
            sway
            swaylock
            swayidle
            bemenu ;; so dbus can use this
            xdg-desktop-portal
            xdg-desktop-portal-wlr
            xdg-desktop-portal-gtk
            mako
            font-openmoji ; emoji
            font-wqy-zenhei ; Asian
            qtbase
            qtwayland
            ;; TODO: Can't add these to profile.  See https://issues.guix.gnu.org/65508
            ;; qtbase-5
            ;; qtwayland-5
            ))
 (services
  (list
   (service home-shepherd-service-type)
   (service home-dbus-service-type)
   (service home-bash-service-type)

   (service home-dicod-service-type) ;; Dictionary server
   (service home-syncthing-service-type)

   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)))

   (simple-service 'stuff
                   home-shell-profile-service-type
                   (list (plain-file "profile" "
DEFAULT_PROFILE=$HOME/.config/guix/extra-profiles/default/default
EMACS_PROFILE=$HOME/.config/guix/extra-profiles/emacs/emacs

eval \"$(guix package --search-paths=suffix --profile=$DEFAULT_PROFILE)\"
eval \"$(guix package --search-paths=suffix --profile=$EMACS_PROFILE)\"

export MANPATH=$DEFAULT_PROFILE/share/man${MANPATH:+:}$MANPATH
export MANPATH=$EMACS_PROFILE/share/man${MANPATH:+:}$MANPATH

export INFOPATH=$DEFAULT_PROFILE/share/info${INFOPATH:+:}$INFOPATH
export INFOPATH=$EMACS_PROFILE/share/info${INFOPATH:+:}$INFOPATH

export XDG_DATA_DIRS=$DEFAULT_PROFILE/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS
export XDG_DATA_DIRS=$EMACS_PROFILE/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS

export XDG_CONFIG_DIRS=$DEFAULT_PROFILE/etc/xdg${XDG_CONFIG_DIRS:+:}$XDG_CONFIG_DIRS
export XDG_CONFIG_DIRS=$EMACS_PROFILE/etc/xdg${XDG_CONFIG_DIRS:+:}$XDG_CONFIG_DIRS

# Start graphical interface
if [ \"$(tty)\" = \"/dev/tty7\" ]; then
    chvt 7
    sway
fi
")))

   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (desktop "$HOME")
             (documents "$HOME/documents")
             (download "$HOME/downloads")
             (music "$HOME/music")
             (pictures "$HOME/pictures")
             (publicshare "$HOME")
             (templates "$HOME")
             (videos "$HOME")))

   (service home-xdg-mime-applications-service-type
    (home-xdg-mime-applications-configuration
     (default
       '((application/x-bittorrent . transmission.desktop)
         (x-scheme-handler/magnet  . transmission.desktop)
         (x-scheme-handler/unknown . emacsurl.desktop)
         (x-scheme-handler/about   . emacsurl.desktop)
         (x-scheme-handler/https   . emacsurl.desktop)
         (x-scheme-handler/http    . emacsurl.desktop)
         (text/html                . emacsurl.desktop)
         (x-scheme-handler/mailto  . emacsurl.desktop)
         (application/pdf          . emacs.desktop)
         (application/postscript   . emacs.desktop)
         (application/x-csv        . emacs.desktop)
         (image/gif                . emacs.desktop)
         (image/jpeg               . emacs.desktop)
         (image/png                . emacs.desktop)
         (inode/directory          . emacs.desktop)
         (text/plain               . emacs.desktop)
         (text/x-shellscript       . emacs.desktop)))
     (desktop-entries
      (list
       (xdg-desktop-entry
        (file "mpv")
        (name "mpv")
        (type 'application)
        (config '((exec . "mpv -- %U"))))
       (xdg-desktop-entry
        (file "transmission")
        (name "Transmission")
        (type 'application)
        (config '((exec . "transmission-remote -a %u"))))
       (xdg-desktop-entry
        (file "emacs")
        (name "Emacs")
        (type 'application)
        (config '((exec . "emacsclient -a emacs %u"))))
       (xdg-desktop-entry
        (file "emacsurl")
        (name "Emacs URL")
        (type 'application)
        (config '((exec . "emacsclient -a emacs --eval \"(browse-url (string-remove-suffix \\\"'\\\" (string-remove-prefix \\\"'\\\" \\\"%u\\\")))\""))))))))

   (simple-service 'some-useful-env-vars-service
          		   home-environment-variables-service-type
          		   `(

                     ("LEDGER_FILE" . "$HOME/documents/money/money.ledger")

                     ;; Wayland variables
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("GDK_BACKEND" . "wayland")
                     ("SDL_VIDEODRIVER" . "wayland")
                     ("GTK_USE_PORTAL" . "1")

                     ("QT_QPA_PLATFORM" . "wayland")
                     ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")


                     ("ASPELL_CONF" . "per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl")
                     ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
                     ("HISTFILE" . "$XDG_DATA_HOME/shell-history")
                     ("INPUTRC" . "$XDG_CONFIG_HOME/readline/inputrc")
                     ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/password-store")
                     ("TEXMFCONFIG" . "$XDG_CONFIG_HOME/texlive/texmf-config")
                     ("TEXMFHOME" . "$XDG_DATA_HOME/texmf")
                     ("TEXMFVAR" . "$XDG_CACHE_HOME/texlive/texmf-var")
                     ("WGETRC" . "$XDG_CONFIG_HOME/wget/wgetrc")
                     ("_JAVA_OPTIONS" . "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java")

                     ("EDITOR" . "emacsclient")
                     ("HISTFILESIZE" . "100000")
                     ("HISTSIZE" . "100000")
                     ("HISTTIMEFORMAT" . "[%F %T] ")))

   ;; Color emoji using OpenMoji
   (simple-service 'additional-fonts-service
                   home-fontconfig-service-type
                   (list '(rejectfont
                           (pattern
                            (patelt
                             (@ (name "family"))
                             (string "OpenMoji"))
                            (patelt
                             (@ (name "style"))
                             (string "Black"))))))

   (simple-service
    'dotfiles
    home-files-service-type
    `(

      ;; Dark theme
      (".config/gtk-3.0/settings.ini"
       ,(plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme=1\n"))

      (".config/gtk-4.0/settings.ini"
       ,(plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme=1\n"))

      (".config/glib-2.0/settings/keyfile"
       ,(plain-file "keyfile" "[org/gnome/desktop/interface]
color-scheme='prefer-dark'\n"))

      ;; Prevent wget from creating history file in home directory
      (".config/wget/wgetrc"
       ,(plain-file "wgetrc" "hsts-file=~/.cache/wget-hsts\n"))

      (".config/duc/ducrc"
       ,(plain-file "ducrc"
                    (string-join
                     '("[index]"
                       "one-file-system"
                       "check-hard-links"
                       "progress")
                     "\n"
                     'suffix)))

      ;; Play audio sound on notification
      (".config/mako/config"
       ,(mixed-text-file "mako-config"
                         "on-notify=exec "
                         alsa-utils "/bin/aplay"
                         " /home/pancake/documents/configs/notification.wav\n"
                         "ignore-timeout=1"))

      ;; Plain black lockscreen
      (".config/swaylock/config"
       ,(plain-file "swaylock-config" "color=000000FF\nscaling=solid_color\n"))

      ;; Always create a graphical window even if there is no video.  This makes
      ;; is easy to close mpv
      (".config/mpv/mpv.conf"
       ,(plain-file "mpv-config" "force-window=yes\nhwdec=auto-safe\n"))

      ;; Move between chapters using '(' and ')'
      (".config/mpv/input.conf"
       ,(plain-file "mpv-input-config" ") add chapter 1\n( add chapter -1\n"))

      ;; Only download 1080p or lower.  Place in ~/downloads/videos with a
      ;; specific filename.  Grab English subtitles if we can.  Add
      ;; sponderblock metadata
      (".config/yt-dlp/config"
       ,(plain-file "yt-dlp-config"
                    "\
-f bestvideo[height<=?1080]+bestaudio/best
-o '|%(upload_date>%Y-%m-%d)+U|%(uploader)+U|%(title)+U|%(id)+U.%(ext)+U'
--paths '~/downloads/videos/'
--restrict-filenames
--concurrent-fragments 10
--write-sub
--sub-lang en
--sponsorblock-mark all\n")))))))
