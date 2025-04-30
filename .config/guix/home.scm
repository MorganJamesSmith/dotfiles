(use-modules
 (gnu home services desktop)
 (gnu home services dict)
 (gnu home services fontutils)
 (gnu home services gnupg)
 (gnu home services guix)
 (gnu home services mail)
 (gnu home services pm)
 (gnu home services shells)
 (gnu home services shepherd)
 (gnu home services sound)
 (gnu home services syncthing)
 (gnu home services xdg)
 (gnu home services)
 (gnu packages chromium)
 (gnu packages fonts)
 (gnu packages freedesktop)
 (gnu packages glib)
 (gnu packages gnupg)
 (gnu packages librewolf)
 (gnu packages linux)
 (gnu packages wm)
 (gnu packages xdisorg)
 (gnu services)
 (gnu system shadow)
 (guix channels)
 (guix gexp))

(home-environment
 (packages (list
            sway
            swaylock
            swayidle
            xss-lock
            dbus ;; so sway can use "dbus-update-activation-environment"
            bemenu ;; so dbus can use this
            xdg-desktop-portal
            xdg-desktop-portal-wlr
            xdg-desktop-portal-gtk
            xdg-utils
            mako
            font-openmoji ; emoji
            font-wqy-zenhei ; Asian
            librewolf
            ungoogled-chromium/wayland))
 (services
  (cons*
   (include "/home/pancake/documents/configs/private/home-services.scm")

   (simple-service 'nonguix-channel-service
                   home-channels-service-type
                   (list
                    (channel
                     (name 'nonguix)
                     (url "https://gitlab.com/nonguix/nonguix")
                     ;; Enable signature verification:
                     (introduction
                      (make-channel-introduction
                       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                       (openpgp-fingerprint
                        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

   (service home-shepherd-service-type)
   (service home-dbus-service-type)
   (service home-bash-service-type)

   (service home-dicod-service-type) ;; Dictionary server
   (service home-syncthing-service-type)

   (service home-pipewire-service-type)

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
         (application/pdf          . emacsclient.desktop)
         (application/postscript   . emacsclient.desktop)
         (application/x-csv        . emacsclient.desktop)
         (image/gif                . emacsclient.desktop)
         (image/jpeg               . emacsclient.desktop)
         (image/png                . emacsclient.desktop)
         (inode/directory          . emacsclient.desktop)))
     (desktop-entries
      (list
       (xdg-desktop-entry
        (file "transmission")
        (name "Transmission")
        (type 'application)
        (config '((exec . "transmission-remote -a %u"))))))))

   (simple-service 'some-useful-env-vars-service
          		   home-environment-variables-service-type
          		   `(
                     ;; display dates and times using ISO8601
                     ("LC_TIME" . "en_DK.utf8")

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

                     ("GTK_THEME" . "Adwaita:dark")

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

   (service
    home-xdg-configuration-files-service-type
    `(("gdb/gdbinit" ,(plain-file
                       (plain-file-name %default-gdbinit)
                       (string-append (plain-file-content %default-gdbinit)
                                      (string-join
                                       (list
                                        ""
                                        "set confirm no"
                                        ;; XXX: Do I need to `mkdir -p ~/.cache/gdb`?
                                        "set history filename ~/.cache/gdb/history"
                                        "set history save on"
                                        "set history size unlimited"
                                        "")
                                       "\n"))))
      ("nano/nanorc" ,%default-nanorc)

      ;; Dark theme
      ("gtk-3.0/settings.ini"
       ,(plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme=1\n"))

      ("gtk-4.0/settings.ini"
       ,(plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme=1\n"))

      ;; Doesn't work due to dconf-service in system config.scm
      ("dconf/user.txt"
       ,(plain-file "dark-theme" "[org/gnome/desktop/interface]
color-scheme='prefer-dark'\ngtk-theme='Adwaita:dark'\n"))

      ("glib-2.0/settings/keyfile"
       ,(plain-file "keyfile" "[org/gnome/desktop/interface]
color-scheme='prefer-dark'\n"))

      ;; Prevent wget from creating history file in home directory
      ("wget/wgetrc"
       ,(plain-file "wgetrc" "hsts-file=~/.cache/wget-hsts\n"))

      ("duc/ducrc"
       ,(plain-file "ducrc"
                    (string-join
                     '("[index]"
                       "one-file-system"
                       "check-hard-links"
                       "progress")
                     "\n"
                     'suffix)))

      ;; Play audio sound on notification
      ("mako/config"
       ,(mixed-text-file "mako-config"
                         "on-notify=exec "
                         alsa-utils "/bin/aplay"
                         " /home/pancake/documents/configs/notification.wav\n"
                         "ignore-timeout=1"))

      ;; Plain black lockscreen
      ("swaylock/config"
       ,(plain-file "swaylock-config" "color=000000FF\nscaling=solid_color\nindicator-idle-visible\n"))

      ;; Ledger
      ("ledger/ledgerrc"
       ,(mixed-text-file "ledgerrc"
                         "--exchange $
--date-format %Y-%m-%d
--pedantic\n"))

      ;; Move between chapters using '(' and ')'
      ("mpv/input.conf"
       ,(plain-file "mpv-input-config"
                    (string-join
                     '(") add chapter 1"
                       "( add chapter -1"
                       ;; Fix for when audio in only on one side
                       "h cycle-values audio-channels auto-safe mono")
                     "\n"
                     'suffix)))
      ;; ytdl-format copied from yt-dlp config
      ("mpv/mpv.conf"
       ,(plain-file "mpv-config" "hwdec=auto-safe
ytdl-format=bestvideo[height<=?1080]+bestaudio/best\n"))

      ;; Only download 1080p or lower.  Place in ~/downloads/videos with a
      ;; specific filename.  Grab English subtitles if we can.  Add
      ;; sponderblock metadata
      ("yt-dlp/config"
       ,(plain-file "yt-dlp-config"
                    "\
-f bestvideo[height<=?1080]+bestaudio/best
-o '|%(upload_date>%Y-%m-%d)+U|%(uploader)+U|%(title)+U|%(id)+U.%(ext)+U'
--paths '~/downloads/videos/'
--restrict-filenames
--concurrent-fragments 10
--write-sub
--sub-lang en
--sponsorblock-mark all\n"))))

   (simple-service
    'dotfiles
    home-files-service-type
    `((".guile" ,%default-dotguile)
      (".Xdefaults" ,%default-xdefaults)))


   %base-home-services)))
