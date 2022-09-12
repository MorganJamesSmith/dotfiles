(use-modules
 ((gnu home services shells) #:select (home-bash-service-type
                                       home-shell-profile-service-type))
 ((gnu home services xdg) #:select (home-xdg-mime-applications-configuration
                                    home-xdg-mime-applications-service-type
                                    home-xdg-user-directories-configuration
                                    home-xdg-user-directories-service-type
                                    xdg-desktop-entry))
 ((gnu home services) #:select (home-environment-variables-service-type
                                home-files-service-type
                                home-run-on-first-login-service-type))
 (gnu home services shepherd)
 ((gnu packages fonts) #:select (font-openmoji font-wqy-zenhei))
 ((gnu packages glib) #:select (dbus))
 ((gnu packages linux) #:select (brightnessctl alsa-utils))
 ((gnu packages mpd) #:select (mpdris2))
 ((gnu packages wm) #:select (sway swayidle mako))
 (gnu packages xdisorg)
 (gnu packages qt)
 (gnu packages freedesktop)
 (gnu packages matrix)
 ((gnu services) #:select (service simple-service service-type service-extension))
 ((guix gexp) #:select (file-append gexp plain-file mixed-text-file)))

(define dbus-socket-location ".local/dbus")

(define (log-file-location name)
  #~(string-append
     (or (getenv "XDG_LOG_HOME")
         (format #f "~a/.local/var/log"
                 (getenv "HOME")))
     "/" #$name ".log"))

(define (mpdris2-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run mpdris2")
    (requirement '(home-dbus))
    (provision '(mpdris2))
    (start #~(make-forkexec-constructor
              (list #$(file-append mpdris2 "/bin/mpDris2"))
              #:log-file #$(log-file-location "mpdris2")))
    (stop  #~(make-kill-destructor)))))

(define (home-dbus-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run dbus")
    (provision '(home-dbus))
    (start #~(make-forkexec-constructor
              (list #$(file-append dbus "/bin/dbus-daemon")
                    "--session" (format #f "--address=unix:path=~a/~a"
                                        (getenv "HOME")
                                        #$dbus-socket-location))
              #:log-file #$(log-file-location "dbus")))
    (stop  #~(make-kill-destructor)))))

(define (home-pantalaimon-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run pantalaimon")
    (provision '(pantalaimon))
    (start #~(make-forkexec-constructor
              (list #$(file-append pantalaimon "/bin/pantalaimon"))
              #:log-file #$(log-file-location "pantalaimon")))
    (stop  #~(make-kill-destructor)))))

(define home-mpdris2-service-type
  (service-type
   (name 'home-mpdris2)
   (extensions (list (service-extension home-shepherd-service-type
                                        mpdris2-shepherd-service)))
   (default-value '())
   (description "mpdris2")))

(define home-dbus-service-type
  (service-type
   (name 'home-dbus)
   (extensions (list (service-extension home-shepherd-service-type
                                        home-dbus-shepherd-service)))
   (default-value '())
   (description "dbus")))

(define home-pantalaimon-service-type
  (service-type
   (name 'home-pantalaimon)
   (extensions (list (service-extension home-shepherd-service-type
                                        home-pantalaimon-shepherd-service)))
   (default-value '())
   (description "pantalaimon")))

(home-environment
 (packages (list
            sway
            swayidle
            bemenu ;; so dbus can use this
            qtwayland
            qt5ct
            xdg-desktop-portal
            xdg-desktop-portal-wlr
            xdg-desktop-portal-gtk
            mako
            font-openmoji ; emoji
            font-wqy-zenhei ; Asian
            ))
 (services
  (list
   (service home-shepherd-service-type)
   (service home-mpdris2-service-type)
   (service home-dbus-service-type)
   (service home-pantalaimon-service-type)
   (service home-bash-service-type)

   (simple-service 'stuff
                   home-shell-profile-service-type
                   (list (plain-file "profile" "
eval \"$(guix package --search-paths=suffix --profile=$HOME/.config/guix/extra-profiles/emacs/emacs)\"
eval \"$(guix package --search-paths=suffix --profile=$HOME/.config/guix/extra-profiles/default/default)\"

# Start graphical interface
if [ \"$(tty)\" = \"/dev/tty7\" ]; then
    chvt 7
    ssh-agent -a \"$(gpgconf --list-dirs agent-ssh-socket)\" sway
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
                     ("DBUS_SESSION_BUS_ADDRESS" . ,(string-append "unix:path=$HOME/" dbus-socket-location))
                     
                     ;; Wayland variables
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("QT_QPA_PLATFORM" . "wayland")
                     ("QT_QPA_PLATFORMTHEME" . "qt5ct")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("GDK_BACKEND" . "wayland")
                     ("SDL_VIDEODRIVER" . "wayland")

                     ("ASPELL_CONF" . "per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl")
                     ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
                     ("GNUPGHOME" . "$XDG_DATA_HOME/gnupg")
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

   
   (simple-service
    'dotfiles
    home-files-service-type
    `(

      (".config/fontconfig/conf.d/71-color-emoji.conf"
       ,(plain-file
         "71-color-emoji.conf"
         "<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
<fontconfig>
  <description>Color emoji using openmoji</description>
  <rejectfont>
    <pattern>
      <patelt name='family'>
        <string>OpenMoji</string>
      </patelt>
      <patelt name='style'>
        <string>Black</string>
      </patelt>
    </pattern>
  </rejectfont>
</fontconfig>"))

      ;; Dark theme
      (".config/gtk-3.0/settings.ini"
       ,(plain-file "settings.ini" "[Settings]
gtk-application-prefer-dark-theme=1\n"))

      ;; Prevent wget from creating history file in home directory
      (".config/wget/wgetrc"
       ,(plain-file "wgetrc" "hsts-file=~/.cache/wget-hsts\n"))

      ;; Play audio sound on notification
      (".config/mako/config"
       ,(mixed-text-file "mako-config"
                         "on-notify=exec "
                         alsa-utils "/bin/aplay"
                         " /home/pancake/documents/configs/notification.wav\n"))

      ;; Ensure gpg-agent is running and knows what terminal we are using.  This
      ;; is because ssh-agent will using pinentry through gpg-agent
      (".ssh/config"
       ,(plain-file "ssh-config"
                    "Match host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"\n"))

      ;; Enable ssh-agent support and allow use of emacs-pinentry
      (".local/share/gnupg/gpg-agent.conf"
       ,(plain-file "gpg-agent-config"
                    (string-join
                     '("enable-ssh-support"
                       "allow-emacs-pinentry"
                       "allow-loopback-pinentry"
                       "pinentry-program /home/pancake/.config/guix/extra-profiles/emacs/emacs/bin/pinentry-emacs")
                     "\n"
                     'suffix)))

      ;; Plain black lockscreen 
      (".config/swaylock/config"
       ,(plain-file "swaylock-config" "color=000000FF\nscaling=solid_color\n"))

      ;; Always create a graphical window even if there is no video.  This makes
      ;; is easy to close mpv
      (".config/mpv/mpv.conf"
       ,(plain-file "mpv-config" "force-window=yes\nhwdec=auto-safe"))

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
--replace-in-metadata title,uploader '\\|' '-'
--write-sub
--sub-lang en
--sponsorblock-mark all\n")))))))
