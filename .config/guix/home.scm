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
 ((gnu packages glib) #:select (dbus))
 ((gnu packages gnupg) #:select (gnupg))
 ((gnu packages linux) #:select (brightnessctl pipewire-0.3))
 ((gnu packages mpd) #:select (mpdris2))
 ((gnu packages music) #:select (playerctl))
 ((gnu packages ssh) #:select (openssh))
 ((gnu services) #:select (service simple-service))
 ((guix gexp) #:select (file-append gexp plain-file))
 )

(home-environment
 (services
  (list
   (service home-bash-service-type)

   (simple-service 'stuff
                   home-shell-profile-service-type
                   (list (plain-file "profile" "
eval \"$(guix package --search-paths=suffix --profile=$HOME/.config/guix/extra-profiles/emacs/emacs)\"
eval \"$(guix package --search-paths=suffix --profile=$HOME/.config/guix/extra-profiles/default/default)\"

[ -f /tmp/daemon-env-vars ] && . /tmp/daemon-env-vars

# Start graphical interface
if [ \"$(tty)\" = \"/dev/tty7\" ]; then
    chvt 7
    sx
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
         (x-scheme-handler/mailto  . emacsmail.desktop)
         (application/pdf          . emacs.desktop)
         (application/postscript   . emacs.desktop)
         (application/x-csv        . emacs.desktop)
         (image/gif                . emacs.desktop)
         (image/jpeg               . emacs.desktop)
         (image/png                . emacs.desktop)
         (text/plain               . emacs.desktop)
         (text/x-shellscript       . emacs.desktop)
         (inode/directory          . emacs.desktop)))
     (desktop-entries
      (list
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
        (file "emacsmail")
        (name "Emacs Mail")
        (type 'application)
        (config '((exec . "emacsclient -a emacs --eval \"(browse-url-mail \\\"%u\\\")\""))))))))

   (simple-service
    'start-daemons
    home-run-on-first-login-service-type
    #~(begin
        (system
         (string-join
          (append
           (map
            (lambda (command)
              ;; Put the environment variables in a file that can be sourced in
              ;; our profile and also set them now so the other daemons have
              ;; access to them
              (string-append "eval $( " command " | tee -a /tmp/daemon-env-vars )"))
            (list
             (string-append #$(file-append dbus "/bin/dbus-launch") " --sh-syntax")
             (string-append #$(file-append openssh "/bin/ssh-agent") " -s -a \"$(gpgconf --list-dirs agent-ssh-socket)\"")
             (string-append #$(file-append gnupg "/bin/gpg-agent") " --homedir \"$GNUPGHOME\" --daemon -s")))
           (map
            (lambda (command)
              (string-append command " &"))
            (list
             (string-append #$(file-append pipewire-0.3 "/bin/pipewire"))
             (string-append #$(file-append pipewire-0.3 "/bin/pipewire-media-session"))
             (string-append #$(file-append pipewire-0.3 "/bin/pipewire-pulse"))
             (string-append #$(file-append mpdris2 "/bin/mpDris2")))))
          "\n"))))

   (simple-service 'mkdirs
                   home-run-on-first-login-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (map
                        (compose
                         mkdir-p
                         (lambda (dir)
                           (string-append (getenv (car dir)) "/" (cdr dir))))
                        '(("HOME" . ".config/mpd")
                          ("XDG_CONFIG_HOME" . "aspell")
                          ("CARGO_HOME" . "")
                          ("GNUPGHOME" . "")
                          ("XDG_CONFIG_HOME" . "readline")
                          ("PASSWORD_STORE_DIR" . "")
                          ("TEXMFCONFIG" . "")
                          ("TEXMFHOME" . "")
                          ("TEXMFVAR" . "")
                          ("XDG_CONFIG_HOME" . "wget")
                          ("XDG_CONFIG_HOME" . "java")
                          ("XDG_CONFIG_HOME" . "simplescreenrecorder")))))

   (simple-service 'some-useful-env-vars-service
          		   home-environment-variables-service-type
          		   `(
                     ("ASPELL_CONF" . "\"per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl\"")
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

                     ("GPG_TTY" . "$(tty)")
                     ("EDITOR" . "emacsclient")
                     ("PATH" . "$HOME/.local/bin:$PATH")
                     ("HISTFILESIZE" . "100000")
                     ("HISTSIZE" . "100000")
                     ("HISTTIMEFORMAT" . "\"[%F %T] \"")))

   ;; Prevent wget from creating history file in home directory
   (simple-service 'wgetrc
                   home-files-service-type
                   (list `("config/wget/wgetrc"
                                ,(plain-file "wgetrc"
                                             "hsts-file=~/.cache/wget-hsts\n"))))

   (simple-service 'git-pass
                   home-files-service-type
                   (list `("config/pass-git-helper/git-pass-mapping.ini"
                                ,(plain-file "git-pass" "[*outlook.com*]\ntarget=email/morganjsmith\n")))))))
