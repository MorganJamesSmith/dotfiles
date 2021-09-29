(use-modules (gnu home)
             (gnu home-services)
             (gnu home-services shells)
             (gnu home-services xdg)
             (gnu services)
             (gnu packages admin)
             (guix gexp))


(home-environment
 (services
  (list
   (service home-bash-service-type)

   (simple-service 'stuff
                   home-shell-profile-service-type
                   '("
eval \"$(guix package --search-paths=suffix --profile=$HOME/.config/guix/extra-profiles/default/default)\"
eval \"$(ssh-agent -s -a \"$(gpgconf --list-dirs agent-ssh-socket)\")\"
eval \"$(gpg-agent --homedir \"$GNUPGHOME\" --daemon -s)\"

# Start graphical interface
if [ \"$(tty)\" = \"/dev/tty7\" ]; then
    sx
fi
"))

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
       '((x-scheme-handler/magnet  . transmission.desktop)
         (application/x-bittorrent . transmission.desktop)
         (application/pdf          . emacs.desktop)
         (application/postscript   . emacs.desktop)
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
        (config '((exec . "emacsclient -a emacs %u"))))))))


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
                          ("PASSWORD_STORE_DIR" . "")
                          ("GNUPGHOME" . "")
                          ("XDG_CONFIG_HOME" . "readline")
                          ("XDG_CONFIG_HOME" . "aspell")
                          ("XDG_CONFIG_HOME" . "wget")
                          ("CARGO_HOME" . "")
                          ("XDG_CONFIG_HOME" . "java")
                          ("PYLINTHOME" . "")
                          ("XDG_CONFIG_HOME" . "simplescreenrecorder")
                          ("TEXMFHOME" . "")
                          ("TEXMFVAR" . "")
                          ("TEXMFCONFIG" . "")))))

   (simple-service 'some-useful-env-vars-service
          		   home-environment-variables-service-type
          		   `(("LESSKEY" . "$XDG_CONFIG_HOME/less/lesskey")
                     ("LESSHISTFILE" . "$XDG_CACHE_HOME/less/history")
                     ("PASSWORD_STORE_DIR" . "$XDG_DATA_HOME/password-store")
                     ("GPG_TTY" . "$(tty)")
                     ("GNUPGHOME" . "$XDG_DATA_HOME/gnupg")
                     ("INPUTRC" . "$XDG_CONFIG_HOME/readline/inputrc")
                     ("ASPELL_CONF" . "\"per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl\"")
                     ("WGETRC" . "$XDG_CONFIG_HOME/wget/wgetrc")
                     ("CARGO_HOME" . "$XDG_DATA_HOME/cargo")
                     ("_JAVA_OPTIONS" . "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java")
                     ("PYLINTHOME" . "$XDG_DATA_HOME/pylint")
                     ("TEXMFHOME" . "$XDG_DATA_HOME/texmf")
                     ("TEXMFVAR" . "$XDG_CACHE_HOME/texlive/texmf-var")
                     ("TEXMFCONFIG" . "$XDG_CONFIG_HOME/texlive/texmf-config")

                     ("EDITOR" . "emacsclient")
                     ("PATH" . "$HOME/.local/bin:$PATH")
                     ("HISTFILESIZE" . "100000")
                     ("HISTSIZE" . "100000")
                     ("HISTTIMEFORMAT" . "\"[%F %T] \"")
                     ("HISTFILE" . "$XDG_DATA_HOME/shell-history"))))))
