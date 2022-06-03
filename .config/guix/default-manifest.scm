(define audio
  '("alsa-utils" ; alsamixer
    "pulsemixer"
    "mpd-mpc"     ; Music Playing Daemon CLI
    "playerctl")) ; Lets me play and pause using dbus

(define downloaders
  '("curl"
    "transmission" ; Torrent Client
    "wget"
    "yt-dlp")) ; Downloads more than just YouTube

(define programming
  '("plantuml"
    "global"
    "git"
    "git:send-email"))

(define email
  '("msmtp" ; sending emails
    "isync" ; receiving emails
    "rss2email"))

(define terminal-tools
  '("file" ; filetype checker
    "tree"
    "htop"))

(define stuff-only-needed-for-their-environment-variables
  '("man-db"     ;; MANPATH
    "texinfo"))  ;; INFOPATH

(specifications->manifest
 (append!
  audio
  downloaders
  programming
  email
  terminal-tools
  stuff-only-needed-for-their-environment-variables
  '("aspell" ; spellchecker
    "aspell-dict-en"
    "brightnessctl"
    "flameshot" ; Screenshots
    "flatpak"
    "gnupg"
    "libreoffice"
    "man-pages"
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "password-store"
    "pwgen" ; Password Generator
    "rmlint"
    "recutils"
    "unzip"
    "xpdf" ; pdftotext
    "zip")))
