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
  '("git"
    "git:send-email"
    "guile"))

(define web-browsing
  '("firefox"
    "qutebrowser"))

(define desktop-environment
  '("sway"
    "swayidle"
    "bemenu"
    "qtwayland"
    "qt5ct"))

(define stuff-only-needed-for-their-environment-variables
  '("man-db"     ;; MANPATH
    "texinfo"))  ;; INFOPATH

(specifications->manifest
 (append!
  audio
  downloaders
  programming
  web-browsing
  desktop-environment
  stuff-only-needed-for-their-environment-variables
  '("aspell" ; spellchecker
    "aspell-dict-en"
    "brightnessctl"
    "ffmpeg"
    "file" ; filetype checker
    "flameshot" ; Screenshots
    "flatpak"
    "global"
    "gnupg"
    "htop"
    "icedove" ; email client
    "libreoffice"
    "lxqt-policykit" ; used in xinitrc
    "make"
    "man-pages"
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "password-store"
    "picom" ; compositor
    "pinentry"
    "pulseaudio"
    "pwgen" ; Password Generator
    "rmlint"
    "rsync"
    "sx" ; Start X
    "sxiv"
    "tree"
    "unclutter"
    "unzip"
    "xpdf" ; pdftotext
    "xrandr"
    "xhost"    ; used in xinitrc
    "xset"     ; used in xinitrc
    "xsetroot" ; used in xinitrc
    "xss-lock" ; auto lock screen (run in xinitrc)
    "zip")))
