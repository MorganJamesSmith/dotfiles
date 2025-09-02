(define-module (default-manifest))

(use-modules
 (guix profiles))

(when (current-filename)
  (add-to-load-path (dirname (current-filename))))
(use-modules (transformations))

(define audio
  '("alsa-utils" ; alsamixer
    "pulseaudio"
    "pulsemixer"
    "mpd-mpc"))   ; Music Playing Daemon CLI

(define downloaders
  '("curl"
    "transmission" ; Torrent Client
    "wget"
    "yt-dlp")) ; Downloads more than just YouTube

(define programming
  '("global"
    "git"
    "git:send-email"
    "make"
    "tup"))

(define email
  '("msmtp" ; sending emails
    "cyrus-sasl"
    "cyrus-sasl-xoauth2"
    "isync" ; receiving emails
    "icedove-minimal"
    "rss2email"))

(define terminal-tools
  '("file" ; filetype checker
    "ripgrep"
    "tree"))

(define compression-tools
  '("atool"
    "unzip"
    "zip"
    "zstd"))

(define-public default-manifest-packages
  (specifications->packages-with-transformations
   (append!
    audio
    downloaders
    programming
    email
    terminal-tools
    compression-tools
    '("aspell" ; spellchecker
      "aspell-dict-en"
      "brightnessctl"
      "dino" ; XMPP
      "gnupg"
      "ledger"
      "man-pages"
      "mpv" ; video player
      "openscad" ; 3D modeling program
      "openssh"
      "password-store"
      "pwgen" ; Password Generator
      "rmlint"
      "rsync"
      "xpdf" ; pdftotext
      ))))

(define-public default-manifest
  (packages->manifest default-manifest-packages))

default-manifest
