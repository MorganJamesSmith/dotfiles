(define-module (default-manifest))

(use-modules (gnu packages)
             (guix profiles))

(define audio
  '("alsa-utils" ; alsamixer
    "pulseaudio"
    "pulsemixer"))

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

(define web-browsers
  '("librewolf"
    "ungoogled-chromium-wayland"))

(define-public default-manifest-packages
  (specifications->packages
   (append!
    audio
    downloaders
    programming
    email
    terminal-tools
    compression-tools
    web-browsers
    '("aspell" ; spellchecker
      "aspell-dict-en"
      "brightnessctl"
      "dino" ; XMPP
      "gnupg"
      "ledger"
      "hledger"
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
