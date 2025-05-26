(use-modules
 ((guix transformations) #:select (options->transformation)))

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
    "git:send-email"))

(define email
  '("msmtp" ; sending emails
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

(define transformations
  (options->transformation
   '()))

(define* (specifications->manifest-with-transformations specifications #:optional (packages '()))
  (packages->manifest
   (map
    (lambda* (package #:optional (output "out"))
      (list (transformations package) output))
    (append!
     (map
      specification->package+output
      specifications)
     packages))))

(specifications->manifest-with-transformations
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
    )))
