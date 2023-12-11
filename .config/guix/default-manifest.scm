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
    "rss2email"))

(define terminal-tools
  '("file" ; filetype checker
    "tree"
    "htop"))

(define transformations
  (options->transformation
   '()))

(define (specifications->manifest-with-transformations packages)
  (packages->manifest
   (map
    (compose
     (lambda (package output)
       (list (transformations package) output))
     specification->package+output)
    packages)))

(specifications->manifest-with-transformations
 (append!
  audio
  downloaders
  programming
  email
  terminal-tools
  '("aspell" ; spellchecker
    "aspell-dict-en"
    "atool" ; compression stuff
    "brightnessctl"
    "dino" ; XMPP
    "gnupg"
    "hledger"
    "jami" ; SIP
    "man-pages"
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "password-store"
    "pwgen" ; Password Generator
    "quaternion" ; Matrix
    "rmlint"
    "recutils"
    "unzip"
    "xpdf" ; pdftotext
    "zip")))
