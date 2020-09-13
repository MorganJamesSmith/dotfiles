(define audio
  '("alsa-utils" ; alsamixer
    "pulsemixer"
    "mpd"       ; Music Playing Daemon
    "mpd-mpc")) ; Music Playing Daemon CLI

(define downloaders
  '("curl"
    "transmission" ; Torrent Client
    "unoconv" ; allows emacs to preview .doc files
    "wget"
    "youtube-dl")) ; Downloads more than just YouTube

(define emacs-packages
  (append!
   '("emacs-next"
     "pinentry-emacs"
     "xinit") ; emacs-exwm needs this
   (map
    (lambda (x) (string-append "emacs-" x))
    '("all-the-icons"
      "all-the-icons-dired"
      "auth-source-xoauth2"
      "bluetooth"
      "company"
      "company-quickhelp"
      "counsel"
      "debbugs"
      "diff-hl"
      "disk-usage"
      "djvu"
      "evil"
      "evil-collection"
      "evil-magit"
      "evil-org"
      "explain-pause-mode"
      "exwm"
      "flycheck"
      "flycheck-guile"
      "flycheck-ledger"
      "geiser"
      "guix"
      "helpful"
      "highlight-numbers"
      "htmlize"
      "ledger-mode"
      "literate-calc-mode"
      "magit"
      "modus-vivendi-theme"
      "nginx-mode"
      "nov-el"
      "org"
      "org-beautify-theme"
      "org-bullets"
      "org-drill"
      "org-edna"
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "transmission"
      "undo-tree"
      "use-package"
      "vterm"
      "which-key"
      "ws-butler"
      "yasnippet"
      "youtube-dl"))))

(define programming
  '("git"
    "git:send-email"
    "guile"
    "shellcheck"))

(define web-browsing
  '("icecat"            ; browser
    "nyxt"              ; browser
    "gst-libav"         ; video support in nyxt
    "gst-plugins-bad"   ; video support in nyxt
    "gst-plugins-base"  ; video support in nyxt
    "gst-plugins-good"  ; video support in nyxt
    "gst-plugins-ugly")); video support in nyxt

(specifications->manifest
 (append!
  audio
  downloaders
  emacs-packages
  programming
  web-browsing
  '("aspell" ; spellchecker
    "aspell-dict-en"
    "file" ; filetype checker
    "font-dejavu"
    "font-wqy-zenhei" ; Chinese, Japanese, and more
    "fontconfig" ; `fc-cache -rv' to update font cache
    "glibc-locales"
    "gnupg"
    "guix"
    "htop"
    "icedove" ; email client
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "password-store"
    "picom" ; compositor
    "pwgen" ; Password Generator
    "rsync"
    "sxiv"
    "syncthing"
    "unclutter"
    "unzip")))
