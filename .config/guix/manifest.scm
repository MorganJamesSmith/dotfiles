(use-modules
 ((guix transformations) #:select (options->transformation)))

(define transformations
  (options->transformation
   '((with-branch  . "emacs-modus-themes=main")
     (with-branch  . "emacs-next=master")
     (with-git-url  . "emacs-next=/home/pancake/src/emacs/emacs"))))

(define (specifications->manifest-with-transformations packages)
  (packages->manifest
   (map
    (compose
     (lambda (package output)
       (list (transformations package) output))
     specification->package+output)
    packages)))

(define audio
  '("alsa-utils" ; alsamixer
    "pulsemixer"
    "mpd-mpc")) ; Music Playing Daemon CLI

(define downloaders
  '("curl"
    "transmission" ; Torrent Client
    "wget"
    "youtube-dl")) ; Downloads more than just YouTube

(define emacs-packages
  (append!
   '("emacs-next"
     "pinentry-emacs"
     "ghostscript" ; allows Emacs to preview PostScript
     "unoconv")    ; allows Emacs to preview docx files
   (map
    (lambda (x) (string-append "emacs-" x))
    '("auth-source-xoauth2"
      "bluetooth"
      "company"
      "company-quickhelp"
      "counsel"
      "debbugs"
      "diff-hl"
      "disk-usage"
      "djvu"
      "elpher"
      "crdt"
      "eshell-syntax-highlighting"
      "evil"
      "evil-collection"
      "evil-goggles"
      "evil-org"
      "exwm"
      "flycheck"
      "flycheck-guile"
      "flycheck-ledger"
      "geiser"
      "guix"
      "helpful"
      "scad-mode"
      "highlight-numbers"
      "htmlize"
      "ledger-mode"
      "literate-calc-mode"
      "magit"
      "modus-themes"
      "nginx-mode"
      "nov-el"
      "org"
      "org-contrib"
      "org-drill"
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "transmission"
      "typit"
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
  '("icecat"
    "ungoogled-chromium"))

(specifications->manifest-with-transformations
 (append!
  audio
  downloaders
  emacs-packages
  programming
  web-browsing
  '("aspell" ; spellchecker
    "aspell-dict-en"
    "ffmpeg"
    "file" ; filetype checker
    "flameshot" ; Screenshots
    "font-dejavu"
    "font-wqy-zenhei" ; Chinese, Japanese, and more
    "fontconfig" ; `fc-cache -rv' to update font cache
    "gnupg"
    "htop"
    "icedove" ; email client
    "libreoffice"
    "lm-sensors"
    "lxqt-policykit" ; used in xinitrc
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "pass-git-helper"
    "password-store"
    "picom" ; compositor
    "pinentry"
    "pwgen" ; Password Generator
    "rsync"
    "sxiv"
    "syncthing"
    "texlive"
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
