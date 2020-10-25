(use-modules
 ((guix scripts build) #:select (options->transformation)))

(define transformations
  (options->transformation
   '((with-branch  . "emacs-evil=master")
     (with-branch  . "emacs-evil-collection=master")
     (with-git-url . "emacs-evil-collection=https://github.com/MorganJamesSmith/evil-collection")
     (with-branch  . "emacs-modus-vivendi-theme=master")
     (with-git-url . "emacs-modus-vivendi-theme=https://gitlab.com/protesilaos/modus-themes")
     (with-branch  . "emacs-use-package=master")
     (with-branch  . "emacs-company=master")
     (with-git-url . "emacs-company=https://github.com/company-mode/company-mode"))))

(define (manifest-with-transformations packages)
  (packages->manifest
   (map
    (compose
     (lambda (package+output)
       (transformations #f (car package+output)))
     list
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
     "ghostscript" ; allow Emacs to preview PostScript
     "unoconv"     ; allows Emacs to preview docx files
     "xinit")      ; emacs-exwm needs this
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
      "eshell-syntax-highlighting"
      "evil"
      "evil-collection"
      "evil-magit"
      "evil-org"
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
      "org-drill"
      "org-edna"
      "org-superstar"
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "toc-org"
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
  '("icecat"
    "ungoogled-chromium"))

(manifest-with-transformations
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
    "guix"
    "htop"
    "icedove" ; email client
    "libreoffice"
    "lm-sensors"
    "mpv" ; video player
    "openscad" ; 3D modeling program
    "openssh"
    "password-store"
    "picom" ; compositor
    "pinentry"
    "pwgen" ; Password Generator
    "rsync"
    "sxiv"
    "syncthing"
    "texlive"
    "unclutter"
    "unzip"
    "zip"
    "xrandr")))
