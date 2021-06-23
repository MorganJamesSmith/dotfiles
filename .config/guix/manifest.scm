(use-modules
 ((ice-9 popen) #:select (open-pipe* close-pipe))
 ((ice-9 rdelim) #:select (read-line))
 ((guix transformations) #:select (options->transformation))
 ((guix build utils) #:select (with-directory-excursion)))

(use-modules
 (flat packages emacs))


(define (emacs-git-commit)
  (let* ((pipe (with-directory-excursion "/home/pancake/src/emacs/emacs"
                 (open-pipe* OPEN_READ "git" "rev-parse" "HEAD")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define transformations
  (options->transformation
   `((with-branch  . "emacs-modus-themes=main")

     (with-branch  . "emacs-pdf-tools=master")
     (with-git-url . "emacs-pdf-tools=https://github.com/vedang/pdf-tools")

     (with-branch  . "emacs-ledger-mode=master")

     (with-branch  . "emacs-dash=master")

     (with-branch  . "emacs-exwm=master")
     (with-git-url . "emacs-exwm=https://github.com/ch11ng/exwm")
     (with-branch  . "emacs-xelb=master")
     (with-git-url . "emacs-xelb=https://github.com/ch11ng/xelb")

     (with-commit  . ,(string-append "emacs-native-comp=" (emacs-git-commit)))
     (with-git-url . "emacs-native-comp=/home/pancake/src/emacs/emacs")
     (with-input   . "emacs=emacs-native-comp")
     (with-input   . "emacs-minimal=emacs-native-comp")
     (with-input   . "emacs-no-x=emacs-native-comp")
     (with-input   . "emacs-no-x-toolkit=emacs-native-comp")

     (without-tests . "emacs-yasnippet") ;; Fixed in elpa
     (without-tests  . "emacs-buttercup")
     (without-tests  . "emacs-libgit")
     (without-tests  . "emacs-use-package"))))

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
   '("emacs-native-comp"
     "pinentry-emacs"
     "ghostscript" ; allows Emacs to preview PostScript
     "unoconv")    ; allows Emacs to preview docx files
   (map
    (lambda (x) (string-append "emacs-" x))
    '("auth-source-xoauth2"
      "company"
      "company-quickhelp"
      "crdt"
      "debbugs"
      "diff-hl"
      "disk-usage"
      "djvu"
      "elpher"
      "eshell-syntax-highlighting"
      "exwm"
      "flycheck"
      "flycheck-guile"
      "flycheck-ledger"
      "flymake-shellcheck"
      "geiser"
      "guix"
      "highlight-numbers"
      "htmlize"
      "ledger-mode"
      "literate-calc-mode"
      "magit"
      "modus-themes"
      "nov-el"
      "org"
      "org-contrib"
      "pdf-tools"
      "pinentry"
      "plantuml-mode"
      "rainbow-delimiters"
      "scad-mode"
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
    "nyxt"
    "qutebrowser"
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
    "sx" ; Start X
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
