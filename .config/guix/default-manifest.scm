(use-modules
 (guix cpu)
 ((guix transformations) #:select (options->transformation)))

;; Waiting for this to be accepted upstream: bug#58074
(use-modules (guix packages)
             (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages cyrus-sasl)
             (gnu packages autotools))
(define-public cyrus-sasl-xoauth2
  (package
    (name "cyrus-sasl-xoauth2")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1py9f1mn5k5xihrk0lfrwr6723c22gjb7lmgya83ibvislm2x3wl"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list (string-append "--with-cyrus-sasl="
                                                    #$output)
                                     "--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'bootstrap 'fix-autogen
                          (lambda _
                            ;; autogen.sh is executable but does not have a shebang.
                            (chmod "autogen.sh" #o400))))))
    (inputs (list cyrus-sasl))
    (native-inputs (list autoconf automake libtool))
    (native-search-paths
     (list (search-path-specification
            (variable "SASL_PATH")
            (files (list "lib/sasl2")))))
    (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
    (synopsis "XOAUTH2 plugin for Cyrus SASL")
    (description "Adds support for XOAUTH2 authentication to Cyrus SASL.  This
package can be used with isync to fetch mail from servers that support it.")
    (license license:expat)))

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
    "cyrus-sasl"
    ;; Added below
    ;; "cyrus-sasl-xoauth2"
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
   `(
     ;; Takes forever to build when tuned as it wants to build rust stuff
     ;; (tune . ,(cpu->gcc-architecture (current-cpu)))
     ;; Current release version fails to parse my ledger file
     (with-branch . "ledger=master")
     )))

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
    ))
 (list cyrus-sasl-xoauth2))
